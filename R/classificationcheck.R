#'Combine species-level and plot-level indicator information into one data.frame used to automate classifications
#'
#'Functions starting with tall table outputs from gather functions to calculate metrics used to classify sites, then perform that classification using a combination of indicator data.
#'
#'@param header Data frame. Use the data frame from the \code{header_build_lentic()} output.
#'@param spp_inventory_tall Data frame. Use the data frame from the \code{gather_spp_inventory_lentic()} output.
#'@param lpi_tall Data frame. Use the data frame from the \code{gather_lpi_lentic()} output.
#'@param height_tall Data frame. Use the data frame from the \code{gather_heights_lentic()} output.
#'@param woody_tall Data frame. Use the data frame from the \code{gather_woodyspecies()} output.
#'@param annualuse_tall Data frame. Use the data frame from the \code{gather_annualuse()} output.
#'@param hummocks Data frame. Use the data frame from the \code{gather_hummocks()} output.
#'@param gap_tall optional data frame. Use the data frame from the \code{gather_gap()} output.
#'@param soil_stability_tall Optional data frame. Use the data frame from the \code{gather_soilstab()} output.
#'@param waterqualdet Optional data frame. Use the data frame from the file geodatabase. Must have EvaluationID as the identifying column.
#'@param unknowncodes Optional data frame. Use the data frame from the \code{gather_unknowns_lentic()} output. Unknown species list matching unknown codes to their duration and Growth habit. This is used to fill in duration and growth habit for plants in LPI never identified to a species or genus with those fields specified. If argument is unused, all unknown species without duration or Growth Habit specified will be filtered out before being passed on to \code{pct_cover_lentic()}.
#'@param masterspecieslist Data frame. The centrally managed master species list should be used.
#'#'@param unit String. The sampling unit by which data should be summarized. Should be `by_plot`, `by_line` or `by_geosurface` (for data from Lotic-Integration Plots). Defaults to `by_plot`.
#'@returns Data frame of indicator data calculated by EvaluationID.
#'

#'@export classificationcheck
#'@rdname classificationcheck
classificationcheck <- function(header,
                                lpi_tall,
                                height_tall,
                                woody_tall,
                                masterspecieslist,
                                unknowncodes,
                                soils,
                                PlotChar){

  #Pull relevant PlotChar fields
  PlotChar <- PlotChar%>%
    sf::st_drop_geometry()%>%
    select(PlotID, EvaluationID, AdminState, AvgWidthArea, CowardinConfidence, HGMClassConfidence, WetlandTypeConfidence, WetlandTypeOther, ClassificationComments)

  #calculate the dominance test. maintain only the result of that test.
  dominance <- dominance_test(header%>%sf::st_drop_geometry(), lpi_tall, masterspecieslist, bystrata = F)

  Foliar <- pct_FoliarCover(lpi_tall, unit = "by_plot")

  BareSoilCover <- pct_NonPlantGroundCover(lpi_tall, hit = "first", masterspecieslist, unit = "by_plot")%>%
    dplyr::mutate(BareSoilCoverAll = rowSums(dplyr::select(., dplyr::any_of(c("FH_SoilCover","FH_OrganicMaterialCover")))))%>%
    dplyr::select(PlotID, EvaluationID, BareSoilCoverAll, FH_SaltCrustCover)

  UnkCover <- pct_UnknownCover(lpi_tall = lpi_tall, masterspecieslist = masterspecieslist, covertype = "relative", unit = "by_plot")

  FuncGroupCover <- pct_FunctionalGroupCover(lpi_tall = lpi_tall, masterspecieslist = masterspecieslist, covertype = "absolute")

  GrowthHabitCover <- pct_GrowthHabitCover(lpi_tall = lpi_tall, masterspecieslist = masterspecieslist,covertype = "absolute", unknowncodes = unknowncodes, unit = "by_plot")

  TypeCover <- pct_TypeCover(lpi_tall = lpi_tall, masterspecieslist = masterspecieslist,covertype = "absolute", unknowncodes = unknowncodes, unit = "by_plot")

  #Store whether A1 Histosol was observed in any soil pit.
  soilscheck <- soils%>%
    dplyr::mutate(HistosolCheck = ifelse(HydricIndicatorPrimary %in% c("A1_Histosol") |
                                           HydricIndicatorSecondary %in% c("A1_Histosol"),
                                         "Histosol", "Mineral"))%>%
    dplyr::group_by(PlotID, EvaluationID)%>%
    dplyr::summarize(HistosolCheck = ifelse(any(HistosolCheck=="Histosol"), "Histosol", "Mineral"))

  heightmetrics <- height_metrics(height_tall, masterspecieslist, unknowncodes, method = "mean")

  #Calculate the cover of tall woodies in woody structure form

  # Calculate the number of quadrats for all lines with no woody species present.
  # Distinct columns are the same here independent of the level of reporting
  quadcalc <-
    woody_tall%>%
    dplyr::filter(WoodySpeciesPresent=="No")%>%
    dplyr::distinct(PlotID, EvaluationID, LineKey, LineLengthCM, interval)%>%
    dplyr::mutate(n = round(LineLengthCM/interval, digits = 0))

  # Calculate the number of quadrats for all lines with woody species present.
  # This will use the points in Annual Use Points Repeat to count by line.
  # Grouping for this table should either be plot level or transect level
  quadcount <-
    woody_tall%>%
    dplyr::filter(WoodySpeciesPresent=="Yes")%>%
    dplyr::distinct(PlotID, EvaluationID, LineKey, PointNbr)%>%
    dplyr::group_by(PlotID, EvaluationID, LineKey)%>%
    dplyr::count()%>%
    dplyr::bind_rows(., quadcalc)%>%
    dplyr::group_by(across(c("PlotID", "EvaluationID")))%>%
    dplyr::summarize(nquads = sum(n))

  TallWoodyCover <- woody_tall%>%
    dplyr::filter(OverhangingOrRooted %in% c("Rooted-in", "Overhanging"), HeightClass %in% c("Height Class 4",
                                                                                             "Height Class 5",
                                                                                             "Height Class 6"))%>%
    dplyr::distinct(PlotID, EvaluationID, LineKey, PointNbr)%>%
    dplyr::group_by(PlotID, EvaluationID)%>%
    dplyr::summarize(HeightClass456 = dplyr::n_distinct(LineKey, PointNbr))%>%
    dplyr::left_join(quadcount,
                     .,
                     by = c("PlotID", "EvaluationID"))%>%
    dplyr::mutate(WS_HgtClass456_PctQdrts = ifelse(is.na(HeightClass456), 0, round(HeightClass456/nquads * 100, digits = 2)))

  #join all the data together into one table
  classinddata <- header%>%
    dplyr::left_join(., dominance, by = join_by(PlotID, EvaluationID))%>%
    dplyr::left_join(., Foliar, by = join_by(PlotID, EvaluationID))%>%
    dplyr::left_join(., BareSoilCover, by = join_by(PlotID, EvaluationID))%>%
    dplyr::left_join(., UnkCover, by = join_by(PlotID, EvaluationID))%>%
    dplyr::left_join(., FuncGroupCover, by = join_by(PlotID, EvaluationID))%>%
    dplyr::left_join(., GrowthHabitCover, by = join_by(PlotID, EvaluationID))%>%
    dplyr::left_join(., TypeCover, by = join_by(PlotID, EvaluationID))%>%
    dplyr::left_join(., heightmetrics, by = join_by(PlotID, EvaluationID))%>%
    dplyr::left_join(., soilscheck, by = join_by(PlotID, EvaluationID))%>%
    dplyr::left_join(., TallWoodyCover, by = join_by(PlotID, EvaluationID))%>%
    dplyr::left_join(., PlotChar)

  #Calculate an anticipated wetland type
  classificationcheck <- classinddata %>%
    dplyr::mutate(SuggestedWetlandType1 = dplyr::case_when(
      DominanceTest == "N"~"Non-Wetland Site",
      (BareSoilCoverAll + FH_SaltCrustCover) > 25 & AH_PlayaSpeciesCover > 10~"Playa",
      HistosolCheck == "Histosol" ~"Fen or Bog",
      AH_TreeCover >= 30~"Riparian Forest or Woodland",
      AH_WoodyCover >= 30 ~"Riparian Shrubland or Shrub Wetland",
      AH_MarshSpeciesCover >=20 & BareSoilCoverAll < 50 ~ "Marsh",
      AvgWidthArea < 60 ~ "Spring, Seep, or Vegetated Drainageway",
      AH_GraminoidCover>50 & AH_MarshSpeciesCover < 20~"Wet or Mesic Meadow",
      TRUE~"other"
    ),
    SuggestedWetlandType2 = dplyr::case_when(
      DominanceTest == "N"~"Non-Wetland Site",
      HistosolCheck == "Histosol" ~"Fen or Bog",
      AvgWidthArea < 60 ~ "Spring, Seep, or Vegetated Drainageway",
      AH_TreeCover >= 30~"Riparian Forest or Woodland",
      AH_WoodyCover >= 30 ~"Riparian Shrubland or Shrub Wetland",
      AH_MarshSpeciesCover >=20 & BareSoilCoverAll < 50 ~ "Marsh",
      AH_GraminoidCover>50 & AH_MarshSpeciesCover < 20~"Wet or Mesic Meadow",
      (BareSoilCoverAll + FH_SaltCrustCover) > 25 & AH_PlayaSpeciesCover > 10~"Playa",
      TRUE~"other"
    ))%>%
    dplyr::select(PlotID, EvaluationID, SiteName, AdminState, SamplingApproach, LatitudeWGS84, LongitudeWGS84, CowardinAttribute, HGMClass, WetlandType, SuggestedWetlandType1, SuggestedWetlandType2, AvgWidthArea, DominanceTest, BareSoilCoverAll, FH_SaltCrustCover, TotalFoliarCover, AH_GraminoidCover, AH_ShrubCover, AH_TreeCover, AH_WoodyCover, AH_PlayaSpeciesCover, AH_MarshSpeciesCover, Hgt_Woody_Avg, HistosolCheck, WS_HgtClass456_PctQdrts)

  return(classificationcheck)
}
