#'Combine species-level and plot-level indicator information into one data.frame.
#'
#'Functions starting with tall table outputs from gather functions to calculate all metrics. \code{CombineRelativeCoverMetrics()} and \code{CombineRelativeCoverMetrics()} focus on cover metrics. Together with \code{Community_Metrics()}, these functions feed into final wrapping functions to create two indicator tables ready to be incorporated into the final data set, Indicators and Species Indicators. These data tables are created with \code{allmetrics_byplot()} and \code{allmetrics_byspecies()}.
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


#'@export CombineRelativeCoverMetrics
#'@rdname allmetrics
CombineRelativeCoverMetrics <- function(header, lpi_tall, masterspecieslist, unknowncodes, unit = "by_plot"){

  TotalAbsolute <- pct_TotalAbsoluteCover(lpi_tall, unit = unit)

  RelativeNative <- pct_NativeCover(lpi_tall, masterspecieslist, covertype = "relative", unit = unit)

  RelativeNoxious <- pct_NoxiousCover(header, lpi_tall, masterspecieslist, covertype = "relative", unit = unit)

  RelativeHydro <- pct_HydrophyteCover(header, lpi_tall, masterspecieslist, covertype = "relative", unit = unit)

  RelativeHydroFAC <- pct_HydroFACCover(header, lpi_tall, masterspecieslist, covertype = "relative", unit = unit)

  RelativeGrowthHabit <- pct_GrowthHabitCover(lpi_tall, masterspecieslist, covertype = "relative", unknowncodes, unit = unit)

  RelativeDuration <- pct_DurationCover(lpi_tall, masterspecieslist, covertype = "relative", unknowncodes, unit = unit)

  LPI_Cover_Indicators <- TotalAbsolute %>% dplyr::right_join(header%>%dplyr::select(PlotID,
                                                                                     EvaluationID,
                                                                                     SiteName,
                                                                                     AdminState,
                                                                                     SpeciesState,
                                                                                     FieldEvalDate),
                                                              .,
                                                              by =  c("PlotID", "EvaluationID"))%>%
    dplyr::left_join(., RelativeNative, by = c("PlotID", "EvaluationID"))%>%
    dplyr::left_join(., RelativeNoxious, by =  c("PlotID", "EvaluationID"))%>%
    dplyr::left_join(., RelativeHydro, by =  c("PlotID", "EvaluationID"))%>%
    dplyr::left_join(., RelativeHydroFAC, by =  c("PlotID", "EvaluationID"))%>%
    dplyr::left_join(., RelativeGrowthHabit, by =  c("PlotID", "EvaluationID"))%>%
    dplyr::left_join(., RelativeDuration, by =  c("PlotID", "EvaluationID"))

  return(LPI_Cover_Indicators)
}

#'@export CombineAbsoluteCoverMetrics
#'@rdname allmetrics
CombineAbsoluteCoverMetrics <- function(header, lpi_tall, masterspecieslist, unknowncodes, unit = "by_plot"){

  if(!(unit %in% c("by_plot", "by_line", "by_geosurface"))){
    stop("Can only summarize using a sampling unit of `by_plot`, `by_line`, or `by_geosurface` (for L-R plots only). Update unit to one of these strings. ")
  } else if (unit == "by_line") {
    level <- rlang::quos(PlotID, EvaluationID, LineKey)
    level_colnames <- c("PlotID", "EvaluationID", "LineKey")
  } else if(unit == "by_geosurface") {
    level <- rlang::quos(PlotID, EvaluationID, GeoSurface)
    level_colnames <- c("PlotID", "EvaluationID", "GeoSurface")
  } else {
    level <- rlang::quos(PlotID, EvaluationID)
    level_colnames <- c("PlotID", "EvaluationID")
  }

  Foliar <- pct_FoliarCover(lpi_tall, unit = unit)

  Basal <- pct_BasalCover(lpi_tall, unit = unit)

  AbsoluteNative <- pct_NativeCover(lpi_tall, masterspecieslist, covertype = "absolute", unit = unit)

  AbsoluteNoxious <- pct_NoxiousCover(header, lpi_tall, masterspecieslist, covertype = "absolute", unit = unit)

  AbsoluteHydro <- pct_HydrophyteCover(header, lpi_tall, masterspecieslist, covertype = "absolute", unit = unit)

  AbsoluteHydroFAC <- pct_HydroFACCover(header, lpi_tall, masterspecieslist, covertype = "absolute", unit = unit)

  AbsoluteGrowthHabit <- pct_GrowthHabitCover(lpi_tall, masterspecieslist, covertype = "absolute", unknowncodes, unit = unit)%>%
    dplyr::select(!!!level,
                  dplyr::any_of(c("AH_ForbCover",
                                  "AH_GraminoidCover",
                                  "AH_ShrubCover",
                                  "AH_TreeCover")))

  AbsoluteDuration <- pct_DurationCover(lpi_tall, masterspecieslist, covertype = "absolute", unknowncodes, unit = unit)

  AbsoluteDurationGrowth <- pct_DurationGrowthHabitCover(lpi_tall, masterspecieslist, covertype = "absolute", unknowncodes, unit = unit)%>%
    dplyr::select(!!!level,
                  dplyr::any_of(c("AH_AnnualGraminoidCover")))

  AbsoluteNativeGrowth <- pct_NativeGrowthHabitCover(lpi_tall, masterspecieslist, covertype = "absolute", unknowncodes, unit = unit)%>%
    dplyr::select(!!!level,
                  dplyr::any_of(c("AH_NativeGraminoidCover", "AH_NativeShrubCover", "AH_NonnativeShrubCover")))

  AbsoluteTypeDuration <- pct_DurationTypeCover(lpi_tall, masterspecieslist, covertype = "absolute", unknowncodes, unit = unit)%>%
    dplyr::select(!!!level,
                  dplyr::any_of(c("AH_NonwoodyPerennialCover")))

  if(any(grepl("SG_Group", colnames(masterspecieslist)))){
    AbsoluteSGGroup <- pct_SGGroupCover(lpi_tall, masterspecieslist, covertype = "absolute", unit = unit)%>%
      dplyr::select(!!!level,
                    dplyr::any_of(c("AH_PreferredForbCover" = "AH_SGPreferredForbCover", "AH_SGInvasiveAnnualGrassCover")))
  } else (message("SG_Group is missing from species list. Sagegrouse metrics will not be calculated. "))

  NonPlantCover <- left_join(pct_NonPlantGroundCover(lpi_tall, hit = "any", masterspecieslist, unit = unit)%>%
                               dplyr::select(!!!level,
                                             dplyr::any_of(c("AH_TotalLitterThatchCover" = "AH_LitterThatchCover",
                                                             "AH_MossCover",
                                                             "AH_AlgaeCover",
                                                             "AH_LichenCover",
                                                             "AH_RockCover",
                                                             "AH_WaterCover",
                                                             "AH_SaltCrustCover"))),
                             pct_NonPlantGroundCover(lpi_tall, hit = "first", masterspecieslist, unit = unit)%>%
                               dplyr::select(!!!level,
                                             dplyr::any_of(c("FH_TotalLitterThatchCover" = "FH_LitterThatchCover",
                                                             "FH_MossCover",
                                                             "FH_AlgaeCover",
                                                             "FH_LichenCover",
                                                             "FH_RockCover",
                                                             "FH_WaterCover",
                                                             "FH_SaltCrustCover",
                                                             "BareSoilCover" = "FH_SoilCover",
                                                             "BareOrganicMaterialCover" = "FH_OrganicMaterialCover"))),
                             by = level_colnames
  )

  LPI_AbsoluteCover_Metrics <- Foliar %>% dplyr::left_join(header%>%dplyr::select(PlotID,
                                                                                   EvaluationID,
                                                                                   SiteName,
                                                                                   AdminState,
                                                                                   SpeciesState,
                                                                                   FieldEvalDate),
                                                            .,
                                                            by = c("PlotID", "EvaluationID"))%>%
    dplyr::left_join(., Basal, by = level_colnames)%>%
    dplyr::left_join(., AbsoluteNative, by = level_colnames)%>%
    dplyr::left_join(., AbsoluteNoxious, by = level_colnames)%>%
    dplyr::left_join(., AbsoluteHydro, by = level_colnames)%>%
    dplyr::left_join(., AbsoluteHydroFAC, by = level_colnames)%>%
    dplyr::left_join(., AbsoluteGrowthHabit, by = level_colnames)%>%
    dplyr::left_join(., AbsoluteDuration, by = level_colnames)%>%
    dplyr::left_join(., AbsoluteDurationGrowth, by = level_colnames)%>%
    dplyr::left_join(., AbsoluteNativeGrowth, by = level_colnames)%>%
    dplyr::left_join(., AbsoluteTypeDuration, by = level_colnames)%>%
    {if(any(grepl("SG_Group", colnames(masterspecieslist)))) dplyr::left_join(., AbsoluteSGGroup, by = level_colnames) else .}%>%
    dplyr::left_join(., NonPlantCover, by = level_colnames)

  return(LPI_AbsoluteCover_Metrics)
}

#'@export Community_Metrics
#'@rdname allmetrics
Community_Metrics <- function(header, SpeciesList, masterspecieslist, listtype = "speciesinventory"){

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  #Calculate all metrics using species inventory
  Rich <- Community_Richness(SpeciesList, masterspecieslist, listtype = listtype)
  C.Val <- Community_C.Value(header, SpeciesList, masterspecieslist, listtype = listtype)
  Native <- Community_Native(SpeciesList, masterspecieslist, listtype = listtype)
  Nox <- Community_NoxiousCount(header, SpeciesList, masterspecieslist, listtype = listtype)
  Hydro <- Community_Hydrophytes(header, SpeciesList, masterspecieslist, listtype = listtype)
  HydroFAC <- Community_HydroFAC(header, SpeciesList, masterspecieslist, listtype = listtype)
  GrowthForm <- Community_GrowthHabit(SpeciesList, masterspecieslist, listtype = listtype)
  Duration <- Community_Duration(SpeciesList, masterspecieslist, listtype = listtype)
  if(any(grepl("SG_Group", colnames(masterspecieslist)))){
    SGgroup <- Community_SGGroup(SpeciesList, masterspecieslist, listtype = listtype, method = "count")%>%
      dplyr::rename("NumSpp_PreferredForb" = "SppInv_SGPreferredForb_Cnt")
  } else (message("SG_Group is missing from species list. Sagegrouse metrics will not be calculated. "))
  stability <- Community_StabilityGrowthHabit(SpeciesList, masterspecieslist, listtype = listtype, method = "count")%>%
    dplyr::select(EvaluationID,
                  any_of(c("SppInv_HerbaceousHighStability_Cnt",
                           "SppInv_WoodyHighStability_Cnt")))

  #Join all metrics into one table with PlotID, Name and AdminState.
  AllCommunityMetrics <- dplyr::left_join(header%>%dplyr::select(PlotID,
                                                                 EvaluationID,
                                                                 SiteName,
                                                                 AdminState,
                                                                 SpeciesState,
                                                                 FieldEvalDate),
                                          Rich, by = "EvaluationID")%>%
    dplyr::left_join(., C.Val, by = "EvaluationID") %>%
    dplyr::left_join(., Native, by = "EvaluationID")%>%
    dplyr::left_join(., Nox, by = "EvaluationID")%>%
    dplyr::left_join(., Hydro, by = "EvaluationID")%>%
    dplyr::left_join(., HydroFAC, by = "EvaluationID")%>%
    dplyr::left_join(., GrowthForm, by = "EvaluationID")%>%
    dplyr::left_join(., Duration, by = "EvaluationID")%>%
    {if(any(grepl("SG_Group", colnames(masterspecieslist)))) dplyr::left_join(., SGgroup, by = "EvaluationID") else .}%>%
    dplyr::left_join(., stability, by = "EvaluationID")

  return(AllCommunityMetrics)
}

#'@export allmetrics_byspecies
#'@rdname allmetrics
allmetrics_byspecies <- function(header, spp_inventory_tall, lpi_tall, height_tall, woody_tall, tree_tall = NULL, annualuse_tall, masterspecieslist, unknowncodes){

  SpeciesCover <- pct_AbsoluteSpeciesCover(lpi_tall, masterspecieslist)

  SpeciesHeight <- height_metrics(height_tall, masterspecieslist, unknowncodes, method = "mean", by_species = T)

  SpeciesAnnualUse <- use_metrics(header, annualuse_tall, woody_tall, masterspecieslist, by_species = T)

  SpeciesAgeClass <- ageclass_metrics(header, woody_tall, masterspecieslist, by_species = T, tree_tall = tree_tall)

  #Create a list of plots where annual use was measured
  AnnualUseEvaluationIDs <- annualuse_tall%>%
    dplyr::filter(AnnualUseCollected == "Yes")%>%
    dplyr::distinct(EvaluationID)%>%
    dplyr::pull(EvaluationID)

  #Create a table of species seen during annual use only visits which can be added to the full species inventory
  AnnualUseSpeciesHeader <- header%>%
    dplyr::filter(VisitType == "Annual Use Visit")%>%
    {if(!is.null(SpeciesAnnualUse)) dplyr::left_join(.,
                                                     SpeciesAnnualUse%>%dplyr::select(EvaluationID,
                                                                                      PlotID,
                                                                                      Species,
                                                                                      UnknownCodeKey),
                                                     by = c("EvaluationID", "PlotID"),
                                                     multiple = 'all')
      else .}

  # Create a species list of all species in species inventory for which there is
  # a matching plot in the header.
  SpeciesList <- dplyr::inner_join(header%>%
                                     sf::st_drop_geometry(),
                                   spp_inventory_tall, by = c("PlotID", "EvaluationID"))%>%
    # Add back annual use species since these will not have a species inventory
    {if(nrow(AnnualUseSpeciesHeader)>0)
      dplyr::bind_rows(.,
                       AnnualUseSpeciesHeader%>%
                       sf::st_drop_geometry()%>%
                       dplyr::select(-c(VisitType, LatitudeWGS84, LongitudeWGS84)))
      else .
      }%>%
    dplyr::left_join(., masterspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::mutate(UnknownCodeKey = ifelse(Species.y %in% c(NA, ""), UnknownCodeKey, NA))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::distinct(EvaluationID, Species, UnknownCodeKey, .keep_all = T)%>%
    dplyr::select(any_of(c("EvaluationID",
                           "PlotID",
                           "SiteName",
                           "SamplingApproach",
                           "AdminState",
                           "SpeciesState",
                           "StateCode",
                           "WetlandIndicatorRegion",
                           "CowardinAttribute",
                           "HGMClass",
                           "WetlandType",
                           "EcotypeAlaska",
                           "PlotLayout",
                           "PlotArea_m2",
                           "Elevation_m",
                           "Species",
                           "UnknownCodeKey",
                           "ScientificName" = "Scientific.Name",
                           "GenusSpecies",
                           "CommonName" = "Common.Name",
                           "Species.y",
                           "GrowthHabit" = "type",
                           "GrowthHabitSub",
                           "Duration",
                           "NativeStatus",
                           "StabilityRating" = "StabilityNum",
                           "SG_Group",
                           "PreferredForb",
                           "Abundance" = "abundance")),
                  ends_with("_NOX"),
                  ends_with("_C.Value"),
                  ends_with("WetStatus"))%>%
    tibble::add_column(., StateNoxious = NA,
                       StateCValue = NA,
                       WetlandIndicatorStatus = NA)

  #Add a C-Value based on AdminState.
  for (i in 1:nrow(SpeciesList)){
    if(!is.na(SpeciesList$SpeciesState[i])){
      C.Valuelist <- paste(SpeciesList$SpeciesState[i], "_C.Value", sep = "")
      StateC.Value <- SpeciesList[[i,C.Valuelist]]
      SpeciesList$StateCValue[i] <- StateC.Value
    }
  }

  #Add a Noxious designation by state.
  for (i in 1:nrow(SpeciesList)){
    if(!is.na(SpeciesList$SpeciesState[i])){
      noxiouslist <- paste(SpeciesList$SpeciesState[i], "_NOX", sep = "")
      statenoxious <- SpeciesList[[i,noxiouslist]]
      SpeciesList$StateNoxious[i] <- ifelse(statenoxious != "" & !is.na(statenoxious), "Noxious", "")
    }
  }

  if(!missing(unknowncodes)){
    SpeciesList <- SpeciesList%>%
      dplyr::left_join(.,
                       unknowncodes%>%
                         dplyr::select(EvaluationID, PlotID,
                                       UnknownCodeKey,
                                       GrowthHabitUnknown = GrowthHabit,
                                       DurationUnknown = Duration),
                       by = c("EvaluationID", "PlotID", "UnknownCodeKey"))%>%
      dplyr::mutate(Duration = ifelse(Duration==""|is.na(Duration), DurationUnknown, Duration),
                    GrowthHabitSub = ifelse(GrowthHabitSub==""|is.na(GrowthHabitSub), GrowthHabitUnknown, GrowthHabitSub))%>%
      dplyr::select(-c(GrowthHabitUnknown, DurationUnknown))
  }

  #Add a WetlandIndicatorStatus based on Region. First change all the species statuses that are blank to NR
  SpeciesList <- SpeciesList%>%
    dplyr::mutate(AW_WetStatus = ifelse(Species.y!=""&AW_WetStatus=="","NR",AW_WetStatus),
                  WMVC_WetStatus = ifelse(Species.y!=""&WMVC_WetStatus=="","NR", WMVC_WetStatus),
                  GP_WetStatus = ifelse(Species.y!=""&GP_WetStatus=="","NR", GP_WetStatus),
                  AK_WetStatus = ifelse(Species.y!=""&AK_WetStatus=="","NR", AK_WetStatus))%>%
    dplyr::mutate(WetlandIndicatorStatus = case_when(WetlandIndicatorRegion=="Arid West" ~AW_WetStatus,
                                        WetlandIndicatorRegion=="Western Mountains, Valleys, and Coast" ~WMVC_WetStatus,
                                        WetlandIndicatorRegion=="Great Plains" ~GP_WetStatus,
                                        WetlandIndicatorRegion=="Alaska" ~AK_WetStatus,
                                        TRUE ~ "REGIONMISSING"))%>%
    dplyr::select(-c(ends_with("_NOX"), ends_with("_C.Value"), ends_with("_WetStatus"), Species.y))

  #Ensure that all species in these other tables are already included in the species list, provide a warning if not. This is particularly problematic if it happens with LPI.
  if(nrow(dplyr::anti_join(SpeciesCover, SpeciesList, by = c("PlotID", "EvaluationID", "Code" = "Species", "Scientific.Name" = "ScientificName", "Common.Name" = "CommonName", "UnknownCodeKey")))>0){
    warning("Some plant codes used in LPI are not found in Species Inventory. These species will be excluded. For more information, try `anti_join(abscover, spp_inventory_tall, by = c('EvaluationID', 'Code' = 'Species'))` where abscover is the dataframe produced from the `pct_AbsoluteSpeciesCover` function. ")
  }
  if(nrow(dplyr::anti_join(SpeciesAgeClass, SpeciesList, by = c("PlotID", "EvaluationID", "Species", "UnknownCodeKey")))>0){
    warning("Some plant codes used in age class metrics are not found in Species Inventory. These species will be excluded.")
  }
  if(!is.null(SpeciesAnnualUse)){
    if(nrow(dplyr::anti_join(SpeciesAnnualUse, SpeciesList, by = c("PlotID", "EvaluationID", "Species", "UnknownCodeKey")))>0){
      warning("Some plant codes used in annual use metrics are not found in Species Inventory. These species will be excluded.")
      }
    }

  SpeciesList <- SpeciesList%>%
    dplyr::left_join(., SpeciesCover, by = c("PlotID", "EvaluationID", "Species" = "Code", "ScientificName" = "Scientific.Name", "CommonName" = "Common.Name", "UnknownCodeKey"))%>%
    dplyr::left_join(., SpeciesHeight, by = c("PlotID", "EvaluationID", "Species", "UnknownCodeKey"))%>%
    dplyr::left_join(., SpeciesAgeClass, by = c("PlotID", "EvaluationID","Species", "UnknownCodeKey"))%>%
    {if(!is.null(SpeciesAnnualUse))
      dplyr::left_join(., SpeciesAnnualUse, by = c("PlotID", "EvaluationID", "Species", "UnknownCodeKey"))
      else .}%>%
    dplyr::left_join(., header%>%dplyr::select(EvaluationID), by = "EvaluationID")%>%
    #mutate so that 0s replace NA's, where applicable
    dplyr::mutate(
      #Replace any NAs in species cover and height cnt with 0 if not an annual use visit.
      dplyr::across(.cols = c(AH_SpeciesCover, Hgt_Species_Cnt),
                    ~ifelse(!(EvaluationID %in% header$EvaluationID[header$VisitType=="Annual Use Visit"]),
                            tidyr::replace_na(., 0),
                            .)),
      #replace any NAs in woody structure counts with 0 if species is woody.
      dplyr::across(
        .cols = dplyr::starts_with("WS") & dplyr::ends_with(c("PctQdrts", "Cnt")), ~ifelse(GrowthHabit == "Woody", tidyr::replace_na(.,0), .)))%>%
    {if(!is.null(SpeciesAnnualUse))
      #Replace any NAs in use counts with 0 if annual use was measured. This should only occur if annual use data was collected anywhere in the dataset.
      dplyr::mutate(.,
                    AU_StubbleHgt_Cnt = ifelse(EvaluationID %in% AnnualUseEvaluationIDs &
                                                 GrowthHabitSub == "Graminoid",
                                               tidyr::replace_na(AU_StubbleHgt_Cnt, 0),
                                               AU_StubbleHgt_Cnt),
                    AU_WoodyUseClass_Cnt = ifelse(EvaluationID %in% AnnualUseEvaluationIDs &
                                                    GrowthHabit == "Woody" &
                                                    WetlandIndicatorStatus %in% c("FACW", "FAC", "OBL"),
                                                  tidyr::replace_na(AU_WoodyUseClass_Cnt, 0),
                                                  AU_WoodyUseClass_Cnt))
      else .}


  return(SpeciesList)

}

#'@export allmetrics_byplot
#'@rdname allmetrics
allmetrics_byplot <- function(header,
                              spp_inventory_tall,
                              lpi_tall,
                              height_tall,
                              woody_tall,
                              tree_tall = NULL,
                              annualuse_tall,
                              hummocks = NULL,
                              hydrology = NULL,
                              disturbances = NULL,
                              soils = NULL,
                              gap_tall = NULL,
                              soil_stability_tall = NULL,
                              waterqualdet = NULL,
                              unknowncodes,
                              masterspecieslist){

  #create vector in which to store missing indicator sets.
  missingindicators <- c()

  print("Calculating cover metrics...")
  absolutecovermetrics <- CombineAbsoluteCoverMetrics(header, lpi_tall, masterspecieslist, unknowncodes)%>%
    dplyr::rename_with(., .fn = ~stringr::str_replace(.x, "Absolute", ""), .cols = matches("Absolute"))%>%
    sf::st_drop_geometry()

  unknowncover <- pct_UnknownCover(lpi_tall, masterspecieslist, covertype = "relative", unit = "by_plot")%>%
    dplyr::rename("LPI_RelativeUnknownCover" = "RelativeUnknownCover")

  abscover <- pct_AbsoluteSpeciesCover(lpi_tall, masterspecieslist, unit = "by_plot")
  cwmmetrics <- cwm_metrics(abscover, header, masterspecieslist)

  print("Calculating community metrics...")
  communitymetrics <- Community_Metrics(header = header, SpeciesList = spp_inventory_tall, masterspecieslist = masterspecieslist, listtype = "speciesinventory")%>%
    sf::st_drop_geometry()

  print("Calculating height metrics...")
  heightmetrics <- height_metrics(height_tall, masterspecieslist, method = "mean")

  print("Calculating woody and annual use metrics...")
  ageclassmetrics <- ageclass_metrics(header, woody_tall, tree_tall = tree_tall, masterspecieslist = masterspecieslist)%>%
    dplyr::select(-dplyr::starts_with("WS_TreeMaxHgtClass"))

  if(!is.null(tree_tall)&any(grepl("SG_Group", colnames(masterspecieslist)))){
    sgconifermetrics <- SGConifer_metrics(tree_tall, masterspecieslist = masterspecieslist, unit = "by_plot")
  }  else{missingindicators <- c(missingindicators, "Sage Grouse Tree Metrics")}

  usemetrics <- use_metrics(header, annualuse_tall, woody_tall, masterspecieslist)

  if(!is.null(hummocks)){
    hummocksmetrics <- hummocks_metrics(hummocks)
  } else{missingindicators <- c(missingindicators, "Hummocks")}


  print("Joining all metrics...")
  allmetrics <- dplyr::left_join(header, communitymetrics,
                                 by = c("PlotID", "EvaluationID", "SiteName", "AdminState", "SpeciesState", "FieldEvalDate"))%>%
    dplyr::left_join(., absolutecovermetrics,
                     by = c("PlotID", "EvaluationID", "SiteName", "AdminState", "SpeciesState", "FieldEvalDate"))%>%
    dplyr::left_join(., unknowncover, by = c("PlotID", "EvaluationID"))%>%
    dplyr::left_join(.,
                     cwmmetrics%>%
                       dplyr::select(-dplyr::any_of(c("LPI_CValue_CWM"))),
                     by = c("PlotID", "EvaluationID"))%>%
    dplyr::left_join(., heightmetrics, by = c("PlotID", "EvaluationID"))%>%
    dplyr::left_join(., ageclassmetrics, by = c("PlotID", "EvaluationID"))%>%
    {if(!is.null(tree_tall)&any(grepl("SG_Group", colnames(masterspecieslist)))) dplyr::left_join(., sgconifermetrics, by = c("PlotID", "EvaluationID")) else .}%>%
    {if(!is.null(hummocks)) dplyr::left_join(., hummocksmetrics, by = c("PlotID", "EvaluationID")) else .}%>%
    dplyr::left_join(., usemetrics, by = c("PlotID", "EvaluationID"))

  #optionally add water quality sample counts.
  if(!is.null(waterqualdet)){
    waterqualcount <- waterqualdet%>%
      sf::st_drop_geometry()%>%
      group_by(EvaluationID)%>%
      dplyr::summarize(WQ_SampleCnt = n())

    allmetrics <- allmetrics%>%
      dplyr::left_join(., waterqualcount, by = c("EvaluationID"))
  } else{missingindicators <- c(missingindicators, "Water Quality")}

  #optionally calculate and join gap
  if(!is.null(gap_tall)){
    gapmetrics <- gap_cover(gap_tall)$percent%>%
      dplyr::select(EvaluationID,
                    GapCover_25_50 = `25-51`,
                    GapCover_51_100 = `51-101`,
                    GapCover_101_200 = `101-201`,
                    GapCover_201_plus = `201-Inf`)%>%
      dplyr::mutate(GapCover_25_plus = GapCover_25_50 + GapCover_51_100 + GapCover_101_200 + GapCover_201_plus)

    allmetrics <- allmetrics %>%
      dplyr::left_join(., gapmetrics, by = "EvaluationID")
  } else{missingindicators <- c(missingindicators, "Gap")}

  #optionally calculate and join soil stability
  if(!is.null(soil_stability_tall)){

    soilstabmetrics <- soil_stability(soil_stability_tall, all_cover_types = F)

    allmetrics <- allmetrics %>%
      dplyr::left_join(., soilstabmetrics, by = "EvaluationID")
  } else{missingindicators <- c(missingindicators, "Soil Stability")}

  #optionally add hydrology data
  if(!is.null(hydrology)){

    hydrometrics <- hydrology%>%
      dplyr::select(EvaluationID, Co_ChannelsPresent = DominantChannelPresent)

    allmetrics <- allmetrics %>%
      dplyr::left_join(., hydrometrics, by = "EvaluationID")
  } else{missingindicators <- c(missingindicators, "Hydrology")}

  #optionally add soil data
  if(!is.null(soils)){

    soilmetrics <- soils%>%
      dplyr::arrange(SoilPitNumber)%>%
      dplyr::distinct(EvaluationID, .keep_all = T)%>%
      dplyr::select(EvaluationID, Co_PrimaryHydricSoilIndicator = HydricIndicatorPrimary)

    allmetrics <- allmetrics %>%
      dplyr::left_join(., soilmetrics, by = "EvaluationID")
  } else{missingindicators <- c(missingindicators, "Soils")}

  #optionally add disturbances data
  if(!is.null(disturbances)){

    disturbmetrics <- disturbances%>%
      dplyr::group_by(EvaluationID)%>%
      dplyr::summarize(Co_Disturbances_Cnt = dplyr::n())

    allmetrics <- allmetrics %>%
      dplyr::left_join(., disturbmetrics, by = "EvaluationID")
  } else{missingindicators <- c(missingindicators, "Disturbances")}

  #If some tables weren't found, print these out as a message.
  if(length(missingindicators) > 0){
    message("The following tables were not included in the specified arguments. Indicators associated with these tables will not be included in the output: ")
    cat(missingindicators, sep = "\n")
  }

  return(allmetrics)
}
