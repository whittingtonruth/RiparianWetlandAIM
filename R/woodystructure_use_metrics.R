#'Calculate use metrics from stubble height and woody data.
#'
#'@param header Data frame. Use the data frame from the \code{header_build_lentic()} output.
#'@param annualuse_tall Data frame. Use the data frame from the \code{gather_annualuse()} output.
#'@param woody_tall Data frame. Use the data frame from the \code{gather_woodyspecies()} output.
#'@param hummocks Data frame. Use the data frame from the \code{gather_hummocks()} output.
#'@param by_line Logical. If TRUE then results will be reported further grouped by line using 'LineKey. Defaults to FALSE.
#'@param by_species Logical. If TRUE then results will be reported at the species-plot level. Defaults to FALSE.
#'@returns Data.frame of the summarized woody and annual use data by plot.

#' @export use_metrics
#' @rdname woodystructure_use_metrics
use_metrics <- function(header, annualuse_tall, woody_tall, masterspecieslist, by_line = F, by_species = F){

  #Allow to be calculated by line
  if (by_line) {
    level <- rlang::quos(PlotID, EvaluationID, LineKey)
    level_colnames <- c("PlotID", "EvaluationID", "LineKey")
  } else {
    level <- rlang::quos(PlotID, EvaluationID)
    level_colnames <- c("PlotID", "EvaluationID")
  }

  #select relevant columns from species list.
  masterspecieslist <- masterspecieslist%>%
    dplyr::select(Symbol, ends_with("WetStatus"), Species)

  #calculate use metrics from woodies, first by filtering woody species use to riparian woody species
  riparianwoody <- woody_tall%>%
    dplyr::filter(OverhangingOrRooted %in% c("Rooted-in", "Overhanging"), UseClass != "")%>%
    dplyr::right_join(header%>%
                        dplyr::select(PlotID, EvaluationID, WetlandIndicatorRegion)%>%
                        sf::st_drop_geometry(),
                     .,
                     by = c("PlotID", "EvaluationID"))%>%
    dplyr::left_join(., masterspecieslist, by = c("RiparianWoodySpecies" = "Symbol"))%>%
    dplyr::mutate(RipStatus = case_when(WetlandIndicatorRegion=="Arid West" ~AW_WetStatus,
                                        WetlandIndicatorRegion=="Western Mountains, Valleys, and Coast" ~WMVC_WetStatus,
                                        WetlandIndicatorRegion=="Great Plains" ~GP_WetStatus,
                                        WetlandIndicatorRegion=="Alaska" ~ AK_WetStatus,
                                        TRUE ~ "REGIONMISSING"),
                  UnknownCodeKey = ifelse(Species %in% c(NA, ""), UnknownCodeKey, NA))%>%
    dplyr::select(-Species)

  if(by_species == F){

    #Calculate annual use on graminoids. First find dominant species excluding "N".
    dominantspecies <- annualuse_tall%>%
      dplyr::filter(StubbleHeightDominantSpecies != "N")%>%
      dplyr::group_by(!!!level, StubbleHeightDominantSpecies)%>%
      dplyr::summarize(Count = n(), .groups = "drop_last")%>%
      dplyr::mutate(rank = rank(-Count, ties.method = "random"))%>%
      dplyr::arrange(EvaluationID, desc(Count))%>%
      dplyr::filter(rank<=2)%>%
      pivot_wider(., id_cols = c(!!!level), values_from = StubbleHeightDominantSpecies, names_prefix = "AU_DominantGraminoid", names_from= rank)

    #Next calculate AnnualUse metrics
    annualusemetrics <- annualuse_tall%>%
      dplyr::group_by(!!!level)%>%
      dplyr::summarize(AU_SoilAlteration_Avg = round(mean(as.numeric(SoilAlteration), na.rm = T), digits = 2),
                       AU_StubbleHgt_Avg = round(mean(StubbleHeight, na.rm = T), digits = 2),
                       AU_Grazed_Pct = round(sum(ifelse(Grazed == "Yes", 1, 0), na.rm = T)/sum(ifelse(Grazed %in% c("Yes", "No"), 1, 0))*100, digits = 2),
                       AU_StubbleHgt_Cnt = sum(ifelse(StubbleHeightDominantSpecies != "N", 1, 0)), .groups = "keep")%>%
      dplyr::left_join(., dominantspecies, by = level_colnames)

    #now put together dominant woody species
    dominantripwood <- riparianwoody%>%
      dplyr::filter(RipStatus %in% c("OBL", "FACW", "FAC"))%>%
      dplyr::group_by(!!!level, RiparianWoodySpecies)%>%
      dplyr::summarize(Count = n(), .groups = "drop_last")%>%
      dplyr::mutate(rank = rank(-Count, ties.method = "random"))%>%
      dplyr::arrange(EvaluationID, desc(Count))%>%
      dplyr::filter(rank<=2)%>%
      tidyr::pivot_wider(., id_cols = c(!!!level), values_from = RiparianWoodySpecies, names_prefix = "AU_DominantWoody", names_from= rank)

    #Calculate Woody Metrics
    woodymetrics <- riparianwoody%>%
      dplyr::filter(RipStatus %in% c("OBL", "FACW", "FAC"))%>%
      dplyr::rowwise()%>%
      dplyr::mutate(TotalUseClass = sum(ifelse(!(UseClass %in% c(NA, "")), 1, 0)))%>%
      dplyr::group_by(!!!level)%>%
      dplyr::summarize(AU_WoodyNotAvailable_Pct = round(sum(ifelse(UseClass == "NA", 1, 0))/sum(TotalUseClass)*100, digits = 2),
                       AU_WoodyUseClass_Avg = round(mean(suppressWarnings(as.numeric(UseClass)), na.rm = T), digits = 2),
                       AU_WoodyUseClass_Cnt = sum(TotalUseClass),
                       .groups = "drop")

    #All metrics together
    UseMetrics <- annualusemetrics%>%
      dplyr::left_join(., woodymetrics, by = level_colnames)%>%
      dplyr::left_join(., dominantripwood, by = level_colnames)%>%
      dplyr::mutate(AU_WoodyUseClass_Cnt = ifelse(!is.na(AU_SoilAlteration_Avg), tidyr::replace_na(AU_WoodyUseClass_Cnt, 0), AU_WoodyUseClass_Cnt))

  } else{
    annualuse_tall <- annualuse_tall%>%
      dplyr::left_join(.,
                       masterspecieslist%>%dplyr::select(Symbol, Species),
                       by = c("StubbleHeightDominantSpecies"="Symbol"))%>%
      dplyr::mutate(UnknownCodeStubbleKey = ifelse(Species %in% c(NA, ""), UnknownCodeStubbleKey, NA))%>%
      dplyr::select(-Species)

    #No need to calculate "dominant species" when calculating averages by species.
    annualusemetrics <- annualuse_tall%>%
      dplyr::group_by(!!!level, StubbleHeightDominantSpecies, UnknownCodeStubbleKey)%>%
      dplyr::filter(StubbleHeightDominantSpecies !="N")%>%
      dplyr::summarize(AU_StubbleHgt_Avg = round(mean(StubbleHeight, na.rm = T), digits = 2),
                       AU_Grazed_Pct = round(sum(ifelse(Grazed == "Yes", 1, 0), na.rm = T)/
                                               sum(ifelse(Grazed %in% c("Yes", "No"), 1, 0))*100, digits = 2),
                       AU_StubbleHgt_Cnt = n())%>%
      dplyr::rename(Species = StubbleHeightDominantSpecies,
                    UnknownCodeKey = UnknownCodeStubbleKey)

    woodymetrics <- riparianwoody%>%
      dplyr::rowwise()%>%
      dplyr::mutate(TotalUseClass = sum(ifelse(!(UseClass %in% c(NA, "")), 1, 0)))%>%
      dplyr::group_by(!!!level, RiparianWoodySpecies, UnknownCodeKey)%>%
      dplyr::summarize(AU_WoodyNotAvailable_Pct = round(sum(ifelse(UseClass == "NA", 1, 0))/sum(TotalUseClass)*100, digits = 2),
                       AU_WoodyUseClass_Avg = round(mean(suppressWarnings(as.numeric(UseClass)), na.rm = T), digits = 2),
                       AU_WoodyUseClass_Cnt = sum(TotalUseClass))%>%
      dplyr::rename(Species = RiparianWoodySpecies)

    #Metrics together
    UseMetrics <- rbind(annualusemetrics, woodymetrics)
  }

  return(UseMetrics)
}

#' @export ageclass_metrics
#' @rdname woodystructure_use_metrics
ageclass_metrics <- function(header, woody_tall, masterspecieslist, by_line = F, by_species = F){

  #allow metrics to be calculated at the plot level or the line level.
  if (by_line) {
    level <- rlang::quos(PlotID, EvaluationID, LineKey)
    level_colnames <- c("PlotID", "EvaluationID", "LineKey")
  } else {
    level <- rlang::quos(PlotID, EvaluationID)
    level_colnames <- c("PlotID", "EvaluationID")
  }

  # Calculate the number of quadrats for all lines with no woody species present.
  quadcalc <-
    woody_tall%>%
    dplyr::filter(WoodySpeciesPresent=="No")%>%
    dplyr::distinct(!!!level, LineKey, interval)%>%
    dplyr::mutate(n = ifelse(interval == 500, 5, 17))

  # Calculate the number of quadrats for all lines with woody species present.
  #This will use the points in Annual Use Points Repeat to count by line.
  quadcount <-
    woody_tall%>%
    dplyr::filter(WoodySpeciesPresent=="Yes")%>%
    dplyr::distinct(!!!level, LineKey, PointNbr)%>%
    dplyr::group_by(!!!level, LineKey)%>%
    dplyr::count()%>%
    dplyr::bind_rows(., quadcalc)%>%
    dplyr::group_by(!!!level)%>%
    dplyr::summarize(nquads = sum(n))

  #Calculate metrics by plot
  if (!by_species){
    ageclass_sum <- woody_tall%>%
      dplyr::filter(OverhangingOrRooted == "Rooted-in", !is.na(AgeClass), AgeClass != "")%>%
      dplyr::group_by(!!!level)%>%
      dplyr::summarize(WS_Rhizomatous_Cnt = sum(ifelse(AgeClass == "Rhizomatous", 1, 0)),
                       WS_Seedling_Cnt = sum(ifelse(AgeClass == "Seedling", 1, 0)),
                       WS_Young_Cnt = sum(ifelse(AgeClass == "Young", 1, 0)),
                       WS_Mature_Cnt = sum(ifelse(AgeClass == "Mature", 1, 0)))%>%
      dplyr::mutate(WS_Seedling_Pct = round(WS_Seedling_Cnt/sum(WS_Seedling_Cnt, WS_Young_Cnt, WS_Mature_Cnt, na.rm = T)*100, digits = 2),
                    WS_Young_Pct = round(WS_Young_Cnt/sum(WS_Seedling_Cnt, WS_Young_Cnt, WS_Mature_Cnt, na.rm = T)*100, digits = 2),
                    WS_Mature_Pct = round(WS_Mature_Cnt/sum(WS_Seedling_Cnt, WS_Young_Cnt, WS_Mature_Cnt, na.rm = T)*100, digits = 2))

    rhiz_byquad <- woody_tall%>%
      dplyr::filter(OverhangingOrRooted == "Rooted-in", AgeClass == "Rhizomatous")%>%
      dplyr::distinct(!!!level, LineKey, PointNbr)%>%
      dplyr::group_by(!!!level)%>%
      dplyr::count()%>%
      dplyr::left_join(.,
                       quadcount,
                       by = c("PlotID","EvaluationID"))%>%
      dplyr::mutate(WS_Rhizomatous_PctQdrts = round(n/nquads * 100, digits = 2))%>%
      dplyr::select(!!!level,
                    WS_Rhizomatous_PctQdrts)

    woodyheightclass_byquad <- woody_tall%>%
      dplyr::filter(OverhangingOrRooted %in% c("Rooted-in", "Overhanging"), HeightClass != "")%>%
      dplyr::mutate(HeightClass = as.numeric(stringr::str_extract(HeightClass, "[:digit:]")))%>%
      dplyr::distinct(!!!level, LineKey, PointNbr, HeightClass)%>%
      dplyr::group_by(!!!level)%>%
      dplyr::summarize(HeightClass1 = sum(ifelse(HeightClass == 1, 1, 0)),
                       HeightClass2 = sum(ifelse(HeightClass == 2, 1, 0)),
                       HeightClass3 = sum(ifelse(HeightClass == 3, 1, 0)),
                       HeightClass4 = sum(ifelse(HeightClass == 4, 1, 0)),
                       HeightClass5 = sum(ifelse(HeightClass == 5, 1, 0)),
                       HeightClass6 = sum(ifelse(HeightClass == 6, 1, 0)))%>%
      dplyr::full_join(.,
                       quadcount,
                       by = c("PlotID","EvaluationID"))%>%
      dplyr::mutate(WS_HgtClass1_PctQdrts = round(HeightClass1/nquads * 100, digits = 2),
                    WS_HgtClass2_PctQdrts = round(HeightClass2/nquads * 100, digits = 2),
                    WS_HgtClass3_PctQdrts = round(HeightClass3/nquads * 100, digits = 2),
                    WS_HgtClass4_PctQdrts = round(HeightClass4/nquads * 100, digits = 2),
                    WS_HgtClass5_PctQdrts = round(HeightClass5/nquads * 100, digits = 2),
                    WS_HgtClass6_PctQdrts = round(HeightClass6/nquads * 100, digits = 2))%>%
      dplyr::select(!!!level,
                    dplyr::starts_with("WS_HgtClass"))%>%
      dplyr::mutate(., dplyr::across(starts_with("WS_HgtClass"), ~tidyr::replace_na(., 0)))

    allwoodyheight_byquads <- woody_tall%>%
      dplyr::filter(OverhangingOrRooted %in% c("Rooted-in", "Overhanging"), HeightClass != "")%>%
      dplyr::distinct(!!!level, LineKey, PointNbr)%>%
      dplyr::group_by(!!!level)%>%
      dplyr::summarize(WoodyQuads = n())%>%
      dplyr::left_join(.,
                       quadcount,
                       by = c("PlotID","EvaluationID"))%>%
      dplyr::mutate(WS_WoodySpp_PctQdrts = round(WoodyQuads/nquads * 100, digits = 2))%>%
      dplyr::select(!!!level,
                    WS_WoodySpp_PctQdrts)

    allwoodyheight <- woody_tall%>%
      dplyr::filter(OverhangingOrRooted %in% c("Rooted-in", "Overhanging"), HeightClass != "")%>%
      dplyr::mutate(HeightClass = as.numeric(stringr::str_extract(HeightClass, "[:digit:]")))%>%
      dplyr::group_by(!!!level)%>%
      dplyr::summarize(WS_DominantHgtClass = paste("Height Class ",
                                                      which.max(tabulate(HeightClass)),
                                                      case_when(which.max(tabulate(HeightClass))==1~" (0.0 - 0.5 m)",
                                                                which.max(tabulate(HeightClass))==2~" (>0.5 - 1.0 m)",
                                                                which.max(tabulate(HeightClass))==3~" (>1.0 - 2.0 m)",
                                                                which.max(tabulate(HeightClass))==4~" (>2.0 - 4.0 m)",
                                                                which.max(tabulate(HeightClass))==5~" (>4.0 - 8.0 m)",
                                                                which.max(tabulate(HeightClass))==6~" (>8.0 m)",
                                                                TRUE~""),
                                                      sep = ""),
                       WS_WoodyHgtClass_Cnt = n())

    WoodyStructureMetrics <- woodyheightclass_byquad%>%
      dplyr::left_join(., allwoodyheight_byquads, by = level_colnames)%>%
      dplyr::left_join(., allwoodyheight, by = level_colnames)%>%
      dplyr::left_join(., ageclass_sum, by = level_colnames)%>%
      dplyr::left_join(., rhiz_byquad, by = level_colnames)%>%
      dplyr::relocate(c(WS_Rhizomatous_Cnt, WS_Seedling_Cnt, WS_Young_Cnt, WS_Mature_Cnt,
                        WS_Rhizomatous_PctQdrts, WS_Seedling_Pct, WS_Young_Pct, WS_Mature_Pct),
                      .after = WS_WoodyHgtClass_Cnt)%>%
      dplyr::mutate(dplyr::across(dplyr::ends_with(c("Cnt","PctQdrts")), ~tidyr::replace_na(., 0)))
  }

  #Calculate metrics per species.
  if (by_species){

    #Join to master list to indicate which are ID'd to Species
    woody_tall <- woody_tall%>%
      dplyr::left_join(.,
                       masterspecieslist%>%dplyr::select(Symbol, Species),
                       by = c("RiparianWoodySpecies"="Symbol"))%>%
      dplyr::mutate(UnknownCodeKey = ifelse(Species %in% c(NA, ""), UnknownCodeKey, NA))

    #Calculate metrics on known species
    ageclass_sum <- woody_tall%>%
      dplyr::filter(OverhangingOrRooted == "Rooted-in", !is.na(AgeClass), AgeClass != "")%>%
      dplyr::group_by(!!!level, RiparianWoodySpecies, UnknownCodeKey)%>%
      dplyr::summarize(WS_Rhizomatous_Cnt = sum(ifelse(AgeClass == "Rhizomatous", 1, 0)),
                       WS_Seedling_Cnt = sum(ifelse(AgeClass == "Seedling", 1, 0)),
                       WS_Young_Cnt = sum(ifelse(AgeClass == "Young", 1, 0)),
                       WS_Mature_Cnt = sum(ifelse(AgeClass == "Mature", 1, 0)))%>%
      dplyr::mutate(WS_Seedling_Pct = round(WS_Seedling_Cnt/sum(WS_Seedling_Cnt, WS_Young_Cnt, WS_Mature_Cnt, na.rm = T)*100,
                                            digits = 2),
                    WS_Young_Pct = round(WS_Young_Cnt/sum(WS_Seedling_Cnt, WS_Young_Cnt, WS_Mature_Cnt, na.rm = T)*100,
                                         digits = 2),
                    WS_Mature_Pct = round(WS_Mature_Cnt/sum(WS_Seedling_Cnt, WS_Young_Cnt, WS_Mature_Cnt, na.rm = T)*100,
                                          digits = 2))

    rhiz_byquad <- woody_tall%>%
      dplyr::filter(OverhangingOrRooted == "Rooted-in", AgeClass == "Rhizomatous")%>%
      dplyr::distinct(!!!level, LineKey, PointNbr, RiparianWoodySpecies, UnknownCodeKey)%>%
      dplyr::group_by(!!!level, RiparianWoodySpecies, UnknownCodeKey)%>%
      dplyr::count()%>%
      dplyr::left_join(.,
                       quadcount,
                       by = c("PlotID","EvaluationID"))%>%
      dplyr::mutate(WS_Rhizomatous_PctQdrts = round(n/nquads * 100, digits = 2))%>%
      dplyr::select(!!!level, RiparianWoodySpecies, UnknownCodeKey,
                    WS_Rhizomatous_PctQdrts)

    woodyheightclass_byquad <- woody_tall%>%
      dplyr::filter(OverhangingOrRooted %in% c("Rooted-in", "Overhanging"), HeightClass != "")%>%
      dplyr::mutate(HeightClass = as.numeric(stringr::str_extract(HeightClass, "[:digit:]")))%>%
      dplyr::distinct(!!!level, LineKey, PointNbr, RiparianWoodySpecies, UnknownCodeKey, HeightClass)%>%
      dplyr::group_by(!!!level, RiparianWoodySpecies, UnknownCodeKey)%>%
      dplyr::summarize(HeightClass1 = sum(ifelse(HeightClass == 1, 1, 0)),
                       HeightClass2 = sum(ifelse(HeightClass == 2, 1, 0)),
                       HeightClass3 = sum(ifelse(HeightClass == 3, 1, 0)),
                       HeightClass4 = sum(ifelse(HeightClass == 4, 1, 0)),
                       HeightClass5 = sum(ifelse(HeightClass == 5, 1, 0)),
                       HeightClass6 = sum(ifelse(HeightClass == 6, 1, 0)))%>%
      dplyr::left_join(.,
                       quadcount,
                       by = c("PlotID","EvaluationID"))%>%
      dplyr::mutate(WS_HgtClass1_PctQdrts = round(HeightClass1/nquads * 100, digits = 2),
                    WS_HgtClass2_PctQdrts = round(HeightClass2/nquads * 100, digits = 2),
                    WS_HgtClass3_PctQdrts = round(HeightClass3/nquads * 100, digits = 2),
                    WS_HgtClass4_PctQdrts = round(HeightClass4/nquads * 100, digits = 2),
                    WS_HgtClass5_PctQdrts = round(HeightClass5/nquads * 100, digits = 2),
                    WS_HgtClass6_PctQdrts = round(HeightClass6/nquads * 100, digits = 2))%>%
      dplyr::select(!!!level, RiparianWoodySpecies, UnknownCodeKey,
                    dplyr::starts_with("WS_HgtClass"))

    allwoodyheight_byquads <- woody_tall%>%
      dplyr::filter(OverhangingOrRooted %in% c("Rooted-in", "Overhanging"), HeightClass != "")%>%
      dplyr::distinct(!!!level, LineKey, PointNbr, RiparianWoodySpecies, UnknownCodeKey)%>%
      dplyr::group_by(!!!level, RiparianWoodySpecies, UnknownCodeKey)%>%
      dplyr::summarize(WoodyQuads = n())%>%
      dplyr::left_join(.,
                       quadcount,
                       by = c("PlotID","EvaluationID"))%>%
      dplyr::mutate(WS_WoodySpp_PctQdrts = round(WoodyQuads/nquads * 100, digits = 2))%>%
      dplyr::select(!!!level, RiparianWoodySpecies, UnknownCodeKey,
                    WS_WoodySpp_PctQdrts)

    allwoodyheight <- woody_tall%>%
      dplyr::filter(OverhangingOrRooted %in% c("Rooted-in", "Overhanging"), HeightClass != "")%>%
      dplyr::mutate(HeightClass = as.numeric(stringr::str_extract(HeightClass, "[:digit:]")))%>%
      dplyr::group_by(!!!level, RiparianWoodySpecies, UnknownCodeKey)%>%
      dplyr::summarize(WS_DominantHgtClass = paste("Height Class ",
                                                      which.max(tabulate(HeightClass)),
                                                      case_when(which.max(tabulate(HeightClass))==1~" (0.0 - 0.5 m)",
                                                                which.max(tabulate(HeightClass))==2~" (>0.5 - 1.0 m)",
                                                                which.max(tabulate(HeightClass))==3~" (>1.0 - 2.0 m)",
                                                                which.max(tabulate(HeightClass))==4~" (>2.0 - 4.0 m)",
                                                                which.max(tabulate(HeightClass))==5~" (>4.0 - 8.0 m)",
                                                                which.max(tabulate(HeightClass))==6~" (>8.0 m)",
                                                                TRUE~""),
                                                      sep = ""),
                       WS_WoodyHgtClass_Cnt = n())

    WoodyStructureMetrics <- woodyheightclass_byquad%>%
      dplyr::left_join(., allwoodyheight_byquads, by = c(level_colnames, "RiparianWoodySpecies", "UnknownCodeKey"))%>%
      dplyr::left_join(., allwoodyheight, by = c(level_colnames, "RiparianWoodySpecies", "UnknownCodeKey"))%>%
      dplyr::left_join(., ageclass_sum, by = c(level_colnames, "RiparianWoodySpecies", "UnknownCodeKey"))%>%
      dplyr::left_join(., rhiz_byquad, by = c(level_colnames, "RiparianWoodySpecies", "UnknownCodeKey"))%>%
      dplyr::rename(Species = RiparianWoodySpecies)%>%
      dplyr::relocate(c(WS_Rhizomatous_Cnt, WS_Seedling_Cnt, WS_Young_Cnt, WS_Mature_Cnt,
                        WS_Rhizomatous_PctQdrts, WS_Seedling_Pct, WS_Young_Pct, WS_Mature_Pct),
                      .after = WS_WoodyHgtClass_Cnt)

  }

  return(WoodyStructureMetrics)
}

#' @export hummocks_metrics
#' @rdname woodystructure_use_metrics
hummocks_metrics <- function(hummocks, by_line = F){

  if (by_line) {
    level <- rlang::quos(PlotID, EvaluationID, LineKey)
  } else {
    level <- rlang::quos(PlotID, EvaluationID)
  }

  nohummockplots <- hummocks%>%
    dplyr::filter(HummocksPresentLine == "No")%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::count()%>%
    dplyr::filter(n==3)%>%
    dplyr::mutate(H_Hummock_Cnt = 0,
                  H_Hummock_Pct = 0)%>%
    dplyr::select(-n)

  hummocksmetrics <- hummocks%>%
    dplyr::group_by(!!!level)%>%
    dplyr::summarize(H_Hummock_Cnt = sum(ifelse(HummocksPresentLine=="Yes", 1, 0)),
              H_Hummock_Pct = round(sum(Width,na.rm = T)/7500*100, digits = 2),
              H_HummockHgt_Avg = ifelse(H_Hummock_Cnt > 0, round(mean(Height, na.rm = T), digits = 2), NA),
              H_HummockWidth_Avg = ifelse(H_Hummock_Cnt > 0, round(mean(Width, na.rm = T), digits = 2), NA),
              H_HummockTroughWidth_Avg = ifelse(H_Hummock_Cnt > 0, round((7500 - sum(Width,na.rm = T))/ H_Hummock_Cnt, digits = 2), NA),
              H_HummockSlope_Avg = ifelse(H_Hummock_Cnt > 0, round(mean(SlopeClass, na.rm = T), digits = 2), NA),
              H_HummockVegCover_Avg = ifelse(H_Hummock_Cnt > 0, round(mean(VegCover, na.rm = T), digits = 2), NA))%>%
    bind_rows(.,
              nohummockplots)

  return(hummocksmetrics)
}

