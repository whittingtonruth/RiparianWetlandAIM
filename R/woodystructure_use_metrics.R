#'Calculate use metrics from stubble height and woody data.
#'
#'@param header Data frame. Use the data frame from the \code{header_build_lentic()} output.
#'@param annualuse_tall Data frame. Use the data frame from the \code{gather_annualuse()} output.
#'@param woody_tall Data frame. Use the data frame from the \code{gather_woodyspecies()} output.
#'@param hummocks Data frame. Use the data frame from the \code{gather_hummocks()} output.
#'@param by_line Logical. If TRUE then results will be reported further grouped by line using 'LineKey.
#'Defaults to FALSE.
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
    dplyr::right_join(header%>%dplyr::select(PlotID, EvaluationID, WetlandIndicatorRegion),
                     .,
                     by = c("PlotID", "EvaluationID"))%>%
    dplyr::left_join(., masterspecieslist, by = c("RiparianWoodySpecies" = "Symbol"))%>%
    dplyr::mutate(RipStatus = case_when(WetlandIndicatorRegion=="Arid West" ~AW_WetStatus,
                                        WetlandIndicatorRegion=="Western Mountains, Valleys, and Coast" ~WMVC_WetStatus,
                                        WetlandIndicatorRegion=="Great Plains" ~GP_WetStatus,
                                        TRUE ~ "REGIONMISSING"),
                  UnknownCodeKey = ifelse(Species %in% c(NA, ""), UnknownCodeKey, NA))%>%
    dplyr::select(-Species)

  if(by_species == F){
    #Calculate annual use on graminoids. First find dominant species.
    dominantspecies <- annualuse_tall%>%
      dplyr::group_by(!!!level, StubbleHeightDominantSpecies)%>%
      dplyr::summarize(Count = n())%>%
      dplyr::mutate(rank = rank(-Count, ties.method = "random"))%>%
      dplyr::arrange(EvaluationID, desc(Count))%>%
      dplyr::filter(rank<=2)%>%
      pivot_wider(., id_cols = c(!!!level), values_from = StubbleHeightDominantSpecies, names_prefix = "DominantGraminoid", names_from= rank)

    #Next calculate AnnualUse metrics
    annualusemetrics <- annualuse_tall%>%
      dplyr::group_by(!!!level)%>%
      dplyr::summarize(AvgSoilAlteration = round(mean(as.numeric(SoilAlteration), na.rm = T), digits = 2),
                      AvgStubbleHeight = round(mean(StubbleHeight, na.rm = T), digits = 2),
                      PctGrazed = round(sum(ifelse(Grazed == "Yes", 1, 0))/sum(ifelse(Grazed %in% c("Yes", "No"), 1, 0))*100, digits = 2))%>%
      dplyr::left_join(., dominantspecies, by = level_colnames)

    #now put together dominant woody species
    dominantripwood <- riparianwoody%>%
      dplyr::filter(RipStatus %in% c("OBL", "FACW", "FAC"))%>%
      dplyr::group_by(!!!level, RiparianWoodySpecies)%>%
      dplyr::summarize(Count = n())%>%
      dplyr::mutate(rank = rank(-Count, ties.method = "random"))%>%
      dplyr::arrange(EvaluationID, desc(Count))%>%
      dplyr::filter(rank<=2)%>%
      tidyr::pivot_wider(., id_cols = c(!!!level), values_from = RiparianWoodySpecies, names_prefix = "DominantRiparianWoody", names_from= rank)

    #Calculate Woody Metrics
    woodymetrics <- riparianwoody%>%
      dplyr::filter(RipStatus %in% c("OBL", "FACW", "FAC"))%>%
      dplyr::rowwise()%>%
      dplyr::mutate(TotalUseClass = sum(ifelse(!(UseClass %in% c(NA, "")), 1, 0)))%>%
      dplyr::group_by(!!!level)%>%
      dplyr::summarize(PctWoodyNotAvailable = round(sum(ifelse(UseClass == "NA", 1, 0))/sum(TotalUseClass)*100, digits = 2),
                       AvgWoodyUseClass = round(mean(suppressWarnings(as.numeric(UseClass)), na.rm = T), digits = 2))

    #All metrics together
    UseMetrics <- annualusemetrics%>%
      dplyr::left_join(., woodymetrics, by = level_colnames)%>%
      dplyr::left_join(., dominantripwood, by = level_colnames)

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
      dplyr::summarize(AvgStubbleHeight = round(mean(StubbleHeight, na.rm = T), digits = 2),
                PercentGrazed = round(sum(ifelse(Grazed == "Yes", 1, 0))/sum(ifelse(Grazed %in% c("Yes", "No"), 1, 0))*100, digits = 2),
                CountStubbleHeight = n())%>%
      dplyr::rename(Species = StubbleHeightDominantSpecies,
                    UnknownCodeKey = UnknownCodeStubbleKey)

    woodymetrics <- riparianwoody%>%
      dplyr::rowwise()%>%
      dplyr::mutate(TotalUseClass = sum(ifelse(!(UseClass %in% c(NA, "")), 1, 0)))%>%
      dplyr::group_by(!!!level, RiparianWoodySpecies, UnknownCodeKey)%>%
      dplyr::summarize(PctWoodyNotAvailable = round(sum(ifelse(UseClass == "NA", 1, 0))/sum(TotalUseClass)*100, digits = 2),
                AvgWoodyUseClass = round(mean(suppressWarnings(as.numeric(UseClass)), na.rm = T), digits = 2),
                CountUseClass = sum(TotalUseClass))%>%
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

  #Calculate metrics by plot
  if (!by_species){
    WoodyStructureMetrics <- woody_tall%>%
      dplyr::filter(OverhangingOrRooted == "Rooted-in", AgeClass != "")%>%
      dplyr::group_by(!!!level, AgeClass)%>%
      dplyr::count()%>%
      dplyr::mutate(AgeClass = paste("Count", AgeClass, sep = ""))%>%
      tidyr::pivot_wider(names_from = AgeClass, values_from = n)%>%
      dplyr::mutate(PctRhizomatous = round(CountRhizomatous/sum(CountSeedling, CountYoung, CountMature, CountRhizomatous, na.rm = T)*100, digits = 2),
                    PctSeedling = round(CountSeedling/sum(CountSeedling, CountYoung, CountMature, na.rm = T)*100, digits = 2),
                    PctYoung = round(CountYoung/sum(CountSeedling, CountYoung, CountMature, na.rm = T)*100, digits = 2),
                    PctMature = round(CountMature/sum(CountSeedling, CountYoung, CountMature, na.rm = T)*100, digits = 2))%>%
      dplyr::relocate(c(CountRhizomatous, CountSeedling), .before = CountYoung)

    WoodyHeightClass <- woody_tall%>%
      dplyr::filter(OverhangingOrRooted %in% c("Rooted-in", "Overhanging"), HeightClass != "")%>%
      dplyr::mutate(HeightClass = as.numeric(stringr::str_extract(HeightClass, "[:digit:]")))%>%
      dplyr::group_by(!!!level)%>%
      dplyr::summarize(CountHeightClass1 = sum(ifelse(HeightClass == 1, 1, 0)),
                       CountHeightClass2 = sum(ifelse(HeightClass == 2, 1, 0)),
                       CountHeightClass3 = sum(ifelse(HeightClass == 3, 1, 0)),
                       CountHeightClass4 = sum(ifelse(HeightClass == 4, 1, 0)),
                       CountHeightClass5 = sum(ifelse(HeightClass == 5, 1, 0)),
                       CountHeightClass6 = sum(ifelse(HeightClass == 6, 1, 0)),
                       RepresentHeightClass = sum(ifelse(CountHeightClass1>0, 1, 0),
                                                  ifelse(CountHeightClass2>0, 1, 0),
                                                  ifelse(CountHeightClass3>0, 1, 0),
                                                  ifelse(CountHeightClass4>0, 1, 0),
                                                  ifelse(CountHeightClass5>0, 1, 0),
                                                  ifelse(CountHeightClass6>0, 1, 0)),
                       AvgWoodyHeightEstimate = round(10^(-0.7083129+0.297*mean(HeightClass)), digits = 2),
                       CountWoodyHeightClass = n())

    WoodyStructureMetrics <- WoodyHeightClass%>%left_join(., WoodyStructureMetrics, by = level_colnames)
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
    WoodyStructureMetrics <- woody_tall%>%
      dplyr::filter(OverhangingOrRooted == "Rooted-in", AgeClass != "")%>%
      dplyr::group_by(!!!level, RiparianWoodySpecies, UnknownCodeKey, AgeClass)%>%
      dplyr::count()%>%
      dplyr::mutate(AgeClass = paste("Count", AgeClass, sep = ""))%>%
      tidyr::pivot_wider(names_from = AgeClass, values_from = n)%>%
      dplyr::mutate(PctRhizomatous = round(CountRhizomatous/sum(CountSeedling, CountYoung, CountMature, CountRhizomatous, na.rm = T)*100, digits = 2),
                    PctSeedling = round(CountSeedling/sum(CountSeedling, CountYoung, CountMature, na.rm = T)*100, digits = 2),
                    PctYoung = round(CountYoung/sum(CountSeedling, CountYoung, CountMature, na.rm = T)*100, digits = 2),
                    PctMature = round(CountMature/sum(CountSeedling, CountYoung, CountMature, na.rm = T)*100, digits = 2))%>%
      dplyr::relocate(c(CountRhizomatous, CountSeedling), .before = CountYoung)

    WoodyHeightClass <- woody_tall%>%
      dplyr::filter(OverhangingOrRooted %in% c("Rooted-in", "Overhanging"), HeightClass != "")%>%
      dplyr::mutate(HeightClass = as.numeric(stringr::str_extract(HeightClass, "[:digit:]")))%>%
      dplyr::group_by(!!!level, RiparianWoodySpecies, UnknownCodeKey)%>%
      dplyr::summarize(CountHeightClass1 = sum(ifelse(HeightClass == 1, 1, 0)),
                       CountHeightClass2 = sum(ifelse(HeightClass == 2, 1, 0)),
                       CountHeightClass3 = sum(ifelse(HeightClass == 3, 1, 0)),
                       CountHeightClass4 = sum(ifelse(HeightClass == 4, 1, 0)),
                       CountHeightClass5 = sum(ifelse(HeightClass == 5, 1, 0)),
                       CountHeightClass6 = sum(ifelse(HeightClass == 6, 1, 0)),
                       RepresentHeightClass = sum(ifelse(CountHeightClass1>0, 1, 0),
                                                  ifelse(CountHeightClass2>0, 1, 0),
                                                  ifelse(CountHeightClass3>0, 1, 0),
                                                  ifelse(CountHeightClass4>0, 1, 0),
                                                  ifelse(CountHeightClass5>0, 1, 0),
                                                  ifelse(CountHeightClass6>0, 1, 0)),
                       AvgWoodyHeightClass = round(mean(HeightClass), digits = 2),
                       CountWoodyHeightClass = n())

    WoodyStructureMetrics <- WoodyHeightClass%>%left_join(., WoodyStructureMetrics, by = c(level_colnames, "RiparianWoodySpecies", "UnknownCodeKey"))
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

  hummocksmetrics <- hummocks%>%
    dplyr::group_by(!!!level)%>%
    dplyr::summarize(CountHummocks = sum(ifelse(HummocksPresentLine=="Yes", 1, 0)),
              PctHummocks = round(sum(Width,na.rm = T)/7500*100, digits = 2),
              AvgHummockHeight = ifelse(CountHummocks > 0, round(mean(Height, na.rm = T), digits = 2), NA),
              AvgHummockWidth = ifelse(CountHummocks > 0, round(mean(Width, na.rm = T), digits = 2), NA),
              AvgTroughWidth = ifelse(CountHummocks > 0, round((7500 - sum(Width,na.rm = T))/ CountHummocks, digits = 2), NA),
              AvgHummockSlope = ifelse(CountHummocks > 0, round(mean(SlopeClass, na.rm = T), digits = 2), NA),
              AvgHummockVegCover = ifelse(CountHummocks > 0, round(mean(VegCover, na.rm = T), digits = 2), NA))

  return(hummocksmetrics)
}

