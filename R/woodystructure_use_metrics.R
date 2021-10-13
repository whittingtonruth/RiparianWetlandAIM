#'Calculate use metrics from stubble height and woody data.
#'
#'@param header source of header data frame.
#'@param annualuse_tall tall data frame of annual use data.
#'@param woody_tall tall data frame of woody use data.
#'@param hummocks tall data frame of hummock data.
#'@param by_line Logical. If TRUE then results will be reported further grouped by line using 'LineKey.
#'Defaults to FALSE.
#'@param by_species Logical. If TRUE then results will be reported at the species-plot level. Defaults to FALSE.
#'@returns Data.frame of the summarized woody and annual use data by plot.

#' @export use_metrics
#' @rdname woodystructure_use_metrics
use_metrics <- function(header, annualuse_tall, woody_tall, masterspecieslist, by_line = F){

  #Allow to be calculated by line
  if (by_line) {
    level <- rlang::quos(PlotID, PlotKey, LineKey)
  } else {
    level <- rlang::quos(PlotID, PlotKey)
  }

  #drop geometry attribute
  annualuse_tall <- annualuse_tall%>%
    sf::st_drop_geometry()

  woody_tall <- woody_tall%>%
    sf::st_drop_geometry()

  #Calculate annual use on graminoids
  dominantspecies <- annualuse_tall%>%
    dplyr::group_by(!!!level, StubbleHeightDominantSpecies)%>%
    dplyr::summarize(Count = n())%>%
    dplyr::mutate(rank = rank(-Count, ties.method = "random"))%>%
    dplyr::arrange(PlotKey, desc(Count))%>%
    dplyr::filter(rank<=2)%>%
    pivot_wider(., id_cols = c(!!!level), values_from = StubbleHeightDominantSpecies, names_prefix = "DominantGraminoid", names_from= rank)

  annualusemetrics <- annualuse_tall%>%
    dplyr::group_by(!!!level)%>%
    dplyr::summarize(AvgSoilAlteration = round(mean(as.numeric(SoilAlteration), na.rm = T), digits = 2),
                     AvgStubbleHeight = round(mean(StubbleHeight, na.rm = T), digits = 2),
                     PctGrazed = round(sum(ifelse(Grazed == "Yes", 1, 0))/sum(ifelse(Grazed %in% c("Yes", "No"), 1, 0))*100, digits = 2))%>%
    dplyr::left_join(., dominantspecies)

  #select relevant columns from species list.
  masterspecieslist <- masterspecieslist%>%
    dplyr::select(Symbol, ends_with("WetStatus"))

  #calculate use metrics from woodies, first by filtering woody species use to riparian woody species
  riparianwoody <- woody_tall%>%
    dplyr::left_join(header%>%dplyr::select(PlotKey, Region),
              .)%>%
    dplyr::left_join(., masterspecieslist, by = c("RiparianWoodySpecies" = "Symbol"))%>%
    dplyr::mutate(RipStatus = ifelse(Region=="Arid West", AW_WetStatus, WMVC_WetStatus))%>%
    dplyr::filter(RipStatus %in% c("OBL", "FACW", "FAC"))

  #now put together dominant woody species
  dominantripwood <- riparianwoody%>%
    dplyr::group_by(!!!level, RiparianWoodySpecies)%>%
    dplyr::summarize(Count = n())%>%
    dplyr::mutate(rank = rank(-Count, ties.method = "random"))%>%
    dplyr::arrange(PlotKey, desc(Count))%>%
    dplyr::filter(rank<=2)%>%
    tidyr::pivot_wider(., id_cols = c(!!!level), values_from = RiparianWoodySpecies, names_prefix = "DominantRiparianWoody", names_from= rank)

  #Calculate Woody Metrics
  woodymetrics <- riparianwoody%>%
    dplyr::rowwise()%>%
    dplyr::mutate(TotalUseClass = sum(ifelse(!(UseClass %in% c(NA, "")), 1, 0)))%>%
    dplyr::group_by(!!!level)%>%
    dplyr::summarize(PctWoodyNotAvailable = round(sum(ifelse(UseClass == "NA", 1, 0))/sum(TotalUseClass)*100, digits = 2),
                     AvgWoodyUseClass = round(mean(suppressWarnings(as.numeric(UseClass)), na.rm = T), digits = 2))

  #Woody together
  UseMetrics <- annualusemetrics%>%
    dplyr::left_join(., woodymetrics)%>%
    dplyr::left_join(., dominantripwood)

  return(UseMetrics)
}

#' @export ageclass_metrics
#' @rdname woodystructure_use_metrics
ageclass_metrics <- function(header, woody_tall, masterspecieslist, by_line = F, by_species = F){

  #allow metrics to be calculated at the plot level or the line level.
  if (by_line) {
    level <- rlang::quos(PlotID, PlotKey, LineKey)
  } else {
    level <- rlang::quos(PlotID, PlotKey)
  }

  #select relevant columns from species list to filter data to riparian woody species only.
  masterspecieslist <- masterspecieslist%>%
    dplyr::select(Symbol, ends_with("WetStatus"))

  #calculate use metrics from woodies, first by filtering woody species use to riparian woody species
  riparianwoody <- woody_tall%>%
    left_join(header%>%dplyr::select(PlotKey, Region),
              .)%>%
    left_join(., masterspecieslist, by = c("RiparianWoodySpecies" = "Symbol"))%>%
    dplyr::mutate(RipStatus = ifelse(Region=="Arid West", AW_WetStatus, WMVC_WetStatus))%>%
    filter(RipStatus %in% c("OBL", "FACW", "FAC"))

  #Calculate metrics by plot
  if (!by_species){
    WoodyStructureMetrics <- riparianwoody%>%
      dplyr::filter(Rhizomatous != "")%>%
      dplyr::rowwise()%>%
      dplyr::mutate(TotalAgeClassRhiz = ifelse(Rhizomatous=="Yes", 1, sum(SeedlingTally, YoungTally, MatureTally, na.rm = T)),
                       TotalAgeClass = sum(SeedlingTally, YoungTally, MatureTally, na.rm = T))%>%
      dplyr::group_by(!!!level)%>%
      dplyr::summarize(PctRhizomatous = round(sum(ifelse(Rhizomatous=="Yes", 1, 0))/sum(TotalAgeClassRhiz)*100, digits = 2),
                       PctSeedlings = round(sum(SeedlingTally, na.rm = T)/sum(TotalAgeClass)*100, digits = 2),
                       PctYoung = round(sum(YoungTally, na.rm = T)/sum(TotalAgeClass)*100, digits = 2),
                       PctMature = round(sum(MatureTally, na.rm = T)/sum(TotalAgeClass)*100, digits = 2),
                       CountRhizomatous = sum(ifelse(Rhizomatous=="Yes", 1, 0)),
                       CountSeedlings = sum(SeedlingTally, na.rm = T),
                       CountYoung = sum(YoungTally, na.rm = T),
                       CountMature = sum(MatureTally, na.rm = T))
  }

  #Calculate metrics per species.
  if (by_species){
    WoodyStructureMetrics <- riparianwoody%>%
      dplyr::filter(Rhizomatous != "")%>%
      dplyr::rowwise()%>%
      dplyr::mutate(TotalAgeClassRhiz = ifelse(Rhizomatous=="Yes", 1, sum(SeedlingTally, YoungTally, MatureTally, na.rm = T)),
                  TotalAgeClass = sum(SeedlingTally, YoungTally, MatureTally, na.rm = T))%>%
      dplyr::group_by(!!!level, RiparianWoodySpecies)%>%
      dplyr::summarize(PctRhizomatous = round(sum(ifelse(Rhizomatous=="Yes", 1, 0))/sum(TotalAgeClassRhiz)*100, digits = 2),
                       PctSeedlings = round(sum(SeedlingTally, na.rm = T)/sum(TotalAgeClass)*100, digits = 2),
                       PctYoung = round(sum(YoungTally, na.rm = T)/sum(TotalAgeClass)*100, digits = 2),
                       PctMature = round(sum(MatureTally, na.rm = T)/sum(TotalAgeClass)*100, digits = 2),
                       CountRhizomatous = sum(ifelse(Rhizomatous=="Yes", 1, 0)),
                       CountSeedlings = sum(SeedlingTally, na.rm = T),
                       CountYoung = sum(YoungTally, na.rm = T),
                       CountMature = sum(MatureTally, na.rm = T))
  }

  return(WoodyStructureMetrics)

}

#' @export hummocks_metrics
#' @rdname woodystructure_use_metrics
hummocks_metrics <- function(hummocks, by_line = F){

  if (by_line) {
    level <- rlang::quos(PlotID, PlotKey, LineKey)
  } else {
    level <- rlang::quos(PlotID, PlotKey)
  }

  hummocksmetrics <- hummocks%>%
    dplyr::group_by(!!!level)%>%
    dplyr::summarize(CountHummocks = sum(ifelse(HummocksPresent=="Yes", 1, 0)),
              PctHummocks = round(sum(Width,na.rm = T)/7500*100, digits = 2),
              AvgHummockHeight = ifelse(CountHummocks > 0, round(mean(Height, na.rm = T), digits = 2), NA),
              AvgHummockWidth = ifelse(CountHummocks > 0, round(mean(Width, na.rm = T), digits = 2), NA),
              AvgTroughWidth = ifelse(CountHummocks > 0, round((7500 - sum(Width,na.rm = T))/ CountHummocks, digits = 2), NA),
              AvgHummockSlope = ifelse(CountHummocks > 0, round(mean(SlopeClass, na.rm = T), digits = 2), NA),
              AvgHummockVegCover = ifelse(CountHummocks > 0, round(mean(VegCover, na.rm = T), digits = 2), NA))

  return(hummocksmetrics)
}

