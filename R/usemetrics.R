#'Calculate use metrics from stubble height and woody data.
#'
#'@param header source of header data frame.
#'@param annualuse_tall tall data frame of annual use data.
#'@param woody_tall tall data frame of woody use data.
#'@param hummocks tall data frame of hummock data.
#'@param by_line Logical. If TRUE then results will be reported further grouped by line using 'LineKey.
#'Defaults to FALSE.
#'@returns Data.frame of the summarized woody and annual use data by plot.


#' @export use_metrics
#' @rdname use_metrics
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
    dplyr::summarize(AverageSoilAlteration = mean(as.numeric(SoilAlteration), na.rm = T),
                     AverageStubbleHeight = mean(StubbleHeight, na.rm = T),
                     PercentGrazed = sum(ifelse(Grazed == "Yes", 1, 0))/sum(ifelse(Grazed %in% c("Yes", "No"), 1, 0))*100)%>%
    left_join(., dominantspecies)

  #filter Woodies to riparian woody species using master species list.
  masterspecieslist <- masterspecieslist%>%
    dplyr::select(Symbol, ends_with("WetStatus"))

  #calculate use metrics from woodies, first by filtering woody species use to riparian woody species
  riparianwoody <- woody_tall%>%
    left_join(header%>%dplyr::select(PlotKey, Region),
              .)%>%
    left_join(., masterspecieslist, by = c("RiparianWoodySpecies" = "Symbol"))%>%
    dplyr::mutate(RipStatus = ifelse(Region=="Arid West", AW_WetStatus, WMVC_WetStatus))%>%
    filter(RipStatus %in% c("OBL", "FACW", "FAC"))

  #now put together dominant woody species
  dominantripwood <- riparianwoody%>%
    dplyr::group_by(!!!level, RiparianWoodySpecies)%>%
    dplyr::summarize(Count = n())%>%
    dplyr::mutate(rank = rank(-Count, ties.method = "random"))%>%
    dplyr::arrange(PlotKey, desc(Count))%>%
    dplyr::filter(rank<=2)%>%
    pivot_wider(., id_cols = c(!!!level), values_from = RiparianWoodySpecies, names_prefix = "DominantRiparianWoody", names_from= rank)

  #proportions
  woodymetrics <- riparianwoody%>%
    rowwise()%>%mutate(TotalAgeClass = ifelse(Rhizomatous=="Yes", 1, sum(SeedlingTally, YoungTally, MatureTally, na.rm = T)),
                       TotalUseClass = sum(ifelse(!(UseClass %in% c(NA, "")), 1, 0)))%>%
    group_by(!!!level)%>%
    summarize(PercentRhizomatous = sum(ifelse(Rhizomatous=="Yes", 1, 0))/sum(TotalAgeClass)*100,
              PercentSeedlings = sum(SeedlingTally, na.rm = T)/sum(TotalAgeClass)*100,
              PercentYoung = sum(YoungTally, na.rm = T)/sum(TotalAgeClass)*100,
              PercentMature = sum(MatureTally, na.rm = T)/sum(TotalAgeClass)*100,
              PercentNotAvailable = sum(ifelse(UseClass == "NA", 1, 0))/sum(TotalUseClass)*100,
              AverageUseClass = mean(suppressWarnings(as.numeric(UseClass)), na.rm = T))

  #Woody together
  UseMetrics <- annualusemetrics%>%
    dplyr::left_join(., woodymetrics)%>%
    dplyr::left_join(., dominantripwood)

  return(UseMetrics)
}

#' @export hummocks_metrics
#' @rdname use_metrics
hummocks_metrics <- function(hummocks, by_line = F){

  if (by_line) {
    level <- rlang::quos(PlotID, PlotKey, LineKey)
  } else {
    level <- rlang::quos(PlotID, PlotKey)
  }

  hummocksmetrics <- hummocks%>%
    group_by(!!!level)%>%
    summarize(CountHummocks = sum(ifelse(HummocksPresent=="Yes", 1, 0)),
              AverageHummockHeight = mean(Height, na.rm = T),
              AverageHummockWidth = mean(Width, na.rm = T),
              AverageHummockTrough = (7500 - sum(Width,na.rm = T))/ CountHummocks,
              AverageHummockSlope = mean(SlopeClass, na.rm = T),
              AverageHummockVegCover = mean(VegCover, na.rm = T),
              PercentHummocks = sum(Width,na.rm = T)/7500*100)

  return(hummocksmetrics)
}
