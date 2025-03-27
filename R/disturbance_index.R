#'Calculate disturbance index from Human and Natural Disturbances form
#'
#'@description Function used to calculate plot-level and landscape-level disturbance index. Function uses the scope and impact scores to calculate a weighted score for each observed disturbance, then sums those scores together for an overall plot score.
#'
#'@param disturbances A tall/long-format data frame. Use the data frame from the \code{pct_AbsoluteSpeciesCover} output, then join desired species traits.

#'@export disturbance_index
#'@rdname disturbance_index
disturbance_index <- function(disturbances){

  disturbindex <- disturbances%>%
    dplyr::filter(!(Disturbance%in%c("Beaver Activity", "Recent Flood", "Instream Habitat Restoration", "AnimalBurrows", "Beaver Dam Blowout")))%>%
    dplyr::mutate(MonitoringPlotScope = as.numeric(stringr::str_sub(MonitoringPlotScope, 1,1)),
                  MonitoringPlotDegree = as.numeric(stringr::str_sub(MonitoringPlotDegree, 1,1)),
                  LandscapeScope = as.numeric(stringr::str_sub(LandscapeScope, 1,1)),
                  LandscapeDegree = as.numeric(stringr::str_sub(LandscapeDegree, 1,1)),
                  PlotImpact = dplyr::case_when((MonitoringPlotScope == 1 | MonitoringPlotDegree == 1) |
                                                  (MonitoringPlotScope == 2 & MonitoringPlotDegree == 2)~ 1,
                                                (MonitoringPlotScope == 2 | MonitoringPlotDegree == 2) ~ 4,
                                                (MonitoringPlotScope == 3 | MonitoringPlotDegree == 3) ~ 7,
                                                (MonitoringPlotScope == 4 & MonitoringPlotDegree == 4) ~ 10,
                                                TRUE ~ 0),
                  LandscapeImpact = dplyr::case_when((LandscapeScope == 1 | LandscapeDegree == 1) |
                                                       (LandscapeScope == 2 & LandscapeDegree == 2)~ 1,
                                                     (LandscapeScope == 2 | LandscapeDegree == 2) ~ 4,
                                                     (LandscapeScope == 3 | LandscapeDegree == 3) ~ 7,
                                                     (LandscapeScope == 4 & LandscapeDegree == 4) ~ 10,
                                                     TRUE ~ 0))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::summarize(TotalPlotImpact = sum(PlotImpact, na.rm = T),
                     TotalLandscapeImpact = sum(LandscapeImpact, na.rm = T))

  return(disturbindex)
  }
