#'Calculate average herbaceous and woody height and litter and water depth.
#'
#'@param height_tall Data frame. Use the data frame from the \code{gather_height_lentic()} output.
#'@param method Character string.  Indicates the type of summary to calculate, \code{"max"}, which yields the average maximum
#'height on the plot or \code{"mean"} which yields the mean height.
#'@param tall Logical. If TRUE then the returned data frame will be tall rather than wide and will not have
#'opbervations for non-existent values. Defaults to FALSE.
#'@param by_line Logical. If TRUE then results will be reported further grouped by line using 'LineKey.
#'Defaults to FALSE.
#'@param omit_zero Logical. Indicates whether zeros should be included in average height and depth calculations.
#'Defaults to TRUE.
#'@param by_species Logical. If TRUE, then results will  reported by species-plot rather than at the plot or transect level.
#'Defaults to FALSE.
#'@returns Data frame of the summarized height data by plot grouped into herbaceous, litter, water, woody, or woody2 categories.


#'@export height_metrics
height_metrics <- function(height_tall,
                             method = "mean",
                             tall = FALSE,
                             by_line = FALSE,
                             omit_zero = TRUE,
                             by_species = FALSE){

  if(!(method %in% c("mean", "max"))){
    stop("Method must be either 'mean' or 'max'.")
  }

  #define the level at which you are summarizing data.
  if (by_line) {
    level <- rlang::quos(PlotID, EvaluationID, LineKey)
  } else {
    level <- rlang::quos(PlotID, EvaluationID)
  }

  #group by species if desired. Otherwise group by type.
  if (by_species) {
    category <- rlang::quos(GrowthHabit_measured,Species)
  } else {
    category <- rlang::quos(type)
  }

  #Filter out zeroes if they should be omitted.
  if (omit_zero == TRUE){
    height_tall <- height_tall%>%
      dplyr::filter(Height > 0 & !(Species %in% c("", "N", "None")))
  }

  if(method == "mean"){

    height_summary <- height_tall%>%
      dplyr::group_by(!!!level, !!!category)%>%
      dplyr::summarize(AvgHeight = round(mean(Height, na.omit = T), digits = 2))%>%
      {if(by_species == F)
        dplyr::mutate(., type = ifelse(type %in% c("Woody", "Woody2", "Herbaceous"), paste("Avg", type, "Height", sep = ""), paste("Avg", type, "Depth", sep = "")))
        else dplyr::mutate(., GrowthHabit_measured = paste("Avg", GrowthHabit_measured, "Height", sep = ""))}

  }

  if (method == "max"){

    height_summary <- height_tall%>%
      dplyr::group_by(!!!level, !!!category)%>%
      dplyr::summarize(AvgHeight = max(Height, na.omit = T))%>%
      {if(by_species == F)
        dplyr::mutate(., type = ifelse(type %in% c("Woody", "Woody2", "Herbaceous"), paste("Max", type, "Height", sep = ""), paste("Max", type, "Depth", sep = "")))
        else dplyr::mutate(., GrowthHabit_measured = paste("Max", GrowthHabit_measured, "Height", sep = ""))}

  }

  if(by_species){

    by_speciescount <- height_tall%>%
      dplyr::group_by(!!!level, Species)%>%
      dplyr::summarize(AvgHeightCount = n())%>%
      dplyr::filter(!(Species %in% c(NA, "N")))

    height_summary <- height_summary%>%
      dplyr::filter(!(Species %in% c(NA, "N")))%>%
      dplyr::left_join(., by_speciescount, by = c("PlotID", "EvaluationID", "Species"))
  }

  if (tall == FALSE) {
    height_summary <- tidyr::pivot_wider(height_summary, names_from = rlang::quo_get_expr(category[[1]]), values_from = AvgHeight)
  }

  return(height_summary)
}
