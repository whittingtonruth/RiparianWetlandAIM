#'Calculate average herbaceous and woody height and litter and water depth.
#'
#'@param height_tall Source of height_tall dataframe.
#'@param method Character string.  Indicates the type of summary to calculate, \code{"max"}, which yields the average maximum
#'height on the plot or \code{"mean"} which yields the mean height.
#'@param tall Logical. If TRUE then the returned data frame will be tall rather than wide and will not have
#'opbervations for non-existent values. Defaults to FALSE.
#'@param by_line Logical. If TRUE then results will be reported further grouped by line using 'LineKey.
#'Defaults to FALSE.
#'@param omit_zero Logical. Indicates whether zeros should be included in average height and depth calculations.
#'Defaults to TRUE.
#'@returns Data.frame of the summarized height data by plot grouped into herbaceous, litter, water, woody, or woody2 categories.


#'@export summarize_height
summarize_height <- function(height_tall,
                        method = "mean",
                        tall = FALSE,
                        by_line = FALSE,
                        omit_zero = TRUE){

  if(!(method %in% c("mean", "max"))){
    stop("Method must be either 'mean' or 'max'.")
  }

  #define the level at which you are summarizing data.
  if (by_line) {
    level <- rlang::quos(PlotID, PlotKey, LineKey)
  } else {
    level <- rlang::quos(PlotID, PlotKey)
  }

  #Filter out zeroes if they should be omitted.
  if (omit_zero == TRUE){
    height_tall <- height_tall%>%
      dplyr::filter(Height > 0 & !(Species %in% c("", "N", "None")))
  }

  if(method == "mean"){

    height_summary <- height_tall%>%
      dplyr::group_by(!!!level, type)%>%
      dplyr::summarize(AvgHeight = mean(Height, na.omit = T))%>%
      dplyr::mutate(type = ifelse(type %in% c("Woody", "Woody2", "Herbaceous"), paste("Avg", type, "Height", sep = ""),
                                  paste("Avg", type, "Depth", sep = "")))

    height_class <- height_tall%>%
      dplyr::filter(!is.na(HeightClass))%>%
      dplyr::group_by(!!!level, type)%>%
      dplyr::summarize(AvgHeight = mean(as.numeric(HeightClass), na.omit = T))%>%
      dplyr::mutate(type = paste("Avg", type, "HeightClass", sep = ""))

    allheights <- rbind(height_summary, height_class)
  }

  if (method == "max"){

    height_summary <- height_tall%>%
      dplyr::group_by(!!!level, type)%>%
      dplyr::summarize(AvgHeight = max(Height, na.omit = T))%>%
      dplyr::mutate(type = ifelse(type %in% c("Woody", "Woody2", "Herbaceous"), paste("Max", type, "Height", sep = ""),
        paste("Max", type, "Depth", sep = "")))

    height_class <- height_tall%>%
      dplyr::filter(!is.na(HeightClass))%>%
      dplyr::group_by(!!!level, type)%>%
      dplyr::summarize(AvgHeight = max(as.numeric(HeightClass), na.omit = T))%>%
      dplyr::mutate(type = paste("Max", type, "HeightClass", sep = ""))

    allheights <- rbind(height_summary, height_class)
  }

  if (tall == FALSE) {
    allheights <- tidyr::pivot_wider(allheights, names_from = type, values_from = AvgHeight)
  }

  return(allheights)
}
