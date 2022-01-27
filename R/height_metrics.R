#'Calculate average herbaceous and woody height and litter and water depth.
#'
#'@param height_tall Data frame. Use the data frame from the \code{gather_height_lentic()} output.
#'@param masterspecieslist Data frame. The centrally managed master species list should be used.
#'@param method Character string.  Indicates the type of summary to calculate, \code{"max"}, which yields the average maximum
#'height on the plot or \code{"mean"} which yields the mean height.
#'@param by_line Logical. If TRUE then results will be reported further grouped by line using 'LineKey.
#'Defaults to FALSE.
#'@param omit_zero Logical. Indicates whether zeros should be included in average height and depth calculations.
#'Defaults to TRUE.
#'@param by_species Logical. If TRUE, then results will  reported by species-plot rather than at the plot or transect level.
#'Defaults to FALSE.
#'@returns Data frame of the summarized height data by plot grouped into herbaceous, litter, water, woody, or woody2 categories.


#'@export height_metrics
height_metrics <- function(height_tall,
                           masterspecieslist,
                             method = "mean",
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

  #group by species if desired. Otherwise group by GrowthHabit_measured.
  if (by_species) {
    category <- rlang::quos(Species, UnknownCodeKey)
  } else {
    category <- rlang::quos(GrowthHabit_measured)
  }

  #Filter out zeroes if they should be omitted.
  if (omit_zero == TRUE){
    height_tall <- height_tall%>%
      dplyr::filter(Height > 0 & !(Species %in% c("", "N", "None")))
  }

  #Edit UnknownCodeKey to NA if id'd to species. This will only effect by_species calculations
  height_tall<- height_tall%>%
    dplyr::left_join(.,
                     masterspecieslist%>%dplyr::select(Symbol, ListSpecies = Species),
                     by = c("Species"="Symbol"))%>%
    dplyr::mutate(UnknownCodeKey = ifelse(ListSpecies %in% c(NA, ""), UnknownCodeKey, NA))%>%
    dplyr::select(-ListSpecies)

  if(method == "mean"){

    height_summary <- height_tall%>%
      dplyr::group_by(!!!level, !!!category)%>%
      dplyr::summarize(Hgt_AvgHeight = round(mean(Height, na.omit = T), digits = 2),
                       Hgt_Count = n())

    if(by_species == F){
      height_litter <- height_tall%>%
        dplyr::filter(GrowthHabit_measured == "LitterThatch", !is.na(type))%>%
        dplyr::group_by(!!!level, type)%>%
        dplyr::summarize(Hgt_AvgHeight = round(mean(Height, na.omit = T), digits = 2),
                         Hgt_Count = n())%>%
        dplyr::mutate(type = case_when(type == "HL" ~"HerbLitter",
                                       type == "WL" ~"WoodyLitter",
                                       type == "DL" ~"DecidLitter",
                                       is.na(type)~type))%>%
        dplyr::rename(GrowthHabit_measured = type)

      height_summary <- rbind(height_summary, height_litter)%>%
        dplyr::mutate(GrowthHabit_measured = ifelse(GrowthHabit_measured=="LitterThatch", "AllLitterThatch", GrowthHabit_measured))%>%
        dplyr::arrange(match(GrowthHabit_measured, c("Herbaceous", "Woody", "Water", "AllLitterThatch", "HerbLitter", "WoodyLitter", "DecidLitter")))%>%
        tidyr::pivot_longer(cols = c(Hgt_AvgHeight, Hgt_Count))%>%
        dplyr::mutate(name = ifelse(GrowthHabit_measured %in% c("Water", "AllLitterThatch", "HerbLitter", "WoodyLitter", "DecidLitter"),
                                    str_replace(name, "Height", "Depth"),
                                    name))%>%
        tidyr::unite(GrowthHabit_measured, name, GrowthHabit_measured)
    }
  }

  if (method == "max"){

    height_summary <- height_tall%>%
      dplyr::group_by(!!!level, !!!category)%>%
      dplyr::summarize(Hgt_MaxHeight = max(Height, na.omit = T),
                       Hgt_Count = n())

    if(by_species == F){
      height_litter <- height_tall%>%
        dplyr::filter(GrowthHabit_measured == "LitterThatch", !is.na(type))%>%
        dplyr::group_by(!!!level, type)%>%
        dplyr::summarize(Hgt_MaxHeight = max(Height, na.omit = T),
                         Hgt_Count = n())%>%
        dplyr::mutate(type = case_when(type == "HL" ~"HerbLitter",
                                       type == "WL" ~"WoodyLitter",
                                       type == "DL" ~"DecidLitter",
                                       is.na(type)~type))%>%
        dplyr::rename(GrowthHabit_measured = type)

      height_summary <- rbind(height_summary, height_litter)%>%
        dplyr::mutate(GrowthHabit_measured = ifelse(GrowthHabit_measured=="LitterThatch", "AllLitterThatch", GrowthHabit_measured))%>%
        dplyr::arrange(match(GrowthHabit_measured, c("Herbaceous", "Woody", "Water", "LitterThatch", "HerbLitter", "WoodyLitter", "DecidLitter")))%>%
        tidyr::pivot_longer(cols = c(Hgt_AvgHeight, Hgt_Count))%>%
        dplyr::mutate(name = ifelse(GrowthHabit_measured %in% c("Water", "AllLitterThatch", "HerbLitter", "WoodyLitter", "DecidLitter"),
                                    str_replace(name, "Height", "Depth"),
                                    name))%>%
        tidyr::unite(GrowthHabit_measured, name, GrowthHabit_measured)
    }

  }

  if(by_species){
    height_summary <- height_summary%>%
      dplyr::filter(!(Species %in% c(NA, "N")))
  }else{
    height_summary <-
      tidyr::pivot_wider(height_summary, names_from = rlang::quo_get_expr(category[[1]]), values_from = value)%>%
      dplyr::select(-c(Hgt_Count_Herbaceous, Hgt_Count_Woody, Hgt_Count_Water))
  }

  return(height_summary)
}
