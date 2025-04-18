#'Calculate average herbaceous and woody height and litter and water depth.
#'
#'@param height_tall Data frame. Use the data frame from the \code{gather_height_lentic()} output.
#'@param nationalspecieslist Data frame. The centrally managed master species list should be used. The assumed structure is that each row is unique on its Symbol.
#'@param unknowncodes Optional data frame. Use the data frame from the \code{gather_unknowns_lentic()} output. Unknown species list matching unknown codes to their duration and Growth habit. This is used to fill in growth habit for plants in LPI never identified to a species or genus with those fields specified. This information is used to filter out herbaceous height measurements of non-herbaceous species and woody height measurements of non-woody species. If not included, unknown growth habit measurements will be maintained in the final height calculations.
#'@param method Character string.  Indicates the type of summary to calculate, \code{"max"}, which yields the average maximum height on the plot or \code{"mean"} which yields the mean height.
#'@param unit String. The sampling unit by which data should be summarized. Should be `by_plot`, or `by_line`. `by_geosurface` is not an option for this calculation. Defaults to `by_plot`.
#'@param omit_zero Logical. Indicates whether zeros should be included in average height and depth calculations. Defaults to TRUE.
#'@param by_species Logical. If TRUE, then results will  reported by species-plot rather than at the plot or transect level. Defaults to FALSE.
#'@returns Data frame of the summarized height data by plot grouped into herbaceous, litter, water, or woody categories.


#'@export height_metrics
height_metrics <- function(height_tall,
                           nationalspecieslist,
                           unknowncodes = NULL,
                           method = "mean",
                           unit = "by_plot",
                           omit_zero = TRUE,
                           by_species = FALSE){

  if(!(method %in% c("mean", "max"))){
    stop("Method must be either 'mean' or 'max'.")
  }

  #define the level at which you are summarizing data.
  if (unit == "by_line") {
    level <- rlang::quos(PlotID, EvaluationID, LineKey)
  } else if (unit == "by_plot") {
    level <- rlang::quos(PlotID, EvaluationID)
  } else {
    stop("Incorrect unit. Height metrics can only calculated by_plot or by_line. ")
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
  #I'll also filter out woody herbaceous measurements and nonwoody woody measurements.
  height_tall<- height_tall%>%
    dplyr::left_join(.,
                     nationalspecieslist%>%dplyr::select(Symbol, TaxonLevel, WoodyNonWoody = GrowthHabit),
                     by = c("Species"="Symbol"))

  #connect to unknown plants to get a growth habit for plants not identified to species.
  if(!is.null(unknowncodes))
    height_tall <- height_tall%>%
    dplyr::left_join(., unknowncodes%>%dplyr::select(UnknownCodeKey, GrowthHabitUnknown = GrowthHabit), by = c("UnknownCodeKey"))%>%
    dplyr::mutate(WoodyNonWoody = case_when(WoodyNonWoody == "" & GrowthHabitUnknown %in% c("Forb", "Graminoid")~"NonWoody",
                                            WoodyNonWoody == "" & GrowthHabitUnknown %in% c("Tree", "Shrub")~"Woody",
                                            TRUE~WoodyNonWoody))

  #filter out all mismatched plant measurements.
  height_tall <- height_tall%>%
    dplyr::filter(!(WoodyNonWoody == "Woody" & GrowthHabit_measured == "Herbaceous") &
                    !(WoodyNonWoody == "NonWoody" & GrowthHabit_measured == "Woody"))%>%
    dplyr::mutate(UnknownCodeKey = ifelse(!TaxonLevel%in%c("Species", "Trinomial"), UnknownCodeKey, NA))%>%
    dplyr::select(-TaxonLevel, WoodyNonWoody)

  if(method == "mean"){

    height_summary <- height_tall%>%
      dplyr::group_by(!!!level, !!!category)%>%
      dplyr::summarize(Avg = round(mean(Height, na.omit = T), digits = 2),
                       Cnt = n())

    if(by_species == F){
      height_litter <- height_tall%>%
        dplyr::filter(GrowthHabit_measured == "LitterThatch", !(type %in% c(NA, "")))%>%
        dplyr::group_by(!!!level, type)%>%
        dplyr::summarize(Avg = round(mean(Height, na.omit = T), digits = 2),
                         Cnt = n())%>%
        dplyr::mutate(type = case_when(type == "HL" ~"HerbLitter",
                                       type == "WL" ~"WoodyLitter",
                                       type == "DL" ~"DecidLitter",
                                       is.na(type)~type))%>%
        dplyr::rename(GrowthHabit_measured = type)

      height_summary <- rbind(height_summary, height_litter)%>%
        dplyr::mutate(GrowthHabit_measured = ifelse(GrowthHabit_measured=="LitterThatch", "AllLitterThatch", GrowthHabit_measured))%>%
        dplyr::arrange(match(GrowthHabit_measured, c("Herbaceous", "Woody", "Water", "AllLitterThatch", "HerbLitter", "WoodyLitter", "DecidLitter")))%>%
        tidyr::pivot_longer(cols = c(Avg, Cnt))%>%
        tidyr::unite(GrowthHabit_measured, GrowthHabit_measured, name, sep = "_")%>%
        dplyr::mutate(GrowthHabit_measured = paste("Hgt_", GrowthHabit_measured, sep = ""))
    } else{
      height_summary <- height_summary%>%
        rename(Hgt_Species_Avg = Avg,
               Hgt_Species_Cnt = Cnt)
    }
  }

  if (method == "max"){

    height_summary <- height_tall%>%
      dplyr::group_by(!!!level, !!!category)%>%
      dplyr::summarize(MaxHgt = max(Height, na.omit = T),
                       Cnt = n())

    if(by_species == F){
      height_litter <- height_tall%>%
        dplyr::filter(GrowthHabit_measured == "LitterThatch", !(type %in% c(NA, "")))%>%
        dplyr::group_by(!!!level, type)%>%
        dplyr::summarize(MaxHgt = max(Height, na.omit = T),
                         Cnt = n())%>%
        dplyr::mutate(type = case_when(type == "HL" ~"HerbLitter",
                                       type == "WL" ~"WoodyLitter",
                                       type == "DL" ~"DecidLitter",
                                       is.na(type)~type))%>%
        dplyr::rename(GrowthHabit_measured = type)

      height_summary <- rbind(height_summary, height_litter)%>%
        dplyr::mutate(GrowthHabit_measured = ifelse(GrowthHabit_measured=="LitterThatch", "AllLitterThatch", GrowthHabit_measured))%>%
        dplyr::arrange(match(GrowthHabit_measured, c("Herbaceous", "Woody", "Water", "LitterThatch", "HerbLitter", "WoodyLitter", "DecidLitter")))%>%
        tidyr::pivot_longer(cols = c(MaxHgt, Cnt))%>%
        tidyr::unite(GrowthHabit_measured, GrowthHabit_measured, name)%>%
        dplyr::mutate(GrowthHabit_measured = paste("Hgt_", GrowthHabit_measured, sep = ""))
    }

  }

  if(by_species){
    height_summary <- height_summary%>%
      dplyr::filter(!(Species %in% c(NA, "N")))
  }else{
    height_summary <-
      tidyr::pivot_wider(height_summary, names_from = rlang::quo_get_expr(category[[1]]), values_from = value)%>%
      dplyr::mutate(dplyr::across(dplyr::ends_with("_Cnt"), ~tidyr::replace_na(., 0)))

  }

  return(height_summary)
}
