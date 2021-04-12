#'Function used to calculate the percent cover by different grouping variables.
#'
#'@param lpi_tall source of lpi_tall data frame.
#'@param masterspecieslist Character string. Full file path (including extension) to the file
#'containing the species list.
#'@param tall Logical. If TRUE then the returned data frame will be tall rather than wide and will not have
#'opbervations for non-existent values. Defaults to FALSE.
#'@param hit Character string. Absolute cover can be calculated from "any", "first", or "basal" hits. This
#'will count all pin drops with hits fitting into `grouping_variable` categories in specified layers and
#'calculate their cover relative to total pin drops. If "first" is used, only TopCanopy hits will be counted.
#'If "basal" is used, only SoilSurface hits will be counted. Relative cover can be calculated from "all" hits,
#'counting all hits of vascular species within `grouping_variable` categories. Defaults to "any".
#'@param by_line Logical. If TRUE then results will be reported further grouped by line using 'LineKey.
#'Defaults to FALSE.
#'@param ... Optional character strings. One or more variable name to calculate percent cover for, i.e.
#'"GrowthHabit", "Duration", "Nativity", or "WetlandIndicatorStatus".
#'

#'@export lentic_covermetrics
#'@rdname lentic_covermetrics
lentic_covermetrics <- function(lpi_tall, masterspecieslist){

  #many cover indicators need to be filtered to plant codes only. Use this regex expression to filter:
  nonplantcodesfilter <- paste(paste("\\.", nonplantcodes$code, "$", sep = ""), collapse = "|")

  #cover calculation for foliar cover
  PercentFoliarCover <- pct_cover_lentic(lpi_tall,
                                         tall = TRUE,
                                         hit = "first",
                                         by_line = FALSE,
                                         code)%>%
    dplyr::filter(!stringr::str_detect(indicator, nonplantcodesfilter))%>%
    dplyr::group_by(PlotKey)%>%
    dplyr::summarize(AbsoluteFoliarCover = sum(percent))

  PercentBasalCover <- pct_cover_lentic(lpi_tall,
                                        tall = TRUE,
                                        hit = "basal",
                                        by_line = TRUE,
                                        code)%>%
    dplyr::filter(!stringr::str_detect(indicator, nonplantcodesfilter))%>%
    dplyr::group_by(PlotKey)%>%
    dplyr::summarize(AbsoluteFoliarCover = sum(percent))
}
