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

#'@export pct_cover_lentic
#'@rdname pct_cover_lentic
pct_cover_lentic <- function(lpi_tall,
                             #masterspecieslist,
                             tall = FALSE,
                             hit = "any",
                             by_line = FALSE,
                             ...){

  #Grouping variable specification
  grouping_variables <- rlang::quos(...)

  #check that arguments are entered correctly.
  if(!is.data.frame(lpi_tall)){
    stop("LPI table is not a data.frame.")
  }

  if(!(hit %in% c("any", "first", "basal", "all"))){
    stop("hit must be 'any', 'all', 'first', or 'basal'.")
  }

  #Specify how to group calculations
  if (by_line) {
    level <- rlang::quos(PlotKey, LineKey)
  } else {
    level <- rlang::quos(PlotKey)
  }

  #convert all grouping variables to uppercase to avoid case issues in grouping.
  lpi_tall <- lpi_tall %>%
    dplyr::mutate_at(dplyr::vars(!!!grouping_variables), toupper)

  #Store the list of non-species codes:
  nonspecies <- c("N", "M","W","TH","HL","DL","WL","NL","AL","SA","VL","DS","GR","CB","ST","EL","OM","S","BY","BR")

  #First calculate the denominator. For absolute cover, this will be the number of pin drops.
  #Relative cover uses vascular plant hits as the denominator. Relative cover can only be calculated
  #for plant species, so this will require an extra step to filter out all non-plant hits.
  point_totals <- if(hit %in% c("any", "first", "basal")){
    lpi_tall%>%dplyr::distinct(LineKey, PointNbr, .keep_all = T)%>%
      dplyr::group_by(!!!level) %>%
      dplyr::summarize(total = dplyr::n())
  } else if(hit == "all"){
    lpi_tall%>%dplyr::filter(layer!="basal", !code%in%nonspecies)%>%
      dplyr::distinct(LineKey, PointNbr, code, .keep_all = T)%>%
      dplyr::group_by(!!!level) %>%
      dplyr::summarize(total = dplyr::n())
  }

  #Set layer filter based on hit. If hit is "any" or "all" no filter should be applied, so a universal filter is used. .
  layerfilter <- if(hit=="first"){
    rlang::expr(layer == "TopCanopy")
  } else if(hit=="basal"){
    rlang::expr(layer == "SoilSurface")
  } else{rlang::expr(layer!="NotALayer")}

  #Steps to cover calculation are as follows:
    #1. Flter to hits in target layer(s) with necessary classification information
    #2. Filter to only unique combinations of linekey, pointnbr, and grouping variable so that count will be presence/absence per pindrop -->only for absolute cover
    #3. Group by defined level and grouping variables.
    #4. Count by group and summarize in table
    #5. Combine all grouping variable combinations into one field, indicators
    #6. Join to totals found in point_totals
    #7. Calculate percent.
    #8. Remove unnecessary columns
  summary <- lpi_tall %>%
    dplyr::filter(!!layerfilter, complete.cases(!!!grouping_variables))%>%
    dplyr::distinct(LineKey, PointNbr, !!!grouping_variables, .keep_all = T) %>%
    dplyr::group_by(!!!level, !!!grouping_variables) %>%
    dplyr::summarize(uniquehits = dplyr::n()) %>%
    tidyr::unite(indicator, !!!grouping_variables, sep = ".")%>%
    dplyr::left_join(., point_totals) %>%
    dplyr::mutate(percent = uniquehits / total * 100)%>%
    dplyr::select(-c(uniquehits, total))

  #remove all indicators with no value for one grouping_variable
  summary <- summary %>% subset(!grepl(
    x = indicator,
    pattern = "^[.]|[.]$|\\.\\.|\\.NA|NA\\.|\\.NA\\."
  ))

  #Add columns for empty indicators in all sites and mutate indicator names to include Absolute or relative.
  summary <- suppressWarnings(
    expand.grid(PlotKey = unique(lpi_tall$PlotKey),
                indicator = unique(summary$indicator), stringsAsFactors = F) %>%
      dplyr::left_join(., summary) %>%
      dplyr::mutate_all(dplyr::funs(replace(., is.na(.), 0)))%>%
      dplyr::mutate(indicator=
                      {ifelse(rep(hit == "all", nrow(.)),
                              paste("Relative.", indicator, sep = ""),
                              paste("Absolute.", indicator, sep = ""))}) %>%
      dplyr::arrange(PlotKey))

  #translate to wide format if desired.
  if(!tall){summary <- summary%>%
    tidyr::pivot_wider(names_from = indicator, values_from = percent)
  }
  return(summary)
}







