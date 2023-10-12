#'Calculate percent cover of grouping variables categories.
#'
#'@description Underlying function used in percent cover functions to perform final cover calculation. \code{lpi_tall} data frame needs to contain the column used to group variables with all categories to be included. All unique combinations of the grouping variable used will be included in the final output, excluding NA or Null values.
#'
#'Depending on the variable, some manipulation to the lpi_tall dataframe may be required prior to feeding it into this function. In some cases, users may want their output grouped differently than the original grouping variable. For example, noxious data often includes several categories of listed species (e.g. List A, List B, etc.). If cover for all noxious categories together is preferred, then the noxious variable may require transformation so that all noxious species fall into a single group. For other grouping variables, users may be interested in reporting the cover of null categories. For example, Wetland Indicator Statuses for many upland species have been left empty. To differentiate meaningfully empty groups from unknown statuses (i.e. an unknown plant identified only as Poa), some transformation of the wetland indicator variable is required. These transformations are taken care of in all built-in cover indicator functions.
#'
#'This function also allows for users to calculate either relative (i.e. proportion of vascular species hits of a particular category) or absolute (i.e. proportion of LPI pin drops of a particular category) cover using the \code{hit} argument. This argument controls how the denominator is calculated. It also changes whether duplicate hits in a single pin drop are removed from the calculation (as in the case of absolute cover) or kept in (as with relative cover). Relative calculations are completed using \code{hit = 'all'}. For these calculations, users should consider how unknown hits should be handled, whether filtered out (i.e. excluded from the denominator), or kept in.
#'
#'@param lpi_tall A tall/long-format data frame. Use the data frame from the \code{gather_lpi_lentic()} output, or a modified version of the \code{lpi_tall} table that adds categorical information columns used in cover calculations.
#'@param tall Logical. If TRUE then the returned data frame will be tall rather than wide and will not have observations for groups not observed in a given plot. Defaults to FALSE.
#'@param hit Character string. Absolute cover can be calculated from "any", "first", or "basal" hits. This will count all pin drops with hits fitting into `grouping_variable` categories in specified layers and calculate their cover relative to total pin drops. If "first" is used, only \code{"TopCanopy"} hits will be counted. If "basal" is used, only \code{"SoilSurface"} hits will be counted. Relative cover can be calculated from "all" hits, counting all hits of vascular species within `grouping_variable` categories. Defaults to "any".
#'@param unit String. The sampling unit by which data should be summarized. Should be `by_plot`, `by_line` or `by_geosurface` (for data from Lotic-Integration Plots). Defaults to `by_plot`.
#'@param ... Optional character strings. One or more variable name to calculate percent cover for, i.e. "GrowthHabit", "Duration", "Nativity", or "WetlandIndicatorStatus".
#'@returns Data frame of the percent cover by plot of each category combination of the grouping variables provided.


#'@export pct_cover_lentic
#'@rdname pct_cover_lentic
pct_cover_lentic <- function(lpi_tall,
                             #masterspecieslist,
                             tall = FALSE,
                             hit = "any",
                             unit = "by_plot",
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
  if(!(unit %in% c("by_plot", "by_line", "by_geosurface"))){
    stop("Can only summarize using a sampling unit of `by_plot`, `by_line`, or `by_geosurface` (for L-R plots only). Update unit to one of these strings. ")
  } else if (unit == "by_line") {
    level <- rlang::quos(PlotID, EvaluationID, LineKey)
    level_colnames <- c("PlotID", "EvaluationID", "LineKey")
  } else if(unit == "by_geosurface") {
    level <- rlang::quos(PlotID, EvaluationID, GeoSurface)
    level_colnames <- c("PlotID", "EvaluationID", "GeoSurface")
  } else {
    level <- rlang::quos(PlotID, EvaluationID)
    level_colnames <- c("PlotID", "EvaluationID")
  }

  #convert all grouping variables to uppercase to avoid case issues in grouping.
  if(!(rlang::as_string(rlang::quo_get_expr(grouping_variables[[1]])) == "code")){
    lpi_tall <- lpi_tall %>%
      dplyr::mutate_at(dplyr::vars(!!!grouping_variables), toupper)
  }

  #Set layer filter based on hit. If hit is "all", SoilSurface should be excluded to avoid species duplicates. If hit is "any" no filter should be applied,
  #so a universal filter is used. If hit is "first", the filter is more complicated, requiring us to select whichever code was hit first, independent of
  #layer. This is completed below.
  layerfilter <- if(hit=="basal"){
    rlang::expr(layer == "SoilSurface")
  } else if(hit=="all"){
    rlang::expr(layer!="SoilSurface")
  } else{rlang::expr(layer!="NotALayer")}

  #Next calculate the denominator. For absolute cover, this will be the number of pin drops.
  #Relative cover uses vascular plant hits as the denominator. Relative cover can only be calculated
  #for plant species, so this will require an extra step to filter out all non-plant hits.
  point_totals <- if(hit %in% c("any", "first", "basal")){
    lpi_tall%>%dplyr::distinct(LineKey, PointNbr, .keep_all = T)%>%
      dplyr::group_by(!!!level) %>%
      dplyr::summarize(total = dplyr::n(), .groups = "drop")
  } else if(hit == "all"){
    lpi_tall%>%dplyr::filter(!!layerfilter & complete.cases(!!!grouping_variables) & !code%in%nonplantcodes$code)%>%
      dplyr::distinct(LineKey, PointNbr, code, .keep_all = T)%>%
      dplyr::group_by(!!!level) %>%
      dplyr::summarize(total = dplyr::n(), .groups = "drop")
  }

  #If pct cover is being calculated for the first hit, LPI should be filtered to the first hit of each pin drop, independent of
  #the layer it is in. This involves first making layer into a factored variable, sorting LPI data, then filtering all data
  #to just the first occurrence per pindrop.
  if (hit == "first"){
    firsthits <- lpi_tall%>%
      dplyr::mutate(layer = as.character(layer))%>%
      dplyr::mutate(layer = factor(layer, levels = c("TopCanopy",
                                                     "Lower1",
                                                     "Lower2",
                                                     "Lower3",
                                                     "Lower4",
                                                     "Lower5",
                                                     "Lower6",
                                                     "Lower7",
                                                     "SoilSurface"))
                    ) %>% dplyr::arrange(layer)%>%
      dplyr::filter(!(code %in% c("", NA, "None", "N")))%>%
      dplyr::group_by(PlotID, EvaluationID, LineKey, PointNbr)%>%
      dplyr::summarize(code = dplyr::first(code))

    lpi_tall <- merge(
      x = dplyr::distinct(dplyr::select(lpi_tall,
                                        any_of(c(
                                          "PlotID",
                                          "EvaluationID",
                                          "LineKey",
                                          "PointNbr",
                                          "GeoSurface",
                                          "layer",
                                          "code",
                                          unlist(lapply(grouping_variables, rlang::as_name))))
                                        )),
      y = firsthits,
      all.y = TRUE
      )%>%
      dplyr::ungroup()
  }

  #Steps to cover calculation are as follows:
    #1. Filter to hits in target layer(s) with necessary classification information
    #2. Filter to only unique combinations of linekey, pointnbr, and grouping variable so that count will be
        #presence/absence per pindrop. This is done conditionally so that it will only happen with absolute cover calculations.
    #3. Group by defined level and grouping variables.
    #4. Count by group and summarize in table
    #5. Combine all grouping variable combinations into one field, metrics
    #6. Join to totals found in point_totals
    #7. Calculate percent.
    #8. Remove unnecessary columns
  summary <- lpi_tall %>%
    dplyr::filter(!!layerfilter, complete.cases(!!!grouping_variables))%>%
    {if(hit !="all") dplyr::distinct(.,LineKey, PointNbr, !!!grouping_variables, .keep_all = T)
      else dplyr::distinct(.,LineKey, PointNbr, code, .keep_all = T)} %>%
    dplyr::group_by(!!!level, !!!grouping_variables) %>%
    dplyr::summarize(uniquehits = dplyr::n(), .groups = "drop") %>%
    tidyr::unite(metric, !!!grouping_variables, sep = ".")%>%
    dplyr::left_join(., point_totals, by = level_colnames) %>%
    dplyr::mutate(percent = uniquehits / total * 100)%>%
    dplyr::select(-c(uniquehits, total))

  #remove all metrics with no value for one grouping_variable
  summary <- summary %>% subset(!grepl(
    x = metric,
    pattern = "^[.]|[.]$|\\.\\.|\\.NA|NA\\.|\\.NA\\."
  ))


  #Based on whether grouping was at the plot- or transect-level, expand.grid is used to
    #add columns for empty metrics across all level cases, whether level be EvaluationID or linekey.
  allsitemetrics <- if(unit == "by_plot"){
      expand.grid(EvaluationID= unique(lpi_tall%>%dplyr::pull(.,!!level[[2]])),
                  metric = unique(summary$metric), stringsAsFactors = F)%>%
      dplyr::right_join(lpi_tall%>%dplyr::distinct(PlotID, EvaluationID),
                        .,
                        by = "EvaluationID")
    } else if (unit == "by_line"){
      expand.grid(LineKey = unique(lpi_tall%>%dplyr::pull(.,!!level[[3]])),
                metric = unique(summary$metric), stringsAsFactors = F)%>%
        dplyr::right_join(lpi_tall%>%dplyr::distinct(PlotID, EvaluationID, LineKey),
                          .,
                          by = "LineKey")
    } else if (unit == "by_geosurface"){
      expand.grid(GeoSurface = unique(lpi_tall%>%dplyr::pull(.,!!level[[3]])),
                  metric = unique(summary$metric), stringsAsFactors = F)%>%

        dplyr::right_join(lpi_tall%>%dplyr::distinct(PlotID, EvaluationID, GeoSurface),
                          .,
                          by = "GeoSurface",
                          relationship = "many-to-many")
      }

  #Now join summary to full metric table.
  summary <- suppressWarnings(allsitemetrics%>%
                                dplyr::left_join(., summary, by = c("metric", level_colnames))%>%
                                dplyr::mutate_all(dplyr::funs(replace(., is.na(.), 0)))%>%
                                dplyr::mutate(metric=
                                                {ifelse(rep(hit == "all", nrow(.)),
                                                        paste("Relative.", metric, sep = ""),
                                                        paste("Absolute.", metric, sep = ""))}) %>%
                                dplyr::arrange(EvaluationID)%>%
                                dplyr::relocate(PlotID)
                              )

  #translate to wide format if desired.
  if(!tall){summary <- summary%>%
    tidyr::pivot_wider(names_from = metric, values_from = percent)
  }
  return(summary)
}

