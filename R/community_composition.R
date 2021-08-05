#'Summarize number or percent of species by PlotKey and provided grouping variables.
#'
#'Basic function that takes species list and summarizes either the percent or count
#'of species in that group. List provided to function should already be cleaned to remove
#'duplicate species or unwanted grouping variable categories.
#'
#'@param SpeciesList List of species by plot that will be summarized, including the grouping variables
#'for which data is being summarized by.
#'@param method character string. The method used for the produced summary table. Can
#'be \code{"percent"}, \code{"mean"}, or {"count"}. Defaults to \code{"percent"}.
#'@param tall indicates whether output will be tall table or wide. Defaults to FALSE.
#'@param ... grouping variables from the master species list to be used in calculating list.
#'

#'@export Community_Composition
Community_Composition <- function(SpeciesList, method = "percent", tall = F, ...){

  if(!(method %in% c("percent", "mean", "count"))){
    stop("Method must be 'percent', 'mean' or 'count'.")
  }

  grouping_variables <- rlang::quos(...)

  #Convert grouping variables to upper case if not numeric vector.
  SpeciesList <- SpeciesList %>%
    mutate(across(.cols = where(is.character) & c(!!!grouping_variables), .fns = ~toupper(.)))

  if(method == "count"){
    totals <- SpeciesList%>%
      dplyr::group_by(PlotKey, !!!grouping_variables)%>%
      dplyr::summarize("count" = n())
  }

  if(method == "mean"){
    totals <- SpeciesList%>%
      dplyr::group_by(PlotKey)%>%
      dplyr::summarize("average" = mean(!!!grouping_variables, na.rm = T))
  }

  if(method == "percent"){
    speciescount <- SpeciesList%>%
      dplyr::group_by(PlotKey)%>%
      dplyr::summarize(TotalSpecies = n())

    totals <- SpeciesList%>%
      dplyr::group_by(PlotKey, !!!grouping_variables)%>%
      dplyr::summarize(count = n())%>%
      dplyr::left_join(., speciescount)%>%
      dplyr::mutate(percent = count / TotalSpecies * 100)%>%
      dplyr::select(-c(count, TotalSpecies))
  }

  #If grouping variables are provided, we want to unite these variables into one category, `metrics`
  #and remove any NAs.
  if(length(grouping_variables) > 0 & method != "mean"){
    totals <-  totals %>% tidyr::unite(., metric, !!!grouping_variables, sep = ".")%>%
      subset(!grepl(
        x = metric,
        pattern = "^[.]|[.]$|\\.\\.|\\.NA|NA\\.|\\.NA\\."))

    AllSiteMetrics <- expand.grid(PlotKey= unique(SpeciesList%>%dplyr::pull(.,PlotKey)),
                metric = unique(totals$metric), stringsAsFactors = F)

    totals <- AllSiteMetrics %>%
      dplyr::left_join(., totals)%>%
      dplyr::mutate(across(.fns = ~replace(., is.na(.), 0)))%>%
      dplyr::mutate(metric =
                      {ifelse(rep(method == "percent", nrow(.)),
                              paste("Percent.", metric, sep = ""),
                              paste("Count.", metric, sep = ""))})%>%
      dplyr::arrange(PlotKey)

    if(!tall){totals <- totals%>%
      tidyr::pivot_wider(names_from = metric, values_from = -c(PlotKey, metric))
    }
    }

  return(totals)
  }
