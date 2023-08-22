#'Summarize Species or plant group by EvaluationID based on species list.
#'
#'@description Underlying function used in community metric functions that summarizes the percent, count, or average of an indicated variable found in a provided species list. All unique combinations of the grouping variable used will be included in the final output, excluding NA or Null values.The species list provided should already be cleaned to remove duplicate species or unwanted grouping variable categories.
#'
#'@param SpeciesList Data frame. Table of species by plot that will be summarized, including the grouping variables for which data is being summarized by. Duplicates and unwanted grouping variables should be filtered out prior to submitting to function.
#'@param method character string. The method used for the produced summary table. Can be "percent", "mean", or "count". Defaults to "percent".
#'@param tall Logical. Indicates whether output will be tall table or wide. Defaults to FALSE.
#'@param ... Optional character strings. Grouping variables from the master species list to be used in calculating list.
#'@returns Data frame of summarized metrics by plot.

#'@export Community_Composition
Community_Composition <- function(SpeciesList, method = "percent", tall = F, ...){

  if(!(method %in% c("percent", "mean", "count"))){
    stop("Method must be 'percent', 'mean' or 'count'.")
  }

  grouping_variables <- rlang::quos(...)

  #Convert grouping variables to upper case if not numeric vector. Rename EvaluationID column if needed.
  SpeciesList <- SpeciesList %>%
    mutate(across(.cols = where(is.character) & c(!!!grouping_variables), .fns = ~toupper(.)))%>%
    {if("LPIDetailEvaluationID" %in% names(.)) rename(., EvaluationID = LPIDetailEvaluationID) else .}

  if(method == "count"){
    totals <- SpeciesList%>%
      dplyr::group_by(EvaluationID, !!!grouping_variables)%>%
      dplyr::summarize("Cnt" = n())
  }

  if(method == "mean"){
    totals <- SpeciesList%>%
      dplyr::group_by(EvaluationID)%>%
      dplyr::summarize("Avg" = round(mean(!!!grouping_variables, na.rm = T), digits = 2))
  }

  if(method == "percent"){
    speciescount <- SpeciesList%>%
      dplyr::group_by(EvaluationID)%>%
      dplyr::summarize(TotalSpecies = n())

    totals <- SpeciesList%>%
      dplyr::group_by(EvaluationID, !!!grouping_variables)%>%
      dplyr::summarize(count = n())%>%
      dplyr::left_join(., speciescount, by = "EvaluationID")%>%
      dplyr::mutate(Pct = round(count / TotalSpecies * 100, digits = 2))%>%
      dplyr::select(-c(count, TotalSpecies))
  }

  #If grouping variables are provided, we want to unite these variables into one category, `metrics`
  #and remove any NAs.
  if(length(grouping_variables) > 0 & method != "mean"){
    totals <-  totals %>% tidyr::unite(., metric, !!!grouping_variables, sep = ".")%>%
      subset(!grepl(
        x = metric,
        pattern = "^[.]|[.]$|\\.\\.|\\.NA|NA\\.|\\.NA\\."))

    AllSiteMetrics <- expand.grid(EvaluationID= unique(SpeciesList%>%dplyr::pull(.,EvaluationID)),
                metric = unique(totals$metric), stringsAsFactors = F)

    totals <- AllSiteMetrics %>%
      dplyr::left_join(., totals, by = c("EvaluationID", "metric"))%>%
      dplyr::mutate(across(.cols = where(is.numeric), .fns = ~replace(., is.na(.), 0)))%>%
      dplyr::mutate(metric =
                      {ifelse(rep(method == "percent", nrow(.)),
                              paste(metric, "Pct", sep = "_"),
                              paste(metric, "Cnt", sep = "_"))})%>%
      dplyr::arrange(EvaluationID)

    if(!tall){totals <- totals%>%
      tidyr::pivot_wider(names_from = metric, values_from = -c(EvaluationID, metric))
    }
    }

  return(totals)
  }
