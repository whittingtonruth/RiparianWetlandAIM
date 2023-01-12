#'Calculate percent cover metrics from LPI
#'
#'@description Metric calculation functions for specific grouping variables. These functions perform various modifications to
#'the \code{lpi_tall} data frame in preparation for the \code{pct_cover_lentic()} function, then filtering the
#'resulting data frame to the categories of interest.
#'
#'Some functions pull information from a variety of tables from the field season data used to categorize variables. Region-
#'or state-specific categorization requires joining the header data frame to the lpi_tall data frame. Growth Form and Duration
#'calculations can optionally pull in data for unknowns entered by crews in the Unknown Plant form. Wetland Indicator Status
#'and Noxious species group categories together before pushing to the \code{pct_cover_lentic()} function.
#'
#'Relative and absolute cover calculations also require different handling of plants identified to genus or higher taxonomic
#'or plant groupings due to the way the percent denominators are calculated. Relative cover requires all species without
#'relevant categorization to be removed to ensure only plant hits with a given category are included in calculations. (Example:
#'In calculating relative native cover, plant hits of Elymus species should not be included in either numerator or denominator.)
#'In contrast, absolute cover requires that all hits be submitted to the \code{pct_cover_lentic()} function, as these will
#'be used to calculate the number of pin drops (i.e. the denominator). Absolute cover filters out uncategorized hits after
#'cover has been calculated.
#'
#'Absolute and Relative cover metrics can be calculated all at once using \code{CombineRelativeCoverMetrics} and
#'\code{CombineAbsoluteCoverMetrics}.
#'
#'@param header Data frame. Use the data frame from the \code{header_build_lentic()} output. Used in Noxious and
#'Wetland Indicator calculations to specify the plot region or state.
#'@param lpi_tall A tall/long-format data frame. Use the data frame from the \code{gather_lpi_lentic()} output.
#'@param masterspecieslist Data frame. The centrally managed master species list should be used.
#'@param covertype Character string. "relative" or "absolute". Specifies the kind of cover calculation.
#'Relative cover is only used for calculations on vascular plant species and specifies the percent of
#'overall hits made up of a particular species or group. Absolute cover is the percent of the pin
#'drops made up by a particular species or group.
#'@param unknowncodes Optional data frame. Use the data frame from the \code{gather_unknowns_lentic()} output.
#'Unknown species list matching unknown codes to their duration and Growth habit. This is used to fill in duration
#'and growth habit for plants in LPI never identified to a species or genus with those fields specified. If argument
#'is unused, all unknown species without Duration or Growth Habit specified will be filtered out before being passed
#'on to \code{pct_cover_lentic()}.
#'@param hit Character string. "any", "first" or "basal". Only used in \code{pct_NonPlantGroundCover()}, where relative
#'cover is not calculated. If "any" is used, any layer will be used to calculate non-plant cover. If "first" is used, only
#'\code{"TopCanopy"} hits will be counted. If "basal" is used, only \code{"SoilSurface"} hits will be counted. Defaults to "any".
#'@return Wide data frame of percent cover by plot of different categories.

#'@export pct_FoliarCover
#'@rdname Cover_Metrics
pct_FoliarCover <- function(lpi_tall){

  #many cover indicators need to be filtered to plant codes only. Use this regex expression to filter:
  nonplantcodesfilter <- paste(paste("\\.", nonplantcodes$code, "$", sep = ""), collapse = "|")

  #cover calculation for foliar cover
  PercentFoliarCover <- pct_cover_lentic(lpi_tall,
                                         tall = TRUE,
                                         hit = "first",
                                         by_line = FALSE,
                                         code)%>%
    dplyr::filter(!stringr::str_detect(metric, nonplantcodesfilter))%>%
    dplyr::group_by(PlotID, EvaluationID)%>%
    dplyr::summarize(TotalFoliarCover = round(sum(percent), digits = 2))

  return(PercentFoliarCover)
}

#'@export pct_BasalCover
#'@rdname Cover_Metrics
pct_BasalCover <- function(lpi_tall){

  #many cover indicators need to be filtered to plant codes only. Use this regex expression to filter:
  nonplantcodesfilter <- paste(paste("\\.", nonplantcodes$code, "$", sep = ""), collapse = "|")

  PercentBasalCover <- pct_cover_lentic(lpi_tall,
                                        tall = TRUE,
                                        hit = "basal",
                                        by_line = FALSE,
                                        code)%>%
    dplyr::filter(!stringr::str_detect(metric, nonplantcodesfilter))%>%
    dplyr::group_by(PlotID, EvaluationID)%>%
    dplyr::summarize(AH_BasalCover = round(sum(percent), digits = 2))

  return(PercentBasalCover)
}

#'@export pct_TotalAbsoluteCover
#'@rdname Cover_Metrics
pct_TotalAbsoluteCover <- function(lpi_tall){

  #many cover indicators need to be filtered to plant codes only. Use this regex expression to filter:
  nonplantcodesfilter <- paste(paste("\\.", nonplantcodes$code, "$", sep = ""), collapse = "|")

  TotalAbsoluteCover <- pct_cover_lentic(lpi_tall,
                                          tall = TRUE,
                                          hit = "any",
                                          by_line = FALSE,
                                          code)%>%
    dplyr::filter(!stringr::str_detect(metric, nonplantcodesfilter))%>%
    dplyr::group_by(PlotID, EvaluationID)%>%
    dplyr::summarize(TotalAbsoluteCover = round(sum(percent), digits = 2))

  return(TotalAbsoluteCover)
}

#'@export pct_NativeCover
#'@rdname Cover_Metrics
pct_NativeCover <- function(lpi_tall, masterspecieslist, covertype = "absolute"){

  if(!(covertype %in% c("relative", "absolute"))){
    stop("covertype must be 'relative' or 'absolute'.")
  }

  fieldname <- ifelse(covertype == "relative", "Relative", "AH_")

  masterspecieslist <- masterspecieslist%>%
    dplyr::select(Symbol,
                  Scientific.Name,
                  Species,
                  GrowthHabitSub,
                  Duration,
                  NativeStatus,
                  ends_with("_WetStatus"),
                  ends_with("_C.Value"),
                  ends_with("_Nox")
    )

  masterspecieslist$NativeStatus[masterspecieslist$NativeStatus=="cryptogenic"] <- "Nonnative"

  #join lpi_tall to species list then filter out species that were not classified as either native or nonnative.
  ##Only filter out plants not classified as native or nonnative for relative cover.
  lpispeciesjoin <- dplyr::left_join(lpi_tall, masterspecieslist, by = c("code" = "Symbol"))%>%
    {if(covertype == "relative") dplyr::filter(., NativeStatus !=""|is.na(NativeStatus)) else .}

  NativeCover <- pct_cover_lentic(lpispeciesjoin,
                                         tall = TRUE,
                                         hit = switch(covertype,
                                                      "relative" = "all",
                                                      "absolute" = "any"),
                                         by_line = FALSE,
                                         NativeStatus)%>%
    dplyr::mutate(metric = paste(fieldname, stringr::str_to_title(stringr::str_replace(metric, "Relative\\.|Absolute\\.", "")), "Cover", sep = ""))%>%
    dplyr::filter(grepl("Native|Nonnative", metric))%>%
    dplyr::mutate(percent = round(percent, digits = 2))%>%
    tidyr::pivot_wider(names_from = metric, values_from = percent)

  return(NativeCover)
}

#'@export pct_NoxiousCover
#'@rdname Cover_Metrics
pct_NoxiousCover <- function(header, lpi_tall, masterspecieslist, covertype = "absolute"){

  if(!(covertype %in% c("relative", "absolute"))){
    stop("covertype must be 'relative' or 'absolute'.")
  }

  fieldname <- ifelse(covertype == "relative", "RelativeNoxiousCover", "AH_NoxiousCover")

  masterspecieslist <- masterspecieslist%>%
    dplyr::select(Symbol,
                  Scientific.Name,
                  Species,
                  GrowthHabitSub,
                  Duration,
                  NativeStatus,
                  ends_with("_WetStatus"),
                  ends_with("_C.Value"),
                  ends_with("_Nox"))

  header <- header%>%
    dplyr::select(EvaluationID,
                  SpeciesState)%>%
    {if("sf" %in% class(header))sf::st_drop_geometry(.) else .}

  #join lpi_tall to species list then add column for checking whether the species is considered Noxious.
  #Filter the list for relative cover to only include plants identified to species.
  lpispeciesjoin <- dplyr::left_join(header, lpi_tall, by = "EvaluationID")%>%
    dplyr::left_join(., masterspecieslist, by = c("code" = "Symbol"))%>%
    mutate(Noxious = "")%>%
    {if(covertype == "relative") dplyr::filter(.,Species !=""|is.na(Species)) else .}

  #Fill in the Noxious column based on the state data was collected.
  for (i in 1:nrow(lpispeciesjoin)){
    noxiouslist <- paste(lpispeciesjoin$SpeciesState[i], "_NOX", sep = "")
    statenoxious <- lpispeciesjoin[,noxiouslist][i]
    lpispeciesjoin$Noxious[i] <- ifelse(statenoxious != "", "Noxious", "")
  }

  NoxiousCover <- pct_cover_lentic(lpispeciesjoin,
                                      tall = TRUE,
                                      hit = switch(covertype,
                                                   "relative" = "all",
                                                   "absolute" = "any"),
                                      by_line = FALSE,
                                      Noxious)%>%
    dplyr::filter(grepl("\\.NOXIOUS$", metric))%>%
    dplyr::group_by(PlotID, EvaluationID)%>%
    dplyr::summarize(!!fieldname := round(sum(percent), digits = 2))

  return(NoxiousCover)
}

#'@export pct_HydrophyteCover
#'@rdname Cover_Metrics
pct_HydrophyteCover <- function(header, lpi_tall, masterspecieslist, covertype = "absolute"){

  if(!(covertype %in% c("relative", "absolute"))){
    stop("covertype must be 'relative' or 'absolute'.")
  }

  fieldname <- ifelse(covertype == "relative", "RelativeHydrophyteCover", "AH_HydrophyteCover")

  masterspecieslist <- masterspecieslist%>%
    dplyr::select(Symbol,
                  Scientific.Name,
                  Species,
                  GrowthHabitSub,
                  Duration,
                  NativeStatus,
                  ends_with("_WetStatus"),
                  ends_with("_C.Value"),
                  ends_with("_Nox")
    )%>%
    #Change all species without an indicator status to Not Rated so they will be included in calculation
    dplyr::mutate(AW_WetStatus = ifelse(Species!=""&Duration != "Nonvascular"&AW_WetStatus=="","NR",AW_WetStatus),
                  WMVC_WetStatus = ifelse(Species!=""&Duration != "Nonvascular"&WMVC_WetStatus=="","NR", WMVC_WetStatus),
                  GP_WetStatus = ifelse(Species!=""&Duration != "Nonvascular"&GP_WetStatus=="","NR", GP_WetStatus),
                  AK_WetStatus = ifelse(Species!=""&Duration != "Nonvascular"&AK_WetStatus=="","NR", AK_WetStatus))

  header <- header%>%
    dplyr::select(EvaluationID,
                  SpeciesState,
                  WetlandIndicatorRegion)%>%
    {if("sf" %in% class(header))sf::st_drop_geometry(.) else .}

  #join lpi_tall to species list then add column that shows the wetland indicator status of the region, then
  #combine OBL and FACW species into one category.
  lpispeciesjoin <- dplyr::left_join(header, lpi_tall, by = "EvaluationID")%>%
    dplyr::left_join(., masterspecieslist, by = c("code" = "Symbol"))%>%
    mutate(Hydro = case_when(WetlandIndicatorRegion=="Arid West" ~AW_WetStatus,
                             WetlandIndicatorRegion=="Western Mountains, Valleys, and Coast" ~WMVC_WetStatus,
                             WetlandIndicatorRegion=="Great Plains" ~GP_WetStatus,
                             WetlandIndicatorRegion=="Alaska" ~AK_WetStatus,
                             TRUE ~ "REGIONMISSING"))%>%
    mutate(Hydro = ifelse(grepl("FACW|OBL", Hydro), "Hydro", Hydro))%>%
    {if(covertype == "relative") dplyr::filter(.,Hydro !=""|is.na(Hydro)) else .}


  HydrophyteCover <- pct_cover_lentic(lpispeciesjoin,
                                                 tall = TRUE,
                                                 hit = switch(covertype,
                                                              "relative" = "all",
                                                              "absolute" = "any"),
                                                 by_line = FALSE,
                                                 Hydro)%>%
    dplyr::filter(grepl("\\.HYDRO$", metric))%>%
    dplyr::group_by(PlotID, EvaluationID)%>%
    dplyr::summarize(!!fieldname := round(sum(percent), digits = 2))

  return(HydrophyteCover)
}

#'@export pct_HydroFACCover
#'@rdname Cover_Metrics
pct_HydroFACCover <- function(header, lpi_tall, masterspecieslist, covertype = "absolute"){

  if(!(covertype %in% c("relative", "absolute"))){
    stop("covertype must be 'relative' or 'absolute'.")
  }

  fieldname <- ifelse(covertype == "relative", "RelativeHydroFACCover", "AbsoluteHydroFACCover")

  masterspecieslist <- masterspecieslist%>%
    dplyr::select(Symbol,
                  Scientific.Name,
                  Species,
                  GrowthHabitSub,
                  Duration,
                  NativeStatus,
                  ends_with("_WetStatus"),
                  ends_with("_C.Value"),
                  ends_with("_Nox")
    )%>%

    #Change all species without an indicator status to Not Rated so they will be included in calculation
    dplyr::mutate(AW_WetStatus = ifelse(Species!=""&Duration != "Nonvascular"&AW_WetStatus=="","NR",AW_WetStatus),
                  WMVC_WetStatus = ifelse(Species!=""&Duration != "Nonvascular"&WMVC_WetStatus=="","NR", WMVC_WetStatus),
                  GP_WetStatus = ifelse(Species!=""&Duration != "Nonvascular"&GP_WetStatus=="","NR", GP_WetStatus),
                  AK_WetStatus = ifelse(Species!=""&Duration != "Nonvascular"&AK_WetStatus=="","NR", AK_WetStatus))

  header <- header%>%
    dplyr::select(EvaluationID,
                  SpeciesState,
                  WetlandIndicatorRegion)%>%
    {if("sf" %in% class(header))sf::st_drop_geometry(.) else .}

  #join lpi_tall to species list then add column that shows the wetland indicator status of the region, then
  #combine OBL and FACW species into one category.
  lpispeciesjoin <- dplyr::left_join(header, lpi_tall, by = "EvaluationID")%>%
    dplyr::left_join(., masterspecieslist, by = c("code" = "Symbol"))%>%
    mutate(HydroFAC = case_when(WetlandIndicatorRegion=="Arid West" ~AW_WetStatus,
                                WetlandIndicatorRegion=="Western Mountains, Valleys, and Coast" ~WMVC_WetStatus,
                                WetlandIndicatorRegion=="Great Plains" ~GP_WetStatus,
                                WetlandIndicatorRegion=="Alaska" ~AK_WetStatus,
                                TRUE ~ "REGIONMISSING"))%>%
    mutate(HydroFAC = ifelse(grepl("FAC$|FACW|OBL", HydroFAC), "HydroFAC", HydroFAC))%>%
    {if(covertype == "relative") dplyr::filter(.,HydroFAC !=""|is.na(HydroFAC)) else .}

  HydroFACCover <- pct_cover_lentic(lpispeciesjoin,
                                              tall = TRUE,
                                              hit = switch(covertype,
                                                           "relative" = "all",
                                                           "absolute" = "any"),
                                              by_line = FALSE,
                                              HydroFAC)%>%
    dplyr::filter(grepl("\\.HYDROFAC$", metric))%>%
    dplyr::group_by(PlotID, EvaluationID)%>%
    dplyr::summarize(!!fieldname := round(sum(percent), digits = 2))

  return(HydroFACCover)
}

#'@export pct_GrowthHabitCover
#'@rdname Cover_Metrics
pct_GrowthHabitCover <- function(lpi_tall, masterspecieslist, covertype = "absolute", unknowncodes){

  if(!(covertype %in% c("relative", "absolute"))){
    stop("covertype must be 'relative' or 'absolute'.")
  }

  fieldname <- ifelse(covertype == "relative", "Relative", "AH_")

  masterspecieslist <- masterspecieslist%>%
    dplyr::select(Symbol,
                  Scientific.Name,
                  Species,
                  GrowthHabitSub,
                  Duration
    )

  #join lpi_tall to species list. Remove plant hits with no GrowthHabit specified for relative cover.
    #These plant hits would be included in the denominator of the calculation if left in.
  lpispeciesjoin <- dplyr::left_join(lpi_tall, masterspecieslist, by = c("code" = "Symbol"))

  #If a unknown code list is also specified, we can use this list to fill in missing growth habits.
  if(!missing(unknowncodes)){
      lpispeciesjoin <- dplyr::left_join(lpispeciesjoin,
                                         dplyr::rename(unknowncodes, DurationUnknown = Duration),
                                         by = c("PlotID", "EvaluationID", "UnknownCodeKey"))%>%
        dplyr::mutate(GrowthHabitSub = ifelse(GrowthHabitSub=="", GrowthHabit,GrowthHabitSub))}

  #Then filter out any blank values where GrowthHabitSub == "". This is only necessary for relative cover calculations
  lpispeciesjoin <- lpispeciesjoin%>%
    {if(covertype == "relative") dplyr::filter(., GrowthHabitSub !=""&!is.na(GrowthHabitSub)) else .}

  #Run pct_cover_lentic, then rename metrics to title case.
  #Remove AbsoluteCover from the data frame to take out nulls.
  #pivot to show in wide format by EvaluationID
  GrowthHabitCover <- pct_cover_lentic(lpispeciesjoin,
                                    tall = TRUE,
                                    hit = switch(covertype,
                                                 "relative" = "all",
                                                 "absolute" = "any"),
                                    by_line = FALSE,
                                    GrowthHabitSub)%>%
    dplyr::mutate(metric = paste(fieldname, stringr::str_to_title(stringr::str_replace(metric, "Relative\\.|Absolute\\.", "")), "Cover", sep = ""))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::mutate(percent = round(percent, digits = 2))%>%
    tidyr::pivot_wider(names_from = metric, values_from = percent)

  return(GrowthHabitCover)
}

#'@export pct_DurationCover
#'@rdname Cover_Metrics
pct_DurationCover <- function(lpi_tall, masterspecieslist, covertype = "absolute", unknowncodes){

  if(!(covertype %in% c("relative", "absolute"))){
    stop("covertype must be 'relative' or 'absolute'.")
  }

  fieldname <- ifelse(covertype == "relative", "Relative", "AH_")

  masterspecieslist <- masterspecieslist%>%
    dplyr::select(Symbol,
                  Scientific.Name,
                  Species,
                  GrowthHabitSub,
                  Duration
    )

  #join lpi_tall to species list. Remove plant hits with no GrowthHabit specified for relative cover.
  #These plant hits would be included in the denominator of the calculation if left in.
  lpispeciesjoin <- dplyr::left_join(lpi_tall, masterspecieslist, by = c("code" = "Symbol"))

  #If a unknown code list is also specified, we can use this list to fill in missing growth habits.
  if(!missing(unknowncodes)){
    lpispeciesjoin <- dplyr::left_join(lpispeciesjoin,
                                       dplyr::rename(unknowncodes, DurationUnknown = Duration),
                                       by = c("PlotID", "EvaluationID", "UnknownCodeKey"))%>%
      dplyr::mutate(Duration = ifelse(Duration=="", DurationUnknown, Duration))
    }

  #Then filter out any blank values where Duration == "". This is only necessary for relative cover calculations. For Absolute
  #cover, I can't remove empty values, because it'll throw off the number of pindrops.
  lpispeciesjoin <- lpispeciesjoin%>%
    {if(covertype == "relative") dplyr::filter(., Duration !=""&!is.na(Duration)) else .}

  #Run pct_cover_lentic, then rename metrics to title case.
  #Remove AbsoluteCover from the data frame to take out nulls.
  #pivot to show in wide format by EvaluationID
  DurationCover <- pct_cover_lentic(lpispeciesjoin,
                                       tall = TRUE,
                                       hit = switch(covertype,
                                                    "relative" = "all",
                                                    "absolute" = "any"),
                                       by_line = FALSE,
                                       Duration)%>%
    dplyr::mutate(metric = paste(fieldname, stringr::str_to_title(stringr::str_replace(metric, "Relative\\.|Absolute\\.", "")), "Cover", sep = ""))%>%
    dplyr::filter(grepl("Perennial|Annual", metric))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::mutate(percent = round(percent, digits = 2))%>%
    tidyr::pivot_wider(names_from = metric, values_from = percent)

  return(DurationCover)
}

#'@export pct_DurationGrowthHabitCover
#'@rdname Cover_Metrics
pct_DurationGrowthHabitCover <- function(lpi_tall, masterspecieslist, covertype = "absolute", unknowncodes){

  if(!(covertype %in% c("relative", "absolute"))){
    stop("covertype must be 'relative' or 'absolute'.")
  }

  fieldname <- ifelse(covertype == "relative", "Relative", "AH_")

  masterspecieslist <- masterspecieslist%>%
    dplyr::select(Symbol,
                  Scientific.Name,
                  Species,
                  GrowthHabitSub,
                  Duration,
                  PreferredForb
    )

  #join lpi_tall to species list. Remove plant hits with no GrowthHabit specified for relative cover.
  #These plant hits would be included in the denominator of the calculation if left in.
  lpispeciesjoin <- dplyr::left_join(lpi_tall, masterspecieslist, by = c("code" = "Symbol"))

  #If a unknown code list is also specified, we can use this list to fill in missing growth habits.
  if(!missing(unknowncodes)){
    lpispeciesjoin <- dplyr::left_join(lpispeciesjoin,
                                       dplyr::rename(unknowncodes, DurationUnknown = Duration),
                                       by = c("PlotID", "EvaluationID", "UnknownCodeKey"))%>%
      dplyr::mutate(Duration = ifelse(Duration=="", DurationUnknown, Duration),
                    GrowthHabitSub = ifelse(GrowthHabitSub=="", GrowthHabit,GrowthHabitSub))
  }

  #Then filter out any blank values where Duration == "". This is only necessary for relative cover calculations. For Absolute
  #cover, I can't remove empty values, because it'll throw off the number of pindrops.
  lpispeciesjoin <- lpispeciesjoin%>%
    {if(covertype == "relative") dplyr::filter(., Duration !=""&!is.na(Duration)&GrowthHabitSub !=""&!is.na(GrowthHabitSub)) else .}

  #Run pct_cover_lentic, then rename metrics to title case.
  #Remove AbsoluteCover from the data frame to take out nulls.
  #pivot to show in wide format by EvaluationID
  DurationGrowthCover <- pct_cover_lentic(lpispeciesjoin,
                                    tall = TRUE,
                                    hit = switch(covertype,
                                                 "relative" = "all",
                                                 "absolute" = "any"),
                                    by_line = FALSE,
                                    Duration,GrowthHabitSub)%>%
    dplyr::mutate(metric = paste(fieldname,
                                 stringr::str_replace_all(
                                   stringr::str_to_title(
                                     stringr::str_replace_all(metric, c("\\." = " ", "Relative|Absolute" = ""))),
                                   " ", ""),
                                 "Cover", sep = ""))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::mutate(percent = round(percent, digits = 2))%>%
    tidyr::pivot_wider(names_from = metric, values_from = percent)

  return(DurationGrowthCover)
}

#'@export pct_PreferredForbCover
#'@rdname Cover_Metrics
pct_PreferredForbCover <- function(lpi_tall, masterspecieslist, covertype = "absolute"){

  if(!(covertype %in% c("relative", "absolute"))){
    stop("covertype must be 'relative' or 'absolute'.")
  }

  fieldname <- ifelse(covertype == "relative", "Relative", "AH_")

  masterspecieslist <- masterspecieslist%>%
    dplyr::select(Symbol,
                  Scientific.Name,
                  Species,
                  PreferredForb
    )

  #join lpi_tall to species list. Remove plant hits with no GrowthHabit specified for relative cover.
  #These plant hits would be included in the denominator of the calculation if left in.
  lpispeciesjoin <- dplyr::left_join(lpi_tall, masterspecieslist, by = c("code" = "Symbol"))

  #Run pct_cover_lentic, then rename metrics to title case.
  #Remove AbsoluteCover from the data frame to take out nulls.
  #pivot to show in wide format by EvaluationID
  PreferredForbCover <- pct_cover_lentic(lpispeciesjoin,
                                    tall = TRUE,
                                    hit = switch(covertype,
                                                 "relative" = "all",
                                                 "absolute" = "any"),
                                    by_line = FALSE,
                                    PreferredForb)%>%
    dplyr::mutate(metric = paste(fieldname, stringr::str_replace_all(metric, c("Relative\\.|Absolute\\." = "", "Y" = "PreferredForb")), "Cover", sep = ""))%>%
    dplyr::filter(grepl("PreferredForb", metric))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::mutate(percent = round(percent, digits = 2))%>%
    tidyr::pivot_wider(names_from = metric, values_from = percent)

  return(PreferredForbCover)
}

#'@export pct_NonPlantGroundCover
#'@rdname Cover_Metrics
pct_NonPlantGroundCover <- function(lpi_tall, hit = "any", masterspecieslist){

  if(!(hit %in% c("any", "first", "basal"))){
    stop("hit for non-plant cover must be 'any', 'first', or 'basal'.")
  }

  fieldname <- switch(hit,
                      "any" = "AH_",
                      "first" = "FH_",
                      "basal" = "Surface_")

  #Change Lichens and Mosses ID'd to species back to generic codes.
  if(!missing(masterspecieslist)){
    lpi_tall <- lpi_tall%>%
      dplyr::left_join(.,
                       masterspecieslist%>%
                         dplyr::select(Symbol, GrowthHabitSub),
                       by = c("code" = "Symbol"))%>%
      dplyr::mutate(code = dplyr::case_when(GrowthHabitSub=="Moss"~"M",
                                            GrowthHabitSub=="Lichen"~"LI",
                                            TRUE~code))
  }

  #Add cover category to non-plant calls.
  nonplantcategory <- data.frame(code = c("SL", "TH", "HL", "DL", "WL", "NL", "EL", "M", "AL", "ALGAE", "AE", "LC", "VL", "LI", "W", "OM", "S", "GR", "CB", "ST", "BY", "BR", "R"),
                                 covercategory= c("LitterThatch", "LitterThatch", "LitterThatch", "LitterThatch", "LitterThatch", "LitterThatch", "LitterThatch", "Moss", "Algae", "Algae", "Algae", "Lichen", "Lichen", "Lichen", "Water", "OrganicMaterial", "Soil", "Rock", "Rock", "Rock", "Rock", "Rock", "Rock"))

  #Join LPI to the nonplant category table to create non-plant categories to summarize by in pct_cover
  lpi_tall <- lpi_tall%>%
    dplyr::left_join(., nonplantcategory, by = "code")

  #cover calculation for non-plant cover
  NonPlantCover <- pct_cover_lentic(lpi_tall,
                                         tall = TRUE,
                                         hit = switch(hit,
                                                      "any" = "any",
                                                      "first" = "first",
                                                      "basal" = "basal"),
                                         by_line = FALSE,
                                         covercategory)%>%
    dplyr::mutate(metric = paste(fieldname, stringr::str_to_title(stringr::str_replace_all(metric, c("Relative\\.|Absolute\\." = ""))), "Cover", sep = ""))%>%
    dplyr::mutate(metric = stringr::str_replace_all(metric, c("Litterthatch" = "LitterThatch","Organicmaterial" = "OrganicMaterial")))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::mutate(percent = round(percent, digits = 2))%>%
    tidyr::pivot_wider(names_from = metric, values_from = percent)

  return(NonPlantCover)
}

#'@export pct_AbsoluteSpeciesCover
#'@rdname Cover_Metrics
pct_AbsoluteSpeciesCover <- function(lpi_tall, masterspecieslist){

  #Create nonplantcodesfilter
  nonplantcodesfilter <- paste(paste("\\.", nonplantcodes$code, "$", sep = ""), collapse = "|")

  #select necessary columns in species list
  masterspecieslist <- masterspecieslist %>%
    dplyr::select(Symbol, Scientific.Name,Common.Name, Species)

  #Remove all unknowncodekeys for species that were identified to species. Use this
  #datatable to calculate cover for unknowns.
  UnknownSpeciesjoin <- dplyr::left_join(lpi_tall, masterspecieslist, by = c("code" = "Symbol"))%>%
    dplyr::mutate(UnknownCodeKey = ifelse(Species!="" & !str_detect(code, "XXXX"), NA, UnknownCodeKey))

  UnknownCodeCover <- pct_cover_lentic(UnknownSpeciesjoin,
                                       tall = TRUE,
                                       hit = "any",
                                       by_line = FALSE,
                                       code, UnknownCodeKey)%>%
    dplyr::filter(percent > 0)%>%
    dplyr::group_by(EvaluationID)%>%
    tidyr::separate(metric, into = c("Absolute", "Code", "UnknownCodeKey"), sep = "\\.")%>%
    dplyr::left_join(., masterspecieslist, by = c("Code" = "Symbol"))

  #Calculate cover for species identified to species, then filter cover to just those species.
  CodeCover <- pct_cover_lentic(lpi_tall,
                                tall = TRUE,
                                hit = "any",
                                by_line = FALSE,
                                code)%>%
    dplyr::filter(!stringr::str_detect(metric, nonplantcodesfilter))%>%
    dplyr::group_by(PlotID, EvaluationID)%>%
    dplyr::mutate(Code = stringr::str_replace(metric, "Absolute.", ""))%>%
    dplyr::left_join(., masterspecieslist, by = c("Code" = "Symbol"))%>%
    dplyr::filter(Species !=""&percent>0)

  #join two cover lists together
  #needs to be done in two steps to keep plants with different unknown codes but the
  #same family/genus codes as other plants.
  Cover_Species <- rbind(UnknownCodeCover, CodeCover)%>%group_by(EvaluationID)%>%
    #dplyr::mutate(PlotID = stringr::str_sub(EvaluationID, start = 6))%>%
    dplyr::select(PlotID,
                  EvaluationID,
                  Code,
                  UnknownCodeKey,
                  Scientific.Name,
                  Common.Name,
                  percent)%>%
    dplyr::arrange(EvaluationID, desc(percent))%>%
    dplyr::rename(SpeciesCover = percent)%>%
    dplyr::mutate(SpeciesCover = round(SpeciesCover, digits = 2))

  return(Cover_Species)

}
