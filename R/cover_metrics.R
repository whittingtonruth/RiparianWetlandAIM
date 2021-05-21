#'Calculate percent cover from LPI
#'
#'Cover metrics return a \code{data.frame} of percent cover of specified categories calculated from
#'gathered LPI data across plots. These defined functions execute the most common cover categories calculated
#'from LPI.
#'
#'@param header source of header data frame.
#'@param lpi_tall source of lpi_tall data frame.
#'@param masterspecieslist Character string. Full file path (including extension) to the file
#'containing the species list.
#'@param covertype Character string. "relative" or "absolute". Specifies the kind of cover calculation.
#'Relative cover is only used for calculations on vascular plant species and specifies the percent of
#'overall hits made up of a particular species or group. Absolute cover is the percent of the pin
#'drops made up by a particular species or group.
#'@return



#'@export pct_FoliarCover
#'@rdname lentic_covermetrics
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
    dplyr::group_by(PlotKey)%>%
    dplyr::summarize(PercentFoliarCover = sum(percent))

  return(PercentFoliarCover)
}

#'@export pct_BasalCover
#'@rdname lentic_covermetrics
pct_BasalCover <- function(lpi_tall){

  #many cover indicators need to be filtered to plant codes only. Use this regex expression to filter:
  nonplantcodesfilter <- paste(paste("\\.", nonplantcodes$code, "$", sep = ""), collapse = "|")

  PercentBasalCover <- pct_cover_lentic(lpi_tall,
                                        tall = TRUE,
                                        hit = "basal",
                                        by_line = FALSE,
                                        code)%>%
    dplyr::filter(!stringr::str_detect(metric, nonplantcodesfilter))%>%
    dplyr::group_by(PlotKey)%>%
    dplyr::summarize(PercentBasalCover = sum(percent))

  return(PercentBasalCover)
}

#'@export pct_TotalAbsoluteCover
#'@rdname lentic_covermetrics
pct_TotalAbsoluteCover <- function(lpi_tall){

  #many cover indicators need to be filtered to plant codes only. Use this regex expression to filter:
  nonplantcodesfilter <- paste(paste("\\.", nonplantcodes$code, "$", sep = ""), collapse = "|")

  TotalAbsoluteCover <- pct_cover_lentic(lpi_tall,
                                          tall = TRUE,
                                          hit = "any",
                                          by_line = FALSE,
                                          code)%>%
    dplyr::filter(!stringr::str_detect(metric, nonplantcodesfilter))%>%
    dplyr::group_by(PlotKey)%>%
    dplyr::summarize(TotalAbsoluteCover = sum(percent))

  return(TotalAbsoluteCover)
}

#'@export pct_NativeCover
#'@rdname lentic_covermetrics
pct_NativeCover <- function(lpi_tall, masterspecieslist, covertype = "relative"){

  if(!(covertype %in% c("relative", "absolute"))){
    stop("covertype must be 'relative' or 'absolute'.")
  }

  fieldname <- ifelse(covertype == "relative", "RelativeNativeCover", "AbsoluteNativeCover")

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
    dplyr::filter(grepl("\\.NATIVE$", metric))%>%
    dplyr::group_by(PlotKey)%>%
    dplyr::summarize(!!fieldname := sum(percent))

  return(NativeCover)
}

#'@export pct_NoxiousCover
#'@rdname lentic_covermetrics
pct_NoxiousCover <- function(header, lpi_tall, masterspecieslist, covertype = "absolute"){

  if(!(covertype %in% c("relative", "absolute"))){
    stop("covertype must be 'relative' or 'absolute'.")
  }

  fieldname <- ifelse(covertype == "relative", "RelativeNoxiousCover", "AbsoluteNoxiousCover")

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
    dplyr::select(PlotKey,
                  AdminState)

  #join lpi_tall to species list then add column for checking whether the species is considered Noxious.
  #Filter the list for relative cover to only include plants identified to species.
  lpispeciesjoin <- dplyr::left_join(header, lpi_tall)%>%
    dplyr::left_join(., masterspecieslist, by = c("code" = "Symbol"))%>%
    mutate(Noxious = "")%>%
    {if(covertype == "relative") dplyr::filter(.,Species !=""|is.na(Species)) else .}

  #Fill in the Noxious column based on the state data was collected.
  for (i in 1:nrow(lpispeciesjoin)){
    noxiouslist <- paste(lpispeciesjoin$AdminState[i], "_NOX", sep = "")
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
    dplyr::group_by(PlotKey)%>%
    dplyr::summarize(!!fieldname := sum(percent))

  return(NoxiousCover)
}

#'@export pct_HydrophyteCover
#'@rdname lentic_covermetrics
pct_HydrophyteCover <- function(header, lpi_tall, masterspecieslist, covertype = "relative"){

  if(!(covertype %in% c("relative", "absolute"))){
    stop("covertype must be 'relative' or 'absolute'.")
  }

  fieldname <- ifelse(covertype == "relative", "RelativeHydrophyteCover", "AbsoluteHydrophyteCover")

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
    dplyr::mutate(AW_WetStatus = ifelse(Species!=""&AW_WetStatus=="","NR",AW_WetStatus))%>%
    dplyr::mutate(WMVC_WetStatus = ifelse(Species!=""&WMVC_WetStatus=="","NR", WMVC_WetStatus))

  header <- header%>%
    dplyr::select(PlotKey,
                  AdminState,
                  Region)

  #join lpi_tall to species list then add column that shows the wetland indicator status of the region, then
  #combine OBL and FACW species into one category.
  lpispeciesjoin <- dplyr::left_join(header, lpi_tall)%>%
    dplyr::left_join(., masterspecieslist, by = c("code" = "Symbol"))%>%
    mutate(Hydro = ifelse(Region=="Arid West", AW_WetStatus, WMVC_WetStatus))%>%
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
    dplyr::group_by(PlotKey)%>%
    dplyr::summarize(!!fieldname := sum(percent))

  return(HydrophyteCover)
}

#'@export pct_HydroFACCover
#'@rdname lentic_covermetrics
pct_HydroFACCover <- function(header, lpi_tall, masterspecieslist, covertype = "relative"){

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
    dplyr::mutate(AW_WetStatus = ifelse(Species!=""&AW_WetStatus=="","NR",AW_WetStatus))%>%
    dplyr::mutate(WMVC_WetStatus = ifelse(Species!=""&WMVC_WetStatus=="","NR", WMVC_WetStatus))

  header <- header%>%
    dplyr::select(PlotKey,
                  AdminState,
                  Region)

  #join lpi_tall to species list then add column that shows the wetland indicator status of the region, then
  #combine OBL and FACW species into one category.
  lpispeciesjoin <- dplyr::left_join(header, lpi_tall)%>%
    dplyr::left_join(., masterspecieslist, by = c("code" = "Symbol"))%>%
    mutate(HydroFAC = ifelse(Region=="Arid West", AW_WetStatus, WMVC_WetStatus))%>%
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
    dplyr::group_by(PlotKey)%>%
    dplyr::summarize(!!fieldname := sum(percent))

  return(HydroFACCover)
}

#'@export pct_GrowthHabitCover
#'@rdname lentic_covermetrics
pct_GrowthHabitCover <- function(lpi_tall, masterspecieslist, unknowncodelist, covertype = "relative"){

  if(!(covertype %in% c("relative", "absolute"))){
    stop("covertype must be 'relative' or 'absolute'.")
  }

  fieldname <- ifelse(covertype == "relative", "Relative", "Absolute")

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

  #join lpi_tall to species list. Remove plant hits with no GrowthHabit specified for relative cover.
    #These plant hits would be included in the denominator of the calculation if left in.
  lpispeciesjoin <- dplyr::left_join(lpi_tall, masterspecieslist, by = c("code" = "Symbol"))%>%
    {if(covertype == "relative") dplyr::filter(., GrowthHabitSub !=""|is.na(GrowthHabitSub)) else .}

  #Run pct_cover_lentic, then rename metrics to title case.
  #Remove AbsoluteCover from the data frame to take out nulls.
  #pivot to show in wide format by PlotKey
  GrowthHabitCover <- pct_cover_lentic(lpispeciesjoin,
                                    tall = TRUE,
                                    hit = switch(covertype,
                                                 "relative" = "all",
                                                 "absolute" = "any"),
                                    by_line = FALSE,
                                    GrowthHabitSub)%>%
    dplyr::mutate(metric = paste(fieldname, stringr::str_to_title(stringr::str_replace(metric, "Relative\\.|Absolute\\.", "")), "Cover", sep = ""))%>%
    dplyr::group_by(PlotKey)%>%
    tidyr::pivot_wider(names_from = metric, values_from = percent)

  return(GrowthHabitCover)
}

#'@export pct_DurationCover
#'@rdname lentic_covermetrics
pct_DurationCover <- function(lpi_tall, masterspecieslist, unknowncodelist, covertype = "relative"){

  if(!(covertype %in% c("relative", "absolute"))){
    stop("covertype must be 'relative' or 'absolute'.")
  }

  fieldname <- ifelse(covertype == "relative", "Relative", "Absolute")

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

  #join lpi_tall to species list. Remove plant hits with no GrowthHabit specified for relative cover.
  #These plant hits would be included in the denominator of the calculation if left in.
  lpispeciesjoin <- dplyr::left_join(lpi_tall, masterspecieslist, by = c("code" = "Symbol"))%>%
    {if(covertype == "relative") dplyr::filter(., Duration !=""|is.na(Duration)) else .}

  #Run pct_cover_lentic, then rename metrics to title case.
  #Remove AbsoluteCover from the data frame to take out nulls.
  #pivot to show in wide format by PlotKey
  DurationCover <- pct_cover_lentic(lpispeciesjoin,
                                       tall = TRUE,
                                       hit = switch(covertype,
                                                    "relative" = "all",
                                                    "absolute" = "any"),
                                       by_line = FALSE,
                                       Duration)%>%
    dplyr::mutate(metric = paste(fieldname, stringr::str_to_title(stringr::str_replace(metric, "Relative\\.|Absolute\\.", "")), "Cover", sep = ""))%>%
    dplyr::group_by(PlotKey)%>%
    tidyr::pivot_wider(names_from = metric, values_from = percent)

  return(DurationCover)
}

#'@export pct_NonPlantGroundCover
#'@rdname lentic_covermetrics
pct_NonPlantGroundCover <- function(lpi_tall, hit = "any"){

  if(!(hit %in% c("any", "basal"))){
    stop("hit for non-plant cover must be 'any' or 'basal'.")
  }

  fieldname <- ifelse(hit == "any", "Total", "Surface")

  #many cover indicators need to be filtered to plant codes only. Use this regex expression to filter:
  nonplantcategory <- data.frame(code = c("TH", "HL", "DL", "WL", "NL", "EL", "M", "W", "OM", "S"),
                                 covercategory= c("Litter", "Litter", "Litter", "Litter", "Litter", "Litter", "Moss", "Water", "Organic Material", "Soil"))

  #Join LPI to the nonplant category table to create non-plant categories to summarize by in pct_cover
  lpi_tall <- lpi_tall%>%
    dplyr::left_join(., nonplantcategory)

  #cover calculation for non-plant cover
  NonPlantCover <- pct_cover_lentic(lpi_tall,
                                         tall = TRUE,
                                         hit = switch(hit,
                                                      "any" = "any",
                                                      "basal" = "basal"),
                                         by_line = FALSE,
                                         covercategory)%>%
    dplyr::mutate(metric = paste(fieldname, stringr::str_to_title(stringr::str_replace(metric, "Relative\\.|Absolute\\.", "")), "Cover", sep = ""))%>%
    dplyr::group_by(PlotKey)%>%
    tidyr::pivot_wider(names_from = metric, values_from = percent)

  return(NonPlantCover)
}


#'@export Combine2019Indicators
#'@rdname lentic_covermetrics
Combine2019Indicators <- function(header, lpi_tall, masterspecieslist, unknownplantslist){

  Foliar <- pct_FoliarCover(lpi_tall)

  Basal <- pct_BasalCover(lpi_tall)

  TotalAbsolute <- pct_TotalAbsoluteCover(lpi_tall)

  RelativeNative <- pct_NativeCover(lpi_tall, masterspecieslist, covertype = "relative")

  AbsoluteNoxious <- pct_NoxiousCover(header, lpi_tall, masterspecieslist, covertype = "absolute")

  RelativeHydro <- pct_HydrophyteCover(header, lpi_tall, masterspecieslist, covertype = "relative")

  RelativeHydroFAC <- pct_HydroFACCover(header, lpi_tall, masterspecieslist, covertype = "relative")

  RelativeGrowthHabit <- pct_GrowthHabitCover(lpi_tall, masterspecieslist, unknownplantlist, covertype = "relative")

  RelativeDuration <- pct_DurationCover(lpi_tall, masterspecieslist, unknownplantlist, covertype = "relative")

  NonPlantCover <- pct_NonPlantGroundCover(lpi_tall, hit = "any")

  LPI_Cover_Indicators <- Foliar %>% dplyr::left_join(., Basal)%>%
    dplyr::left_join(., TotalAbsolute)%>%
    dplyr::left_join(., RelativeNative)%>%
    dplyr::left_join(., AbsoluteNoxious)%>%
    dplyr::left_join(., RelativeHydro)%>%
    dplyr::left_join(., RelativeHydroFAC)%>%
    dplyr::left_join(., RelativeGrowthHabit)%>%
    dplyr::left_join(., RelativeDuration)%>%
    dplyr::left_join(., NonPlantCover)

  return(LPI_Cover_Indicators)
}
