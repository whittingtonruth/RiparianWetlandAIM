#'Summarize community composition for specified grouping variables.
#'
#'@description This group of functions prepares a species list to be used in the embedded
#'\code{community_composition()}. This can involve filtering duplicate species, combining
#'grouping variable categories, and pulling plot information from plot headers to inform
#'species classification. Analogous to \code{cover_metrics} functions.
#'
#'@param header Data frame. Use the data frame from the \code{header_build_lentic()} output. Used in Noxious and
#'Wetland Indicator calculations to specify the plot WetlandIndicatorRegion or state.
#'@param SpeciesList Data frame. Table of species by PlotID that will be summarized. Can be either spp_inventory_tall
#'produced by \code{gather_spp_inventory_lentic()} or lpi_tall produced by \code{gather_lpi_lentic()}.
#'@param masterspecieslist Data frame. The centrally managed master species list should be used.
#'@param listtype Character string. Indicates the source of the SpeciesList provided. Can either be
#'"speciesinventory" or"lpi". Defaults to "speciesinventory".
#'@param method character string. The method used for the produced summary table. Can
#'be "percent", "mean", or "count". Included in functions where applicable.
#'@returns Data frame of summary metrics by plot.

#'@export Community_Richness
#'@rdname Community_Metrics
Community_Richness <- function(SpeciesList, masterspecieslist, listtype = "speciesinventory"){

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  #assign a field name based on source of data.
  fieldname <- paste("Community", ifelse(listtype == "speciesinventory", "SppInv", "LPI"), "Richness", sep = "")

  #Remove non-plant codes if using LPI.
  if(listtype == "lpi"){
    SpeciesList <- SpeciesList %>%
      dplyr::rename(Species = code)%>%
      dplyr::filter(!(Species %in% nonplantcodes$code) & layer != "SoilSurface")
  }

  #join provided species list to master list and filter out duplicates.
  SpeciesList <- SpeciesList%>%
    dplyr::left_join(., masterspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(EvaluationID)%>%
    #filter duplicate "Species" if the species is known. Filter out duplicate unknowns if the code and the unknowncodekey are the same.
    dplyr::filter(!(duplicated(UnknownCodeKey) & Species.y %in% c(NA, "")) &
                    !(duplicated(Species) & !(Species.y %in% c(NA, ""))))

  totals <- Community_Composition(SpeciesList, method = "count")%>%
    dplyr::rename(!!fieldname := count)

  return(totals)
}

#'@export Community_C.Value
#'@rdname Community_Metrics
Community_C.Value <- function(header, SpeciesList, masterspecieslist, listtype = "speciesinventory"){

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  #assign fieldname based on source of the species list data.
  fieldname <- paste("Community", ifelse(listtype == "speciesinventory", "SppInv", "LPI"), "MeanC.Value", sep = "")

  #Remove non-plant codes if using LPI.
  if(listtype == "lpi"){
    SpeciesList <- SpeciesList %>%
      dplyr::rename(Species = code)%>%
      dplyr::filter(!(Species %in% nonplantcodes$code) & layer != "SoilSurface")
  }

  #Join header, specieslist, and master species list together and filter out unknowns. Add C.Value column to add state-specific C-Values
  SpeciesList <- dplyr::left_join(header, SpeciesList, by = "EvaluationID")%>%
    dplyr::left_join(., masterspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & Species.y %in% c(NA, "")) &
                    !(duplicated(Species) & !(Species.y %in% c(NA, ""))))%>%
    dplyr::select(EvaluationID, SpeciesState, Species, ends_with("_C.Value"))%>%
    tibble::add_column(., C.Value = NA)

  #Populate C-Value data based on SpeciesState from header.
  for (i in 1:nrow(SpeciesList)){
    C.Valuelist <- paste(SpeciesList$SpeciesState[i], "_C.Value", sep = "")
    StateC.Value <- SpeciesList[[i,C.Valuelist]]
    SpeciesList$C.Value[i] <- StateC.Value
  }

  totals <- Community_Composition(SpeciesList, method = "mean", tall = T, C.Value)%>%
    dplyr::rename(!!fieldname := average)

  return(totals)
}

#'@export Community_Native
#'@rdname Community_Metrics
Community_Native <- function(SpeciesList, masterspecieslist, listtype = "speciesinventory", method = "percent"){

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  if(!(method %in% c("percent", "count"))){
    stop("Method must be 'percent' or 'count'.")
  }

  #Create fieldname based on source of data and kind of calculation.
  fieldname <- paste("Community", ifelse(listtype == "speciesinventory", "SppInv", "LPI"), ifelse(method == "percent", "Pct", "Count"), sep = "")

  #Remove non-plant codes if using LPI.
  if(listtype == "lpi"){
    SpeciesList <- SpeciesList %>%
      dplyr::rename(Species = code)%>%
      dplyr::filter(!(Species %in% nonplantcodes$code) & layer != "SoilSurface")
  }

  masterspecieslist$NativeStatus[masterspecieslist$NativeStatus=="cryptogenic"] <- "Nonnative"

  #Join specieslist to master species list and filter to relevant entries.
  SpeciesList <- SpeciesList%>%
    dplyr::left_join(., masterspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & Species.y %in% c(NA, "")) &
                    !(duplicated(Species) & !(Species.y %in% c(NA, ""))))%>%
    dplyr::filter(NativeStatus != "" & !is.na(NativeStatus))%>%
    dplyr::select(EvaluationID, Species, NativeStatus)

  #Calculate community metrics and rename columns.
  totals <- Community_Composition(SpeciesList, method = method, tall = T, NativeStatus)%>%
    dplyr::filter(grepl("NATIVE$", metric))%>%
    dplyr::mutate(metric = paste(fieldname, stringr::str_to_title(stringr::str_replace(metric, "Percent\\.|Count\\.", "")), sep = ""))%>%
    dplyr::group_by(EvaluationID)%>%
    tidyr::pivot_wider(names_from = metric,
                       values_from = {ifelse(method == "percent",
                                             expr(percent),
                                             expr(count))})

  return(totals)
}

#'@export Community_NoxiousCount
#'@rdname Community_Metrics
Community_NoxiousCount <- function(header, SpeciesList, masterspecieslist, listtype = "speciesinventory"){

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  #create fieldnames based on source of species data.
  fieldname <- paste("Community", ifelse(listtype == "speciesinventory", "SppInv", "LPI"), "CountNoxious", sep = "")

  #Remove non-plant codes if using LPI.
  if(listtype == "lpi"){
    SpeciesList <- SpeciesList %>%
      dplyr::rename(Species = code)%>%
      dplyr::filter(!(Species %in% nonplantcodes$code) & layer != "SoilSurface")
  }

  #join header, specieslist and master species list together, then filter out duplicates and unnecessary columns.
  SpeciesList <- dplyr::left_join(header, SpeciesList, by = "EvaluationID")%>%
    dplyr::left_join(., masterspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & Species.y %in% c(NA, "")) &
                    !(duplicated(Species) & !(Species.y %in% c(NA, ""))))%>%
    dplyr::select(EvaluationID, SpeciesState, Species, ends_with("_NOX"))%>%
    tibble::add_column(., Noxious = NA)

  #Populate noxious column data based on SpeciesState found in Header.
  for (i in 1:nrow(SpeciesList)){
    noxiouslist <- paste(SpeciesList$SpeciesState[i], "_NOX", sep = "")
    statenoxious <- SpeciesList[[i,noxiouslist]]
    SpeciesList$Noxious[i] <- ifelse(statenoxious != "" & !is.na(statenoxious), "Noxious", "")
  }

  #Calculate community metrics and rename columns.
  totals <- Community_Composition(SpeciesList, method = "count", tall = T, Noxious)%>%
    dplyr::filter(grepl("\\.NOXIOUS$", metric))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::summarize(!!fieldname := count)

  return(totals)
}

#'@export Community_Hydrophytes
#'@rdname Community_Metrics
Community_Hydrophytes <- function(header, SpeciesList, masterspecieslist, listtype = "speciesinventory", method = "percent"){

  if(!(method %in% c("percent", "count"))){
    stop("Method must be 'percent' or 'count'.")
  }

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  #Create fieldname based on source of species list and method used to calculate metrics
  fieldname <- paste("Community", ifelse(listtype == "speciesinventory", "SppInv", "LPI"), ifelse(method == "percent", "Pct", "Count"), "Hydrophyte", sep = "")

  masterspecieslist <- masterspecieslist%>%
    dplyr::select(Symbol,
                  Scientific.Name,
                  Species,
                  ends_with("_WetStatus")
    )%>%
    #Change all species without an indicator status to Not Rated so they will be included in calculation
    dplyr::mutate(AW_WetStatus = ifelse(Species!=""&AW_WetStatus=="","NR",AW_WetStatus),
                  WMVC_WetStatus = ifelse(Species!=""&WMVC_WetStatus=="","NR", WMVC_WetStatus),
                  GP_WetStatus = ifelse(Species!=""&GP_WetStatus=="","NR", GP_WetStatus))

  #If using LPI, change the code column to Species, then remove all nonplant codes.
  if(listtype == "lpi"){
    SpeciesList <- SpeciesList %>%
      dplyr::rename(Species = code)%>%
      dplyr::filter(!(Species %in% nonplantcodes$code) & layer != "SoilSurface")
  }

  #Join the header, SpeciesList, and master species list together, then:
  #1. Group by plot
  #2. Filter all duplicated species and all duplicated unknowns. This is particularly important for LPI
  #3. Add a Hydro column that pushes forward the right Wetland Status based on the header region.
  #4. Combine wetland statuses you're interested in.
  #5. Remove NA's from hydro that will be any unknown without an indicator status.
  #6. Keep interesting columns.
  SpeciesList <- dplyr::left_join(header, SpeciesList, by = "EvaluationID")%>%
    dplyr::left_join(., masterspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & Species.y %in% c(NA, "")) &
                    !(duplicated(Species) & !(Species.y %in% c(NA, ""))))%>%
    dplyr::mutate(Hydro = case_when(WetlandIndicatorRegion=="Arid West" ~AW_WetStatus,
                                    WetlandIndicatorRegion=="Western Mountains, Valleys, and Coast" ~WMVC_WetStatus,
                                    WetlandIndicatorRegion=="Great Plains" ~GP_WetStatus,
                                    TRUE ~ "REGIONMISSING"))%>%
    dplyr::mutate(Hydro = ifelse(grepl("FACW|OBL", Hydro), "Hydro", Hydro))%>%
    dplyr::filter(.,Hydro !=""|is.na(Hydro))%>%
    dplyr::select(EvaluationID,
                  Species,
                  Hydro)

  totals <- Community_Composition(SpeciesList, method = method, tall = T, Hydro)%>%
    dplyr::filter(grepl("\\.HYDRO$", metric))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::summarize(!!fieldname := {ifelse(method == "percent",
                                            percent,
                                            count)})

  return(totals)
}

#'@export Community_HydroFAC
#'@rdname Community_Metrics
Community_HydroFAC <- function(header, SpeciesList, masterspecieslist, listtype = "speciesinventory", method = "percent"){

  if(!(method %in% c("percent", "count"))){
    stop("Method must be 'percent' or 'count'.")
  }

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  #Create fieldname based on source of species list and method used to calculate metrics
  fieldname <- paste("Community", ifelse(listtype == "speciesinventory", "SppInv", "LPI"), ifelse(method == "percent", "Pct", "Count"), "HydroFAC", sep = "")

  masterspecieslist <- masterspecieslist%>%
    dplyr::select(Symbol,
                  Scientific.Name,
                  Species,
                  ends_with("_WetStatus")
    )%>%
    #Change all species without an indicator status to Not Rated so they will be included in calculation
    dplyr::mutate(AW_WetStatus = ifelse(Species!=""&AW_WetStatus=="","NR",AW_WetStatus),
                  WMVC_WetStatus = ifelse(Species!=""&WMVC_WetStatus=="","NR", WMVC_WetStatus),
                  GP_WetStatus = ifelse(Species!=""&GP_WetStatus=="","NR", GP_WetStatus))

  #If using LPI, change the code column to Species, then remove all nonplant codes.
  if(listtype == "lpi"){
    SpeciesList <- SpeciesList %>%
      dplyr::rename(Species = code)%>%
      dplyr::filter(!(Species %in% nonplantcodes$code) & layer != "SoilSurface")
  }

  #Join the header, SpeciesList, and master species list together, then:
  #1. Group by plot
  #2. Filter all duplicated species and all duplicated unknowns. This is particularly important for LPI
  #3. Add a Hydro column that pushes forward the right Wetland Status based on the header WetlandIndicatorRegion.
  #4. Combine wetland statuses you're interested in.
  #5. Remove NA's from hydro that will be any unknown without an indicator status.
  #6. Keep interesting columns.
  SpeciesList <- dplyr::left_join(header, SpeciesList, by = "EvaluationID")%>%
    dplyr::left_join(., masterspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & Species.y %in% c(NA, "")) &
                    !(duplicated(Species) & !(Species.y %in% c(NA, ""))))%>%
    dplyr::mutate(HydroFAC = case_when(WetlandIndicatorRegion=="Arid West" ~AW_WetStatus,
                                       WetlandIndicatorRegion=="Western Mountains, Valleys, and Coast" ~WMVC_WetStatus,
                                       WetlandIndicatorRegion=="Great Plains" ~GP_WetStatus,
                                       TRUE ~ "REGIONMISSING"))%>%
    dplyr::mutate(HydroFAC = ifelse(grepl("FAC$|FACW|OBL", HydroFAC), "HydroFAC", HydroFAC))%>%
    dplyr::filter(.,HydroFAC !=""|is.na(HydroFAC))%>%
    dplyr::select(EvaluationID,
                  Species,
                  HydroFAC)

  totals <- Community_Composition(SpeciesList, method = method, tall = T, HydroFAC)%>%
    dplyr::filter(grepl("\\.HYDROFAC$", metric))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::summarize(!!fieldname := {ifelse(method == "percent",
                                            percent,
                                            count)})

  return(totals)
}

#'@export Community_GrowthHabit
#'@rdname Community_Metrics
Community_GrowthHabit <- function(SpeciesList, masterspecieslist, listtype = "speciesinventory", method = "percent"){

  if(!(method %in% c("percent", "count"))){
    stop("Method must be 'percent' or 'count'.")
  }

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  fieldname <- paste("Community", ifelse(listtype == "speciesinventory", "SppInv", "LPI"), ifelse(method == "percent", "Pct", "Count"), sep = "")

  #If using LPI, change the code column to Species, then remove all nonplant codes.
  if(listtype == "lpi"){
    SpeciesList <- SpeciesList %>%
      dplyr::rename(Species = code)%>%
      dplyr::filter(!(Species %in% nonplantcodes$code) & layer != "SoilSurface")
  }

  #Join the SpeciesList and master species list together.
  SpeciesList <- SpeciesList%>%
    dplyr::left_join(., masterspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & Species.y %in% c(NA, "")) &
                    !(duplicated(Species) & !(Species.y %in% c(NA, ""))))%>%
    dplyr::filter(GrowthHabitSub != "" & !is.na(GrowthHabitSub))

  totals <- Community_Composition(SpeciesList, method = method, tall = T, GrowthHabitSub)%>%
    dplyr::mutate(metric = paste(fieldname, stringr::str_to_title(stringr::str_replace(metric, "Percent\\.|Count\\.", "")), sep = ""))%>%
    dplyr::group_by(EvaluationID)%>%
    tidyr::pivot_wider(names_from = metric, values_from = {ifelse(method == "percent",
                                                                  expr(percent),
                                                                  expr(count))})

  return(totals)
}

#'@export Community_Duration
#'@rdname Community_Metrics
Community_Duration <- function(SpeciesList, masterspecieslist, listtype = "speciesinventory", method = "percent"){

  if(!(method %in% c("percent", "count"))){
    stop("Method must be 'percent' or 'count'.")
  }

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  fieldname <- paste("Community", ifelse(listtype == "speciesinventory", "SppInv", "LPI"), ifelse(method == "percent", "Pct", "Count"), sep = "")

  #If using LPI, change the code column to Species, then remove all nonplant codes.
  if(listtype == "lpi"){
    SpeciesList <- SpeciesList %>%
      dplyr::rename(Species = code)%>%
      dplyr::filter(!(Species %in% nonplantcodes$code) & layer != "SoilSurface")
  }

  #Join the SpeciesList and master species list together.
  SpeciesList <- SpeciesList%>%
    dplyr::left_join(., masterspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & Species.y %in% c(NA, "")) &
                    !(duplicated(Species) & !(Species.y %in% c(NA, ""))))%>%
    dplyr::filter(Duration != "" & !is.na(Duration))

  totals <- Community_Composition(SpeciesList, method = method, tall = T, Duration)%>%
    dplyr::mutate(metric = paste(fieldname, stringr::str_to_title(stringr::str_replace(metric, "Percent\\.|Count\\.", "")), sep = ""))%>%
    dplyr::group_by(EvaluationID)%>%
    tidyr::pivot_wider(names_from = metric, values_from = {ifelse(method == "percent",
                                                                  expr(percent),
                                                                  expr(count))})

  return(totals)
}
