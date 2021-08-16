#'Functions for summarizing community makeup for specific grouping variables.
#'
#'@param header data.frame. Table providing plot-based information on AdminState and Region used in
#'Community Metric calculations.
#'@param SpeciesList List of species by PlotKey that will be summarized. Can be either spp_inventory
#'produced by \code{gather_spp_inventory_lentic} or \code{gather_lpi_lentic}.
#'@param masterspecieslist Dataframe containing all possible species codes with
#'necessary grouping variable information.
#'@param listtype character string. Indicates the source of the SpeciesList provided. Can either be
#'\code{"speciesinventory"} or \code{"lpi"}. Defaults to speciesinventory.
#'@param method character string. The method used for the produced summary table. Can
#'be \code{"percent"}, \code{"mean"}, or {"count"}.

#'@export Community_Richness
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
    dplyr::group_by(PlotKey)%>%
    #filter duplicate "Species" if the species is known. Filter out duplicate unknowns if the code and the unknowncodekey are the same.
    dplyr::filter(!(duplicated(UnknownCodeKey) & Species.y %in% c(NA, "")) &
                    !(duplicated(Species) & !(Species.y %in% c(NA, ""))))

  totals <- Community_Composition(SpeciesList, method = "count")%>%
    dplyr::rename(!!fieldname := count)

  return(totals)
}

#'@export Community_C.Value
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
  SpeciesList <- dplyr::left_join(header, SpeciesList)%>%
    dplyr::left_join(., masterspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(PlotKey)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & Species.y %in% c(NA, "")) &
                    !(duplicated(Species) & !(Species.y %in% c(NA, ""))))%>%
    dplyr::select(PlotKey, AdminState, Species, ends_with("_C.Value"))%>%
    tibble::add_column(., C.Value = NA)

  #Populate C-Value data based on AdminState from header.
  for (i in 1:nrow(SpeciesList)){
    C.Valuelist <- paste(SpeciesList$AdminState[i], "_C.Value", sep = "")
    StateC.Value <- SpeciesList[[i,C.Valuelist]]
    SpeciesList$C.Value[i] <- StateC.Value
  }

  totals <- Community_Composition(SpeciesList, method = "mean", tall = T,C.Value)%>%
    dplyr::rename(!!fieldname := average)

  return(totals)
}

#'@export Community_Native
Community_Native <- function(SpeciesList, masterspecieslist, listtype = "speciesinventory", method = "percent"){

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  if(!(method %in% c("percent", "count"))){
    stop("Method must be 'percent' or 'count'.")
  }

  #Create fieldname based on source of data and kind of calculation.
  fieldname <- paste("Community", ifelse(listtype == "speciesinventory", "SppInv", "LPI"), ifelse(method == "percent", "Pct", "Count"), "Native", sep = "")

  #Remove non-plant codes if using LPI.
  if(listtype == "lpi"){
    SpeciesList <- SpeciesList %>%
      dplyr::rename(Species = code)%>%
      dplyr::filter(!(Species %in% nonplantcodes$code) & layer != "SoilSurface")
  }

  #Join specieslist to master species list and filter to relevant entries.
  SpeciesList <- SpeciesList%>%
    dplyr::left_join(., masterspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(PlotKey)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & Species.y %in% c(NA, "")) &
                    !(duplicated(Species) & !(Species.y %in% c(NA, ""))))%>%
    dplyr::filter(NativeStatus != "" & !is.na(NativeStatus))%>%
    dplyr::select(PlotKey, Species, NativeStatus)

  #Calculate community metrics and rename columns.
  totals <- Community_Composition(SpeciesList, method = method, tall = T, NativeStatus)%>%
    dplyr::filter(grepl("\\.NATIVE$", metric))%>%
    dplyr::group_by(PlotKey)%>%
    dplyr::summarize(!!fieldname := {ifelse(method == "percent",
                                            percent,
                                            count)})

  return(totals)
}

#'@export Community_NoxiousCount
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
  SpeciesList <- dplyr::left_join(header, SpeciesList)%>%
    dplyr::left_join(., masterspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(PlotKey)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & Species.y %in% c(NA, "")) &
                    !(duplicated(Species) & !(Species.y %in% c(NA, ""))))%>%
    dplyr::select(PlotKey, AdminState, Species, ends_with("_NOX"))%>%
    tibble::add_column(., Noxious = NA)

  #Populate noxious column data based on AdminState found in Header.
  for (i in 1:nrow(SpeciesList)){
    noxiouslist <- paste(SpeciesList$AdminState[i], "_NOX", sep = "")
    statenoxious <- SpeciesList[[i,noxiouslist]]
    SpeciesList$Noxious[i] <- ifelse(statenoxious != "" & !is.na(statenoxious), "Noxious", "")
  }

  #Calculate community metrics and rename columns.
  totals <- Community_Composition(SpeciesList, method = "count", tall = T, Noxious)%>%
    dplyr::filter(grepl("\\.NOXIOUS$", metric))%>%
    dplyr::group_by(PlotKey)%>%
    dplyr::summarize(!!fieldname := count)

  return(totals)
}

#'@export Community_Hydrophytes
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
    dplyr::mutate(AW_WetStatus = ifelse(Species!=""&AW_WetStatus=="","NR",AW_WetStatus))%>%
    dplyr::mutate(WMVC_WetStatus = ifelse(Species!=""&WMVC_WetStatus=="","NR", WMVC_WetStatus))

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
  SpeciesList <- dplyr::left_join(header, SpeciesList)%>%
    dplyr::left_join(., masterspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(PlotKey)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & Species.y %in% c(NA, "")) &
                    !(duplicated(Species) & !(Species.y %in% c(NA, ""))))%>%
    dplyr::mutate(Hydro = ifelse(Region=="Arid West", AW_WetStatus, WMVC_WetStatus))%>%
    dplyr::mutate(Hydro = ifelse(grepl("FACW|OBL", Hydro), "Hydro", Hydro))%>%
    dplyr::filter(.,Hydro !=""|is.na(Hydro))%>%
    dplyr::select(PlotKey,
                  Species,
                  Hydro)

  totals <- Community_Composition(SpeciesList, method = method, tall = T, Hydro)%>%
    dplyr::filter(grepl("\\.HYDRO$", metric))%>%
    dplyr::group_by(PlotKey)%>%
    dplyr::summarize(!!fieldname := {ifelse(method == "percent",
                                            percent,
                                            count)})

  return(totals)
}

#'@export Community_HydroFAC
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
    dplyr::mutate(AW_WetStatus = ifelse(Species!=""&AW_WetStatus=="","NR",AW_WetStatus))%>%
    dplyr::mutate(WMVC_WetStatus = ifelse(Species!=""&WMVC_WetStatus=="","NR", WMVC_WetStatus))

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
  SpeciesList <- dplyr::left_join(header, SpeciesList)%>%
    dplyr::left_join(., masterspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(PlotKey)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & Species.y %in% c(NA, "")) &
                    !(duplicated(Species) & !(Species.y %in% c(NA, ""))))%>%
    dplyr::mutate(HydroFAC = ifelse(Region=="Arid West", AW_WetStatus, WMVC_WetStatus))%>%
    dplyr::mutate(HydroFAC = ifelse(grepl("FAC$|FACW|OBL", HydroFAC), "HydroFAC", HydroFAC))%>%
    dplyr::filter(.,HydroFAC !=""|is.na(HydroFAC))%>%
    dplyr::select(PlotKey,
                  Species,
                  HydroFAC)

  totals <- Community_Composition(SpeciesList, method = method, tall = T, HydroFAC)%>%
    dplyr::filter(grepl("\\.HYDROFAC$", metric))%>%
    dplyr::group_by(PlotKey)%>%
    dplyr::summarize(!!fieldname := {ifelse(method == "percent",
                                            percent,
                                            count)})

  return(totals)
}

#'@export Community_GrowthHabit
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
    dplyr::group_by(PlotKey)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & Species.y %in% c(NA, "")) &
                    !(duplicated(Species) & !(Species.y %in% c(NA, ""))))%>%
    dplyr::filter(GrowthHabitSub != "" & !is.na(GrowthHabitSub))

  totals <- Community_Composition(SpeciesList, method = method, tall = T, GrowthHabitSub)%>%
    dplyr::mutate(metric = paste(fieldname, stringr::str_to_title(stringr::str_replace(metric, "Percent\\.|Count\\.", "")), sep = ""))%>%
    dplyr::group_by(PlotKey)%>%
    tidyr::pivot_wider(names_from = metric, values_from = {ifelse(method == "percent",
                                                                  expr(percent),
                                                                  expr(count))})

  return(totals)
}

#'@export Community_Duration
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
    dplyr::group_by(PlotKey)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & Species.y %in% c(NA, "")) &
                    !(duplicated(Species) & !(Species.y %in% c(NA, ""))))%>%
    dplyr::filter(Duration != "" & !is.na(Duration))

  totals <- Community_Composition(SpeciesList, method = method, tall = T, Duration)%>%
    dplyr::mutate(metric = paste(fieldname, stringr::str_to_title(stringr::str_replace(metric, "Percent\\.|Count\\.", "")), sep = ""))%>%
    dplyr::group_by(PlotKey)%>%
    tidyr::pivot_wider(names_from = metric, values_from = {ifelse(method == "percent",
                                                                  expr(percent),
                                                                  expr(count))})

  return(totals)
}

#'@export Community_Metrics
Community_Metrics <- function(header, spp_inventory, lpi_tall, masterspecieslist){

  #Calculate all metrics using species inventory
  SppInvRich <- Community_Richness(spp_inventory, masterspecieslist, listtype = "speciesinventory")
  SppInvC.Val <- Community_C.Value(header, spp_inventory, masterspecieslist, listtype = "speciesinventory")
  SppInvNative <- Community_Native(spp_inventory, masterspecieslist, listtype = "speciesinventory")
  SppInvNox <- Community_NoxiousCount(header, spp_inventory, masterspecieslist, listtype = "speciesinventory")
  SppInvHydro <- Community_Hydrophytes(header, spp_inventory, masterspecieslist, listtype = "speciesinventory")
  SppInvHydroFAC <- Community_HydroFAC(header, spp_inventory, masterspecieslist, listtype = "speciesinventory")
  SppInvGrowthForm <- Community_GrowthHabit(spp_inventory, masterspecieslist, listtype = "speciesinventory")
  SppInvDuration <- Community_Duration(spp_inventory, masterspecieslist, listtype = "speciesinventory")

  #Calculate all metrics using LPI
  LPIRich <- Community_Richness(lpi_tall, masterspecieslist, listtype = "lpi")
  LPIC.Val <- Community_C.Value(header, lpi_tall, masterspecieslist, listtype = "lpi")
  LPINative <- Community_Native(lpi_tall, masterspecieslist, listtype = "lpi")
  LPINox <- Community_NoxiousCount(header, lpi_tall, masterspecieslist, listtype = "lpi")
  LPIHydro <- Community_Hydrophytes(header, lpi_tall, masterspecieslist, listtype = "lpi")
  LPIHydroFAC <- Community_HydroFAC(header, lpi_tall, masterspecieslist, listtype = "lpi")
  LPIGrowthForm <- Community_GrowthHabit(lpi_tall, masterspecieslist, listtype = "lpi")
  LPIDuration <- Community_Duration(lpi_tall, masterspecieslist, listtype = "lpi")

  #Join all metrics into one table with PlotID, Name and AdminState.
  AllCommunityMetrics <- dplyr::left_join(header%>%dplyr::select(PlotID,
                                                                 PlotKey,
                                                                 SiteName,
                                                                 AdminState),
                                          SppInvRich)%>%
    dplyr::left_join(., SppInvC.Val) %>%
    dplyr::left_join(., SppInvNative)%>%
    dplyr::left_join(., SppInvNox)%>%
    dplyr::left_join(., SppInvHydro)%>%
    dplyr::left_join(., SppInvHydroFAC)%>%
    dplyr::left_join(., SppInvGrowthForm)%>%
    dplyr::left_join(., SppInvDuration)%>%

    dplyr::left_join(., LPIRich)%>%
    dplyr::left_join(., LPIC.Val)%>%
    dplyr::left_join(., LPINative)%>%
    dplyr::left_join(., LPINox)%>%
    dplyr::left_join(., LPIHydro)%>%
    dplyr::left_join(., LPIHydroFAC)%>%
    dplyr::left_join(., LPIGrowthForm)%>%
    dplyr::left_join(., LPIDuration)

  return(AllCommunityMetrics)
}
