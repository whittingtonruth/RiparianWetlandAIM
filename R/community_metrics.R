#'Summarize community composition for specified grouping variables.
#'
#'@description This group of functions prepares a species list which is then used in the \code{community_composition()} function. This can involve filtering duplicate species, combining grouping variable categories, and pulling plot information from plot headers to inform species classification. Analogous to \code{cover_metrics} functions.
#'
#'Similarly to relative cover calculations, all species with unknown traits (e.g. an unknown plant identified only as Elymus sp.) must be filtered out prior to calculating community metrics. All community metric calculations may be calculated using \code{Community_Metrics()}
#'
#'@param header Data frame. Use the data frame from the \code{header_build_lentic()} output. Used in Noxious and Wetland Indicator calculations to specify the plot WetlandIndicatorRegion or state.
#'@param SpeciesList Data frame. Table of species by PlotID that will be summarized. Can be either species_inventory_tall produced by \code{gather_spp_inventory_lentic()} or lpi_tall produced by \code{gather_lpi_lentic()}.
#'@param nationalspecieslist Data frame. The centrally managed master species list should be used. The assumed structure is that each row is unique on its Symbol.
#'@param statespecieslist Data frame. The centrally managed master species list should be used. This dataframe should contain a unique record for each Symbol-SpeciesState combination.
#'@param unknowncodes Optional data frame. Use the data frame from the \code{gather_unknowns_lentic()} output. Unknown species list matching unknown codes to their duration and Growth habit. This is used to fill in duration and growth habit for plants in the species list never identified to a species or genus with those fields specified. If argument is unused, all unknown species without duration or Growth Habit specified will be filtered out before being passed on to \code{community_composition()}.
#'@param listtype Character string. Indicates the source of the SpeciesList provided. Can either be "speciesinventory" or"lpi". Defaults to "speciesinventory".
#'@param method character string. The method used for the produced summary table. Can be "percent", "mean", or "count". Included in functions where applicable.
#'@returns Data frame of summary metrics by plot.

#'@export Community_Richness
#'@rdname Community_Metrics
Community_Richness <- function(SpeciesList, nationalspecieslist, listtype = "speciesinventory"){

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  #assign a field name based on source of data.
  fieldname <- paste(ifelse(listtype == "speciesinventory", "SppInv", "LPI"), "_Richness_Cnt", sep = "")

  #Remove non-plant codes if using LPI.
  if(listtype == "lpi"){
    SpeciesList <- SpeciesList %>%
      dplyr::rename(Species = code)%>%
      dplyr::filter(!(Species %in% nonplantcodes$code) & layer != "SoilSurface")
  }

  #join provided species list to master list and filter out duplicates.
  SpeciesList <- SpeciesList%>%
    dplyr::left_join(., nationalspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(EvaluationID)%>%
    #filter duplicate "Species" if the species is at the species or trinomial level. Filter out duplicate unknowns if the code and the unknowncodekey are the same.
    dplyr::filter(!(duplicated(UnknownCodeKey) & !TaxonLevel %in% c("Species", "Trinomial")) &
                    !(duplicated(Species) & TaxonLevel %in% c("Species", "Trinomial")),
                  Duration != "Nonvascular")

  totals <- Community_Composition(SpeciesList, method = "count")%>%
    dplyr::rename(!!fieldname := Cnt)

  return(totals)
}

#'@export Community_C.Value
#'@rdname Community_Metrics
Community_C.Value <- function(header, SpeciesList, statespecieslist, listtype = "speciesinventory"){

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  #assign fieldname based on source of the species list data.
  fieldname <- paste(ifelse(listtype == "speciesinventory", "SppInv", "LPI"), "_CValue_Avg", sep = "")

  #Remove non-plant codes if using LPI.
  if(listtype == "lpi"){
    SpeciesList <- SpeciesList %>%
      dplyr::rename(Species = code)%>%
      dplyr::filter(!(Species %in% nonplantcodes$code) & layer != "SoilSurface")
  }

  #filter out sites with no c-value data available in provided species list.
  if(!all(header$SpeciesState %in% c("AK", "AZ", "CA", "CO", "ID", "MN", "MT", "NM", "NV", "OR", "UT", "WA", "WI", "WY"))){
    warning("Some states in header do not have a C-Value column in the species list provided. Sites in states outside of the expected set will be removed from c-value calculations. ")
  }

  header <- header%>%
    dplyr::filter(SpeciesState %in% c("AK", "AZ", "CA", "CO", "ID", "MN", "MT", "NM", "NV", "OR", "UT", "WA", "WI", "WY"))%>%
    {if("sf" %in% class(header))sf::st_drop_geometry(.) else .}

  #Join header, specieslist, and master species list together and filter out unknowns. Add C.Value column to add state-specific C-Values
  SpeciesList <- dplyr::left_join(header,
                                  SpeciesList, by = "EvaluationID")%>%
    dplyr::left_join(., statespecieslist, by = c("Species" = "Symbol", "SpeciesState"))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & !TaxonLevel %in% c("Species", "Trinomial")) &
                    !(duplicated(Species) & TaxonLevel %in% c("Species", "Trinomial")),
                  GrowthHabit != "Nonvascular")%>%
    dplyr::select(EvaluationID, SpeciesState, Species, StateCValue)

  totals <- Community_Composition(SpeciesList, method = "mean", tall = T, StateCValue)%>%
    dplyr::rename(!!fieldname := Avg)

  return(totals)
}

#'@export Community_Native
#'@rdname Community_Metrics
Community_Native <- function(SpeciesList, nationalspecieslist, listtype = "speciesinventory", method = "percent"){

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  if(!(method %in% c("percent", "count"))){
    stop("Method must be 'percent' or 'count'.")
  }

  #Create fieldname based on source of data and kind of calculation.
  fieldname <- ifelse(listtype == "speciesinventory", "SppInv", "LPI")

  #Remove non-plant codes if using LPI.
  if(listtype == "lpi"){
    SpeciesList <- SpeciesList %>%
      dplyr::rename(Species = code)%>%
      dplyr::filter(!(Species %in% nonplantcodes$code) & layer != "SoilSurface")
  }

  nationalspecieslist$NativeStatus[nationalspecieslist$NativeStatus=="cryptogenic"] <- "nonnative"

  #Join specieslist to master species list and filter to relevant entries.
  SpeciesList <- SpeciesList%>%
    dplyr::left_join(., nationalspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & !TaxonLevel %in% c("Species", "Trinomial")) &
                    !(duplicated(Species) & TaxonLevel %in% c("Species", "Trinomial")),
                  Duration != "Nonvascular")%>%
    dplyr::filter(NativeStatus != "" & !is.na(NativeStatus))%>%
    dplyr::select(EvaluationID, Species, NativeStatus)

  #Calculate community metrics and rename columns.
  totals <- Community_Composition(SpeciesList, method = method, tall = T, NativeStatus)%>%
    dplyr::filter(grepl("NATIVE_", metric))%>%
    dplyr::mutate(metric = paste(fieldname,
                                 stringr::str_replace(stringr::str_to_title(stringr::str_replace(metric, "_", " ")), " ", "_"), sep = "_"))%>%
    dplyr::group_by(EvaluationID)%>%
    tidyr::pivot_wider(names_from = metric,
                       values_from = {ifelse(method == "percent",
                                             expr(Pct),
                                             expr(Cnt))})

  return(totals)
}

#'@export Community_NoxiousCount
#'@rdname Community_Metrics
Community_NoxiousCount <- function(header, SpeciesList, statespecieslist, listtype = "speciesinventory"){

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  #create fieldnames based on source of species data.
  fieldname <- ifelse(listtype == "speciesinventory", "SppInv", "LPI")

  #Remove non-plant codes if using LPI.
  if(listtype == "lpi"){
    SpeciesList <- SpeciesList %>%
      dplyr::rename(Species = code)%>%
      dplyr::filter(!(Species %in% nonplantcodes$code) & layer != "SoilSurface")
  }

  if(!all(header$SpeciesState %in% c("AK", "AZ", "CA", "CO", "ID", "MN", "MT", "NM", "NV", "OR", "UT", "WA", "WI", "WY"))){
    warning("Some states in header do not have a noxious column in the species list provided. Sites in states outside of the expected set will be removed from noxious calculations. ")
  }

  header <- header%>%
    dplyr::filter(SpeciesState %in% c("AK", "AZ", "CA", "CO", "ID", "MN", "MT", "NM", "NV", "OR", "UT", "WA", "WI", "WY"))%>%
    {if("sf" %in% class(header))sf::st_drop_geometry(.) else .}

  #join header, specieslist and master species list together, then filter out duplicates and unnecessary columns.
  SpeciesList <- dplyr::left_join(header,
                                  SpeciesList, by = "EvaluationID")%>%
    dplyr::left_join(., statespecieslist, by = c("Species" = "Symbol", "SpeciesState"))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & !TaxonLevel %in% c("Species", "Trinomial")) &
                    !(duplicated(Species) & TaxonLevel %in% c("Species", "Trinomial")),
                  GrowthHabit != "Nonvascular")%>%
    dplyr::select(EvaluationID, SpeciesState, Species, StateNoxious)%>%
    dplyr::mutate(Noxious = ifelse(StateNoxious%in%c("", NA), "", "Noxious"))

  #Calculate community metrics and rename columns.
  totals <- Community_Composition(SpeciesList, method = "count", tall = T, Noxious)%>%
    dplyr::filter(grepl("*NOXIOUS_", metric))%>%
    dplyr::mutate(metric = paste(fieldname, stringr::str_replace(stringr::str_to_title(stringr::str_replace(metric, "_", " ")), " ", "_"), sep = "_"))%>%
    dplyr::group_by(EvaluationID)%>%
    tidyr::pivot_wider(names_from = metric,
                       values_from = Cnt)

  return(totals)
}

#'@export Community_HydroNoFAC
#'@rdname Community_Metrics
Community_HydroNoFAC <- function(header, SpeciesList, nationalspecieslist, listtype = "speciesinventory", method = "percent"){

  if(!(method %in% c("percent", "count"))){
    stop("Method must be 'percent' or 'count'.")
  }

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  #Create fieldname based on source of species list and method used to calculate metrics
  fieldname <- ifelse(listtype == "speciesinventory", "SppInv", "LPI")

  nationalspecieslist <- nationalspecieslist%>%
    dplyr::select(Symbol,
                  ScientificName,
                  TaxonLevel,
                  Duration,
                  ends_with("_WetStatus")
    )%>%
    #Change all species without an indicator status to Not Rated so they will be included in calculation
    dplyr::mutate(AW_WetStatus = ifelse(TaxonLevel%in%c("Species", "Trinomial")&Duration != "Nonvascular"&AW_WetStatus=="",
                                        "NR",AW_WetStatus),
                  WMVC_WetStatus = ifelse(TaxonLevel%in%c("Species", "Trinomial")&Duration != "Nonvascular"&WMVC_WetStatus=="",
                                          "NR", WMVC_WetStatus),
                  GP_WetStatus = ifelse(TaxonLevel%in%c("Species", "Trinomial")&Duration != "Nonvascular"&GP_WetStatus=="",
                                        "NR", GP_WetStatus),
                  AK_WetStatus = ifelse(TaxonLevel%in%c("Species", "Trinomial")&Duration != "Nonvascular"&AK_WetStatus=="",
                                        "NR", AK_WetStatus),
                  MW_WetStatus = ifelse(TaxonLevel%in%c("Species", "Trinomial")&Duration != "Nonvascular"&MW_WetStatus=="",
                                        "NR", MW_WetStatus),
                  NCNE_WetStatus = ifelse(TaxonLevel%in%c("Species", "Trinomial")&Duration != "Nonvascular"&NCNE_WetStatus=="",
                                          "NR", NCNE_WetStatus))

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
  SpeciesList <- dplyr::left_join(header%>%
                                    {if("sf" %in% class(header))sf::st_drop_geometry(.) else .},
                                  SpeciesList, by = "EvaluationID")%>%
    dplyr::left_join(., nationalspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & !TaxonLevel %in% c("Species", "Trinomial")) &
                    !(duplicated(Species) & TaxonLevel %in% c("Species", "Trinomial")),
                  Duration != "Nonvascular")%>%
    dplyr::mutate(Hydro = case_when(WetlandIndicatorRegion=="Arid West" ~AW_WetStatus,
                                    WetlandIndicatorRegion=="Western Mountains, Valleys, and Coast" ~WMVC_WetStatus,
                                    WetlandIndicatorRegion=="Great Plains" ~GP_WetStatus,
                                    WetlandIndicatorRegion=="Alaska"~AK_WetStatus,
                                    WetlandIndicatorRegion=="Midwest"~MW_WetStatus,
                                    WetlandIndicatorRegion=="Northcentral and Northeast"~NCNE_WetStatus,
                                    TRUE ~ "REGIONMISSING"))%>%
    dplyr::mutate(Hydro = ifelse(grepl("FACW|OBL", Hydro), "HydroNoFAC", Hydro))%>%
    dplyr::filter(.,Hydro !=""|is.na(Hydro))%>%
    dplyr::select(EvaluationID,
                  Species,
                  Hydro)

  totals <- Community_Composition(SpeciesList, method = method, tall = T, Hydro)%>%
    dplyr::filter(grepl("*HYDRONOFAC_", metric))%>%
    dplyr::mutate(metric = paste(fieldname,
                                 stringr::str_replace_all(metric, c("HYDRONOFAC" = "HydroNoFAC")), sep = "_"))%>%
    dplyr::group_by(EvaluationID)%>%
    tidyr::pivot_wider(names_from = metric,
                       values_from = {ifelse(method == "percent",
                                             expr(Pct),
                                             expr(Cnt))})

  return(totals)
}

#'@export Community_HydroWithFAC
#'@rdname Community_Metrics
Community_HydroWithFAC <- function(header, SpeciesList, nationalspecieslist, listtype = "speciesinventory", method = "percent"){

  if(!(method %in% c("percent", "count"))){
    stop("Method must be 'percent' or 'count'.")
  }

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  #Create fieldname based on source of species list and method used to calculate metrics
  fieldname <- ifelse(listtype == "speciesinventory", "SppInv", "LPI")

  nationalspecieslist <- nationalspecieslist%>%
    dplyr::select(Symbol,
                  ScientificName,
                  TaxonLevel,
                  Duration,
                  ends_with("_WetStatus")
    )%>%
    #Change all species without an indicator status to Not Rated so they will be included in calculation
    dplyr::mutate(AW_WetStatus = ifelse(TaxonLevel%in%c("Species", "Trinomial")&Duration != "Nonvascular"&AW_WetStatus=="",
                                        "NR",AW_WetStatus),
                  WMVC_WetStatus = ifelse(TaxonLevel%in%c("Species", "Trinomial")&Duration != "Nonvascular"&WMVC_WetStatus=="",
                                          "NR", WMVC_WetStatus),
                  GP_WetStatus = ifelse(TaxonLevel%in%c("Species", "Trinomial")&Duration != "Nonvascular"&GP_WetStatus=="",
                                        "NR", GP_WetStatus),
                  AK_WetStatus = ifelse(TaxonLevel%in%c("Species", "Trinomial")&Duration != "Nonvascular"&AK_WetStatus=="",
                                        "NR", AK_WetStatus),
                  MW_WetStatus = ifelse(TaxonLevel%in%c("Species", "Trinomial")&Duration != "Nonvascular"&MW_WetStatus=="",
                                        "NR", MW_WetStatus),
                  NCNE_WetStatus = ifelse(TaxonLevel%in%c("Species", "Trinomial")&Duration != "Nonvascular"&NCNE_WetStatus=="",
                                          "NR", NCNE_WetStatus))

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
  SpeciesList <- dplyr::left_join(header%>%
                                    {if("sf" %in% class(header))sf::st_drop_geometry(.) else .},
                                  SpeciesList, by = "EvaluationID")%>%
    dplyr::left_join(., nationalspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & !TaxonLevel %in% c("Species", "Trinomial")) &
                    !(duplicated(Species) & TaxonLevel %in% c("Species", "Trinomial")),
                  Duration != "Nonvascular")%>%
    dplyr::mutate(HydroWithFAC = case_when(WetlandIndicatorRegion=="Arid West" ~AW_WetStatus,
                                       WetlandIndicatorRegion=="Western Mountains, Valleys, and Coast" ~WMVC_WetStatus,
                                       WetlandIndicatorRegion=="Great Plains" ~GP_WetStatus,
                                       WetlandIndicatorRegion=="Alaska"~AK_WetStatus,
                                       WetlandIndicatorRegion=="Midwest"~MW_WetStatus,
                                       WetlandIndicatorRegion=="Northcentral and Northeast"~NCNE_WetStatus,
                                       TRUE ~ "REGIONMISSING"))%>%
    dplyr::mutate(HydroWithFAC = ifelse(grepl("FAC$|FACW|OBL", HydroWithFAC), "HydroWithFAC", HydroWithFAC))%>%
    dplyr::filter(.,HydroWithFAC !=""|is.na(HydroWithFAC))%>%
    dplyr::select(EvaluationID,
                  Species,
                  HydroWithFAC)

  totals <- Community_Composition(SpeciesList, method = method, tall = T, HydroWithFAC)%>%
    dplyr::filter(grepl("*HYDROWITHFAC_", metric))%>%
    dplyr::mutate(metric = paste(fieldname,
                                 stringr::str_replace(metric, "HYDROWITHFAC", "HydroWithFAC"), sep = "_"))%>%
    dplyr::group_by(EvaluationID)%>%
    tidyr::pivot_wider(names_from = metric,
                       values_from = {ifelse(method == "percent",
                                             expr(Pct),
                                             expr(Cnt))})

  return(totals)
}

#'@export Community_SGGroup
#'@rdname Community_Metrics
Community_SGGroup <- function(SpeciesList, nationalspecieslist, listtype = "speciesinventory", method = "percent"){

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  if(!(method %in% c("percent", "count"))){
    stop("Method must be 'percent' or 'count'.")
  }

  #Create fieldname based on source of data and kind of calculation.
  fieldname <- ifelse(listtype == "speciesinventory", "SppInv", "LPI")

  #Remove non-plant codes if using LPI.
  if(listtype == "lpi"){
    SpeciesList <- SpeciesList %>%
      dplyr::rename(Species = code)%>%
      dplyr::filter(!(Species %in% nonplantcodes$code) & layer != "SoilSurface")
  }

  #Join specieslist to master species list and filter to relevant entries.
  SpeciesList <- SpeciesList%>%
    dplyr::left_join(., nationalspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & !TaxonLevel %in% c("Species", "Trinomial")) &
                    !(duplicated(Species) & TaxonLevel %in% c("Species", "Trinomial")),
                  Duration != "Nonvascular")%>%
    dplyr::select(EvaluationID, Species, SG_Group)

  spplist <- SpeciesList%>%
    dplyr::filter(SG_Group=="Preferred Forb")%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::mutate(Spp_PreferredForb = paste0(Species, collapse = "; "))%>%
    dplyr::distinct(EvaluationID, .keep_all = T)

  #Calculate community metrics and rename columns.
  totals <- Community_Composition(SpeciesList, method = method, tall = T, SG_Group)%>%
    dplyr::filter(!grepl("^_", metric))%>%
    dplyr::mutate(metric = paste0(fieldname, "_SG",
                                 stringr::str_replace_all(
                                   stringr::str_to_title(metric),c(" "="", "cnt" = "Cnt", "pct" = "Pct"))))%>%
    dplyr::group_by(EvaluationID)%>%
    tidyr::pivot_wider(names_from = metric,
                       values_from = {ifelse(method == "percent",
                                             expr(Pct),
                                             expr(Cnt))})%>%
    dplyr::left_join(.,
                     spplist%>%dplyr::select(EvaluationID, Spp_PreferredForb),
                     by = "EvaluationID")


  return(totals)
}

#'@export Community_GrowthHabitSub
#'@rdname Community_Metrics
Community_GrowthHabitSub <- function(SpeciesList, nationalspecieslist, unknowncodes = NULL, listtype = "speciesinventory", method = "percent"){

  if(!(method %in% c("percent", "count"))){
    stop("Method must be 'percent' or 'count'.")
  }

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  fieldname <- ifelse(listtype == "speciesinventory", "SppInv", "LPI")

  #If using LPI, change the code column to Species, then remove all nonplant codes.
  if(listtype == "lpi"){
    SpeciesList <- SpeciesList %>%
      dplyr::rename(Species = code)%>%
      dplyr::filter(!(Species %in% nonplantcodes$code) & layer != "SoilSurface")
  }

  #Join the SpeciesList and master species list together.
  SpeciesList <- SpeciesList%>%
    dplyr::left_join(., nationalspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & !TaxonLevel %in% c("Species", "Trinomial")) &
                    !(duplicated(Species) & TaxonLevel %in% c("Species", "Trinomial")),
                  Duration != "Nonvascular")%>%
    #if unknowncodes is provided, fill in the growthhabit for unknowns.
    {if(!is.null(unknowncodes)) dplyr::left_join(.,
                                                unknowncodes%>%
                                                  dplyr::select(UnknownCodeKey, DurationUnknown = Duration, GrowthHabitUnknown = GrowthHabit),
                                                by = "UnknownCodeKey")%>%
        dplyr::mutate(., GrowthHabitSub = ifelse(GrowthHabitSub=="", GrowthHabitUnknown, GrowthHabitSub))
      else .}%>%
    dplyr::filter(GrowthHabitSub != "" & !is.na(GrowthHabitSub))

  totals <- Community_Composition(SpeciesList, method = method, tall = T, GrowthHabitSub)%>%
    dplyr::mutate(metric = paste(fieldname,
                                 stringr::str_replace(stringr::str_to_title(stringr::str_replace(metric, "_", " ")), " ", "_"), sep = "_"))%>%
    dplyr::group_by(EvaluationID)%>%
    tidyr::pivot_wider(names_from = metric, values_from = {ifelse(method == "percent",
                                                                  expr(Pct),
                                                                  expr(Cnt))})

  return(totals)
}

#'@export Community_Duration
#'@rdname Community_Metrics
Community_Duration <- function(SpeciesList, nationalspecieslist, unknowncodes = NULL, listtype = "speciesinventory", method = "percent"){

  if(!(method %in% c("percent", "count"))){
    stop("Method must be 'percent' or 'count'.")
  }

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  fieldname <- ifelse(listtype == "speciesinventory", "SppInv", "LPI")

  #If using LPI, change the code column to Species, then remove all nonplant codes.
  if(listtype == "lpi"){
    SpeciesList <- SpeciesList %>%
      dplyr::rename(Species = code)%>%
      dplyr::filter(!(Species %in% nonplantcodes$code) & layer != "SoilSurface")
  }

  #Join the SpeciesList and master species list together.
  SpeciesList <- SpeciesList%>%
    dplyr::left_join(., nationalspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & !TaxonLevel %in% c("Species", "Trinomial")) &
                    !(duplicated(Species) & TaxonLevel %in% c("Species", "Trinomial")),
                  Duration != "Nonvascular")%>%
    #if unknowncodes is provided, fill in the duration for unknowns.
    {if(!is.null(unknowncodes)) dplyr::left_join(.,
                                                 unknowncodes%>%
                                                   dplyr::select(UnknownCodeKey, DurationUnknown = Duration, GrowthHabitUnknown = GrowthHabit),
                                                 by = "UnknownCodeKey")%>%
        dplyr::mutate(., Duration = ifelse(Duration=="", DurationUnknown, Duration))
      else .}%>%
    dplyr::filter(Duration != "" & !is.na(Duration))

  totals <- Community_Composition(SpeciesList, method = method, tall = T, Duration)%>%
    dplyr::mutate(metric = paste(fieldname,
                                 stringr::str_replace(stringr::str_to_title(stringr::str_replace(metric, "_", " ")), " ", "_"), sep = "_"))%>%
    dplyr::group_by(EvaluationID)%>%
    tidyr::pivot_wider(names_from = metric, values_from = {ifelse(method == "percent",
                                                                  expr(Pct),
                                                                  expr(Cnt))})

  return(totals)
}

#'@export Community_StabilityGrowthHabit
#'@rdname Community_Metrics
Community_StabilityGrowthHabit <- function(SpeciesList, nationalspecieslist, unknowncodes = NULL, listtype = "speciesinventory", method = "percent"){

  if(!(method %in% c("percent", "count"))){
    stop("Method must be 'percent' or 'count'.")
  }

  if(!(listtype %in% c("speciesinventory", "lpi"))){
    stop("listtype must be 'speciesinventory' or 'lpi'.")
  }

  fieldname <- ifelse(listtype == "speciesinventory", "SppInv", "LPI")

  #If using LPI, change the code column to Species, then remove all nonplant codes.
  if(listtype == "lpi"){
    SpeciesList <- SpeciesList %>%
      dplyr::rename(Species = code)%>%
      dplyr::filter(!(Species %in% nonplantcodes$code) & layer != "SoilSurface")
  }

  #Join the SpeciesList and master species list together.
  SpeciesList <- SpeciesList%>%
    dplyr::left_join(., nationalspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(EvaluationID)%>%
    #Filter out duplicated entries. Complicated with unknowns which may duplicate a lower level taxonomic code.
    dplyr::filter(!(duplicated(UnknownCodeKey) & !TaxonLevel %in% c("Species", "Trinomial")) &
                    !(duplicated(Species) & TaxonLevel %in% c("Species", "Trinomial")),
                  Duration != "Nonvascular")%>%
    #if unknowncodes is provided, fill in the growthhabit for unknowns.
    {if(!is.null(unknowncodes)) dplyr::left_join(.,
                                                 unknowncodes%>%
                                                   dplyr::select(UnknownCodeKey, DurationUnknown = Duration, GrowthHabitSubUnknown = GrowthHabit),
                                                 by = "UnknownCodeKey")%>%
        dplyr::mutate(., GrowthHabitSub = dplyr::case_when(GrowthHabit!=""&!is.na(GrowthHabit)~GrowthHabit,
                                                           GrowthHabit==""&GrowthHabitSubUnknown%in%c("Tree", "Shrub")~"Woody",
                                                           GrowthHabit==""&GrowthHabitSubUnknown%in%c("Graminoid", "Forb")~"NonWoody",
                                                           GrowthHabit==""&GrowthHabitSubUnknown%in%c("Liverwort", "Moss", "Lichen")~"Nonvascular"))
      else .}%>%
    dplyr::filter(StabilityRating != "" & !is.na(StabilityRating), !GrowthHabit %in% c("", NA, "Nonvascular"))%>%
    dplyr::mutate(GrowthHabit = case_when(GrowthHabit == "Woody"~"Woody",
                                          GrowthHabit == "NonWoody"~"Herbaceous"))

  totals <- Community_Composition(SpeciesList, method = method, tall = T, GrowthHabit, StabilityRating)%>%
    dplyr::mutate(metric = paste(fieldname,
                                 stringr::str_replace_all(
                                   stringr::str_to_title(
                                     stringr::str_replace(metric, "\\.", " ")),
                                   c(" " = "", "_cnt" = "Stability_Cnt", "_pct" = "Stability_Pct")),
                                 sep = "_"))%>%
    dplyr::group_by(EvaluationID)%>%
    tidyr::pivot_wider(names_from = metric, values_from = {ifelse(method == "percent",
                                                                  expr(Pct),
                                                                  expr(Cnt))})

  return(totals)
}
