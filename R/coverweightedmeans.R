#'Calculate cover-weighted averages of numeric plant traits
#'
#'@description Function used to calculate a cover-weighted average (or CWM) of given plant traits for species found on plot. Nonvascular species are not included in cover weighted averages.
#'
#'@param cover_species A tall/long-format data frame. Use the data frame from the \code{pct_AbsoluteSpeciesCover} output, then join desired species traits.
#'@param masterspecieslist Data frame. The centrally managed master species list should be used.
#'@param planttraits character vector. The field name(s) of numeric plant traits found in the \code{masterspecieslist} which will be used to calculate a cover-weighted mean.
#'@returns Data frame of a cover weighted mean (CWM) of plant traits selected for each plot found in the \code{cover_species} dataframe.


#'@export coverweightedmean
#'@rdname coverweightedmean
coverweightedmean <- function(cover_species,
                planttraits){

  #Error for if plant traits are missing from master species list.
  if(!all(planttraits %in% colnames(cover_species))){
    stop("At least one of the planttraits specified was not found in the masterspecieslist provided. ")
  }

  #final calculation
  cwmind <- cover_species%>%
    mutate_at(.vars = planttraits, ~ifelse(. == "", NA, .))%>%
    group_by(PlotID, EvaluationID)%>%
    #This allows a calculation of the cwm for multiple plant traits. Function applied calculates the relative cover of each species including only those species with the plant trait specified.
    #Ex. a site with only two species with a stability rating will divide their absolute cover by the total absolute cover of those two species.
    mutate(across(.cols = all_of(planttraits),
                  .fns = ~.x * ifelse(is.na(.x),
                                 NA,
                                 AH_SpeciesCover/sum(ifelse(is.na(.x), NA, AH_SpeciesCover), na.rm = T)),
                  .names = "LPI_{.col}_CWM"))%>%
    summarize_at(vars(paste0("LPI_", planttraits, "_CWM")),
                 #Sum all weighted ratings. If all ratings are NA, should return NA.
                 ~ifelse(all(is.na(.)), NA, round(sum(., na.rm = T), 2)))

  return(cwmind)
}

#'@export cwm_metrics
#'@rdname coverweightedmean
cwm_metrics <- function(cover_species,
                        header,
                        masterspecieslist){

  masterspecieslist <- masterspecieslist%>%
    select(Symbol, SpeciesName = Species, type,
           ends_with("_C.Value"),
           ends_with("_WetStatus"),
           StabilityNum)

  #To ensure any new states not included in hard coded field calculations, warning will be given.
  if(!all(header$SpeciesState %in% c("AK", "AZ", "CA", "CO", "ID", "MT", "NM", "NV", "OR", "UT", "WY", "WA"))){
    warning("Some states in header do not have a C-Value column in the species list provided. Sites in states outside of the expected set will have NA as their C-Value CWM. ")
  }

  cover_species <- cover_species%>%
    left_join(., header%>%
                dplyr::select(EvaluationID, SpeciesState, WetlandIndicatorRegion),
              by = "EvaluationID")%>%
    left_join(., masterspecieslist, by = c("Code" = "Symbol"))%>%
    #Filter out nonvascular codes to ensure they don't effect calculations
    filter(type!="Nonvascular")%>%
    #add a wetland indicator status with respect to plot region
    mutate(WetlandIndicatorStatus = case_when(WetlandIndicatorRegion=="Arid West" ~AW_WetStatus,
                                              WetlandIndicatorRegion=="Western Mountains, Valleys, and Coast" ~WMVC_WetStatus,
                                              WetlandIndicatorRegion=="Great Plains" ~GP_WetStatus,
                                              WetlandIndicatorRegion=="Alaska" ~AK_WetStatus,
                                              TRUE ~ "REGIONMISSING"),
           #Vascular plants ID'd to species without a indicator status should be considered upland.
           WetlandIndicatorStatus = ifelse(WetlandIndicatorStatus == "" & SpeciesName != "", "UPL", WetlandIndicatorStatus),
           SiteWetlandValue = case_when(WetlandIndicatorStatus == "UPL"~0,
                                        WetlandIndicatorStatus == "FACU"~25,
                                        WetlandIndicatorStatus == "FAC"~50,
                                        WetlandIndicatorStatus == "FACW"~75,
                                        WetlandIndicatorStatus == "OBL"~100),
           CValue = case_when(SpeciesState == "AK"~AK_C.Value,
                              SpeciesState == "AZ"~AZ_C.Value,
                              SpeciesState == "CA"~CA_C.Value,
                              SpeciesState == "CO"~CO_C.Value,
                              SpeciesState == "ID"~ID_C.Value,
                              SpeciesState == "MT"~MT_C.Value,
                              SpeciesState == "NM"~NM_C.Value,
                              SpeciesState == "NV"~NV_C.Value,
                              SpeciesState == "OR"~OR_C.Value,
                              SpeciesState == "UT"~UT_C.Value,
                              SpeciesState == "WY"~WY_C.Value,
                              SpeciesState == "WA"~WA_C.Value))%>%
    select(PlotID, EvaluationID, Code, AH_SpeciesCover, WetlandIndicatorStatus, SiteWetlandValue, CValue, StabilityNum)

  cwm_metrics <- coverweightedmean(cover_species, planttraits = c("StabilityNum", "SiteWetlandValue", "CValue"))%>%
    rename(LPI_StabilityValue_CWM = LPI_StabilityNum_CWM)%>%
    mutate(LPI_SiteWetlandRating_CWM = case_when(LPI_SiteWetlandValue_CWM < 17 ~ "UPL",
                                                 LPI_SiteWetlandValue_CWM < 43 ~ "FACU",
                                                 LPI_SiteWetlandValue_CWM < 67 ~ "FAC",
                                                 LPI_SiteWetlandValue_CWM < 92 ~ "FACW",
                                                 LPI_SiteWetlandValue_CWM <=100 ~ "OBL"))

  return(cwm_metrics)

}
