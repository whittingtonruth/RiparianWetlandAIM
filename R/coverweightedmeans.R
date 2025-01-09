#'Calculate cover-weighted averages of numeric plant traits
#'
#'@description Function used to calculate a cover-weighted average (or CWM) of given plant traits for species found on plot. Nonvascular species are not included in cover weighted averages.
#'
#'@param cover_species A tall/long-format data frame. Use the data frame from the \code{pct_AbsoluteSpeciesCover} output, then join desired species traits.
#'@param nationalspecieslist Data frame. The centrally managed master species list should be used. The assumed structure is that each row is unique on its Symbol.
#'@param statespecieslist Data frame. The centrally managed master species list should be used. This dataframe should contain a unique record for each Symbol-SpeciesState combination.
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
    dplyr::mutate_at(.vars = planttraits, ~ifelse(. == "", NA, .))%>%
    dplyr::group_by(PlotID, EvaluationID)%>%
    #This allows a calculation of the cwm for multiple plant traits. Function applied calculates the relative cover of each species including only those species with the plant trait specified.
    #Ex. a site with only two species with a stability rating will divide their absolute cover by the total absolute cover of those two species.
    dplyr::mutate(across(.cols = dplyr::all_of(planttraits),
                         .fns = ~.x * ifelse(is.na(.x),
                                             NA,
                                             AH_SpeciesCover/sum(ifelse(is.na(.x), NA, AH_SpeciesCover), na.rm = T)),
                         .names = "LPI_{.col}_CWM"))%>%
    dplyr::summarize_at(vars(paste0("LPI_", planttraits, "_CWM")),
                        #Sum all weighted ratings. If all ratings are NA, should return NA.
                        ~ifelse(all(is.na(.)), NA, round(sum(., na.rm = T), 2)))

  return(cwmind)
}

#'@export cwm_metrics
#'@rdname coverweightedmean
cwm_metrics <- function(cover_species,
                        header,
                        nationalspecieslist,
                        statespecieslist){

  nationalspecieslist <- nationalspecieslist%>%
    dplyr::select(Symbol, TaxonLevel, GrowthHabit,
           StabilityRating,
           dplyr::ends_with("WetStatus"))

  statespecieslist <- statespecieslist%>%
    dplyr::select(Symbol, SpeciesState, StateCValue)

  #To ensure any new states not included in hard coded field calculations, warning will be given.
  if(!all(header$SpeciesState %in% c("AK", "AZ", "CA", "CO", "ID", "MN", "MT", "NM", "NV", "OR", "UT", "WA", "WI", "WY"))){
    warning("Some states in header do not have a C-Value column in the species list provided. Sites in states outside of the expected set will have NA as their C-Value CWM. ")
  }

  cover_species <- cover_species%>%
    dplyr::left_join(., header%>%
                       dplyr::select(EvaluationID, SpeciesState, WetlandIndicatorRegion),
                     by = "EvaluationID")%>%
    dplyr::left_join(., nationalspecieslist, by = c("Code" = "Symbol"))%>%
    dplyr::left_join(., statespecieslist, by = c("Code" = "Symbol", "SpeciesState"))%>%
    #Filter out nonvascular codes to ensure they don't effect calculations
    dplyr::filter(GrowthHabit!="Nonvascular")%>%
    #add a wetland indicator status with respect to plot region
    dplyr::mutate(WetlandIndicatorStatus = dplyr::case_when(WetlandIndicatorRegion=="Arid West" ~AW_WetStatus,
                                                            WetlandIndicatorRegion=="Western Mountains, Valleys, and Coast" ~WMVC_WetStatus,
                                                            WetlandIndicatorRegion=="Great Plains" ~GP_WetStatus,
                                                            WetlandIndicatorRegion=="Alaska" ~AK_WetStatus,
                                                            WetlandIndicatorRegion=="Midwest"~MW_WetStatus,
                                                            WetlandIndicatorRegion=="Northcentral and Northeast"~NCNE_WetStatus,
                                                            TRUE ~ "REGIONMISSING"),
                  #Vascular plants ID'd to species without a indicator status should be considered upland.
                  WetlandIndicatorStatus = ifelse(WetlandIndicatorStatus == "" & TaxonLevel%in%c("Species", "Trinomial"), "UPL", WetlandIndicatorStatus),
                  SiteWetlandValue = dplyr::case_when(WetlandIndicatorStatus == "UPL"~0,
                                                      WetlandIndicatorStatus == "FACU"~25,
                                                      WetlandIndicatorStatus == "FAC"~50,
                                                      WetlandIndicatorStatus == "FACW"~75,
                                                      WetlandIndicatorStatus == "OBL"~100))%>%
    dplyr::select(PlotID, EvaluationID, Code, AH_SpeciesCover, WetlandIndicatorStatus, SiteWetlandValue, StateCValue, StabilityRating)

  cwm_metrics <- coverweightedmean(cover_species, planttraits = c("StabilityRating", "SiteWetlandValue", "StateCValue"))%>%
    dplyr::mutate(LPI_SiteWetlandRating_CWM = dplyr::case_when(LPI_SiteWetlandValue_CWM < 17 ~ "UPL",
                                                               LPI_SiteWetlandValue_CWM < 43 ~ "FACU",
                                                               LPI_SiteWetlandValue_CWM < 67 ~ "FAC",
                                                               LPI_SiteWetlandValue_CWM < 92 ~ "FACW",
                                                               LPI_SiteWetlandValue_CWM <=100 ~ "OBL"))

  return(cwm_metrics)

}
