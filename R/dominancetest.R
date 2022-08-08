#'Calculate whether each site passes the Dominance Test and Prevalence Test
#'
#'@description
#'
#'@param header Data frame. Use the data frame from the \code{header_build_lentic()} output. Used in Noxious and
#'Wetland Indicator calculations to specify the plot region or state.
#'@param lpi_tall A tall/long-format data frame. Use the data frame from the \code{gather_lpi_lentic()} output.
#'@param masterspecieslist Data frame. The centrally managed master species list should be used.
#'@param bystrata logical. Indicates whether Dominance Test should be performed by strata, as is recommended by the USACE.
#'@return Dataframe showing site scores on several ways to classify dominance of hydrophytic species.

#'@export dominance_test
#'@rdname dominance_test
dominance_test <- function(header, lpi_tall, masterspecieslist, test){

  header <- header%>%
    dplyr::select(EvaluationID,
                  SpeciesState,
                  WetlandIndicatorRegion)

  AbsoluteSpeciesCover <- pct_AbsoluteSpeciesCover(lpi_tall, masterspecieslist)%>%
    #join to header to get wetland indicator region
    dplyr::left_join(header, ., by = c("EvaluationID"))%>%

    #Join to masterlist for indicator staus and growth habit
    dplyr::left_join(.,
                     masterspecieslist%>%select(Symbol, ends_with("WetStatus"), GrowthHabitSub, Species),
                     by = c("Code" = "Symbol"))

  #calculate the total absolute cover made up by species that were never identified. May help explain some sites that didn't pass test.
  UnknownCover <- AbsoluteSpeciesCover%>%
    dplyr::filter(Species=="")%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::summarize(AbsoluteUnknownCover = sum(SpeciesCover))

  #Continue to filter out unknowns. Not fair to use in Dominance test. Then define wetland indicator status and strata
  AbsoluteSpeciesCover <- AbsoluteSpeciesCover%>%
    dplyr::filter(Species!="")%>%
    dplyr::mutate(HydroFAC = ifelse(WetlandIndicatorRegion=="Arid West", AW_WetStatus, WMVC_WetStatus),
                  Strata = ifelse(GrowthHabitSub %in% c("Graminoid", "Forb"), "Herbaceous", GrowthHabitSub))%>%
    dplyr::select(-c(ends_with("WetStatus"), GrowthHabitSub))

  #Allow for dominance to be applied with or without strata
  if(bystrata){
    level <- rlang::quos(EvaluationID, Strata)
  }else{
    level <- rlang::quos(EvaluationID)
  }

  Totals <- AbsoluteSpeciesCover%>%
    dplyr::group_by(!!!level)%>%
    summarise(TotalCover = sum(SpeciesCover), TwentyPercent = 0.2 * TotalCover, FiftyPercent = 0.5 * TotalCover)

  Dominants <- AbsoluteSpeciesCover%>%
    dplyr::group_by(!!!level)%>%
    arrange(!!!level, desc(SpeciesCover))%>%
    left_join(Totals)%>%
    dplyr::mutate(PreviousCumulativeCover = cumsum(SpeciesCover)-SpeciesCover,
                  Dominant = ifelse(PreviousCumulativeCover < FiftyPercent, "50", ifelse(SpeciesCover > TwentyPercent, "20", "N")),
                  HydroDominant = ifelse(HydroFAC %in% c("FAC", "FACW","OBL"), Dominant, "N"),
                  WeightedPrevalence = case_when(HydroFAC == "UPL" ~ SpeciesCover * 5,
                                         HydroFAC == "FACU" ~ SpeciesCover * 4,
                                         HydroFAC == "FAC" ~ SpeciesCover * 3,
                                         HydroFAC == "FACW" ~ SpeciesCover * 2,
                                         HydroFAC == "OBL" ~ SpeciesCover * 1))

  PlotDominanceTest <- Dominants%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::mutate(HydroDominantCount = ifelse(HydroDominant %in% c("50", "20"), 1, 0),
                  DominantCount = ifelse(Dominant %in% c("50", "20"), 1, 0))%>%
    dplyr::summarize(TotalHydroDominants = sum(HydroDominantCount),
                     TotalDominants = sum(DominantCount))%>%
    dplyr::left_join(UnknownCover)%>%
    dplyr::relocate(AbsoluteUnknownCover, .before = TotalHydroDominants)%>%
    dplyr::mutate(PercentHydroDominants = TotalHydroDominants/TotalDominants,
                  DominanceTest = ifelse(PercentHydroDominants >= 0.5, "Y", "N"))

  #Prevalence Test
  PlotPrevalenceTest <- Dominants %>%
    dplyr::filter(HydroFAC != "")%>%
    dplyr::group_by(EvaluationID)%>%
    dplyr::summarize(TotalCover = sum(SpeciesCover),
                     TotalPrevalence = sum(WeightedPrevalence, na.rm = T),
                     PrevalenceIndex = sum(WeightedPrevalence)/sum(SpeciesCover))%>%
    dplyr::mutate(PrevalenceTest = ifelse(PrevalenceIndex <= 3, "Y", "N"))

  AllDominanceTests <- dplyr::left_join(PlotDominanceTest,
                   PlotPrevalenceTest)%>%
    dplyr::left_join(., pct_HydroFACCover(header, lpi_tall, masterspecieslist, covertype = "relative"))%>%
    dplyr::left_join(pct_HydroFACCover(header, lpi_tall, masterspecieslist, covertype = "absolute"))

  return(AllDominanceTests)

}