#'Combine species-level indicator information into one data.frame.
#'
#'Functions starting with tall table outputs from gather functions to calculate all metrics.
#'\code{CombineRelativeCoverMetrics()} and \code{CombineRelativeCoverMetrics()} focus on cover metrics.
#'Together with \code{Community_Metrics()}, these functions feed into final wrapping functions to create
#'two indicator tables ready to be incorperated into the final dataset, Indicators and SpeciesIndicators.
#'These data tables are created with \code{allmetrics_byplot()} and \code{allmetrics_byspecies()}.
#'
#'@param header Data frame. Use the data frame from the \code{header_build_lentic()} output.
#'@param spp_inventory Data frame. Use the data frame from the \code{gather_spp_inventory_lentic()} output.
#'@param lpi_tall Data frame. Use the data frame from the \code{gather_lpi_lentic()} output.
#'@param height_tall Data frame. Use the data frame from the \code{gather_heights_lentic()} output.
#'@param woody_tall Data frame. Use the data frame from the \code{gather_woodyspecies()} output.
#'@param annualuse_tall Data frame. Use the data frame from the \code{gather_annualuse()} output.
#'@param hummocks Data frame. Use the data frame from the \code{gather_hummocks()} output.
#'@param unknowncodes Optional dataframe. Use the data frame from the \code{gather_unknowns_lentic()} output.
#'Unknown species list matching unknown codes to their duration and Growth habit. This is used to fill in duration
#'and growth habit for plants in LPI never identified to a species or genus with those fields specified. If
#'argument is unused, all unknown species without duration or Growth Habit specified will be filtered out
#'before being passed on to \code{pct_cover_lentic()}.
#'@param masterspecieslist Data frame. The centrally managed master species list should be used.
#'@returns data.frame of species-based indicator data.


#'@export CombineRelativeCoverMetrics
#'@rdname allmetrics
CombineRelativeCoverMetrics <- function(header, lpi_tall, masterspecieslist, unknowncodes){

  TotalAbsolute <- pct_TotalAbsoluteCover(lpi_tall)

  RelativeNative <- pct_NativeCover(lpi_tall, masterspecieslist, covertype = "relative")

  RelativeNoxious <- pct_NoxiousCover(header, lpi_tall, masterspecieslist, covertype = "relative")

  RelativeHydro <- pct_HydrophyteCover(header, lpi_tall, masterspecieslist, covertype = "relative")

  RelativeHydroFAC <- pct_HydroFACCover(header, lpi_tall, masterspecieslist, covertype = "relative")

  RelativeGrowthHabit <- pct_GrowthHabitCover(lpi_tall, masterspecieslist, covertype = "relative", unknowncodes)

  RelativeDuration <- pct_DurationCover(lpi_tall, masterspecieslist, covertype = "relative", unknowncodes)

  NonPlantCover <- left_join(pct_NonPlantGroundCover(lpi_tall, hit = "any")%>%dplyr::select(PlotKey,
                                                                                            TotalLitterThatchCover,
                                                                                            TotalMossCover,
                                                                                            TotalRockCover,
                                                                                            TotalWaterCover),
                             pct_NonPlantGroundCover(lpi_tall, hit = "first")%>%dplyr::select(PlotKey,
                                                                                              BareSoilCover,
                                                                                              `BareOrganicMaterialCover`),
                             by = "PlotKey"
  )

  LPI_Cover_Indicators <- TotalAbsolute %>% dplyr::right_join(header%>%dplyr::select(PlotID,
                                                                                     PlotKey,
                                                                                     SiteName,
                                                                                     AdminState,
                                                                                     VisitDate,
                                                                                     LatWGS,
                                                                                     LongWGS),
                                                              .,
                                                              by = "PlotKey")%>%
    dplyr::left_join(., RelativeNative, by = "PlotKey")%>%
    dplyr::left_join(., RelativeNoxious, by = "PlotKey")%>%
    dplyr::left_join(., RelativeHydro, by = "PlotKey")%>%
    dplyr::left_join(., RelativeHydroFAC, by = "PlotKey")%>%
    dplyr::left_join(., RelativeGrowthHabit, by = "PlotKey")%>%
    dplyr::left_join(., RelativeDuration, by = "PlotKey")%>%
    dplyr::left_join(., NonPlantCover, by = "PlotKey")

  return(LPI_Cover_Indicators)
}

#'@export CombineAbsoluteCoverMetrics
#'@rdname allmetrics
CombineAbsoluteCoverMetrics <- function(header, lpi_tall, masterspecieslist, unknowncodes){

  Foliar <- pct_FoliarCover(lpi_tall)

  Basal <- pct_BasalCover(lpi_tall)

  AbsoluteNative <- pct_NativeCover(lpi_tall, masterspecieslist, covertype = "absolute")

  AbsoluteNoxious <- pct_NoxiousCover(header, lpi_tall, masterspecieslist, covertype = "absolute")

  AbsoluteHydro <- pct_HydrophyteCover(header, lpi_tall, masterspecieslist, covertype = "absolute")

  AbsoluteHydroFAC <- pct_HydroFACCover(header, lpi_tall, masterspecieslist, covertype = "absolute")

  AbsoluteGrowthHabit <- pct_GrowthHabitCover(lpi_tall, masterspecieslist, covertype = "absolute", unknowncodes)

  AbsoluteDuration <- pct_DurationCover(lpi_tall, masterspecieslist, covertype = "absolute", unknowncodes)

  NonPlantCover <- left_join(pct_NonPlantGroundCover(lpi_tall, hit = "any")%>%dplyr::select(PlotKey,
                                                                                            TotalLitterThatchCover,
                                                                                            TotalMossCover,
                                                                                            TotalRockCover,
                                                                                            TotalWaterCover),
                             pct_NonPlantGroundCover(lpi_tall, hit = "first")%>%dplyr::select(PlotKey,
                                                                                              BareSoilCover,
                                                                                              `BareOrganicMaterialCover`),
                             by = "PlotKey"
  )

  LPI_AbsoluteCover_Metrics <- Foliar %>% dplyr::right_join(header%>%dplyr::select(PlotID,
                                                                                   PlotKey,
                                                                                   SiteName,
                                                                                   AdminState,
                                                                                   VisitDate,
                                                                                   LatWGS,
                                                                                   LongWGS),
                                                            .,
                                                            by = "PlotKey")%>%
    dplyr::left_join(., Basal, by = "PlotKey")%>%
    dplyr::left_join(., AbsoluteNative, by = "PlotKey")%>%
    dplyr::left_join(., AbsoluteNoxious, by = "PlotKey")%>%
    dplyr::left_join(., AbsoluteHydro, by = "PlotKey")%>%
    dplyr::left_join(., AbsoluteHydroFAC, by = "PlotKey")%>%
    dplyr::left_join(., AbsoluteGrowthHabit, by = "PlotKey")%>%
    dplyr::left_join(., AbsoluteDuration, by = "PlotKey")%>%
    dplyr::left_join(., NonPlantCover, by = "PlotKey")

  return(LPI_AbsoluteCover_Metrics)
}

#'@export Community_Metrics
#'@rdname allmetrics
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
                                                                 AdminState,
                                                                 VisitDate,
                                                                 LatWGS,
                                                                 LongWGS),
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

#'@export allmetrics_byspecies
#'@rdname allmetrics
allmetrics_byspecies <- function(header, spp_inventory, lpi_tall, height_tall, woody_tall, annualuse_tall, masterspecieslist, unknowncodes){

  SpeciesList <- dplyr::left_join(header, spp_inventory, by = c("PlotID", "PlotKey"))%>%
    dplyr::left_join(., masterspecieslist, by = c("Species" = "Symbol"))%>%
    dplyr::group_by(PlotKey)%>%
    dplyr::filter(!(duplicated(UnknownCodeKey) & Species.y %in% c(NA, "")) &
                    !(duplicated(Species) & !(Species.y %in% c(NA, ""))))%>%
    dplyr::select(PlotKey,
                  PlotID,
                  SiteName,
                  AdminState,
                  Region,
                  LatWGS,
                  LongWGS,
                  Elevation,
                  Species,
                  UnknownCodeKey,
                  Scientific.Name,
                  Species.y,
                  GrowthHabit = type,
                  GrowthHabitSub,
                  Duration,
                  ends_with("_NOX"),
                  ends_with("_C.Value"),
                  ends_with("WetStatus"),
                  abundance)%>%
    tibble::add_column(., Noxious = NA,
                       CValue = NA,
                       WetStatus = NA)

  #Add a C-Value based on AdminState.
  for (i in 1:nrow(SpeciesList)){
    C.Valuelist <- paste(SpeciesList$AdminState[i], "_C.Value", sep = "")
    StateC.Value <- SpeciesList[[i,C.Valuelist]]
    SpeciesList$CValue[i] <- StateC.Value
  }

  #Add a Noxious designation by state.
  for (i in 1:nrow(SpeciesList)){
    noxiouslist <- paste(SpeciesList$AdminState[i], "_NOX", sep = "")
    statenoxious <- SpeciesList[[i,noxiouslist]]
    SpeciesList$Noxious[i] <- ifelse(statenoxious != "" & !is.na(statenoxious), "Noxious", "")
  }

  if(!missing(unknowncodes)){
    SpeciesList <- SpeciesList%>%
      dplyr::left_join(., unknowncodes%>%
                         dplyr::rename(GrowthHabitUnknown = GrowthHabit, DurationUnknown = Duration),
                       by = c("PlotKey", "PlotID", "UnknownCodeKey"))%>%
      dplyr::mutate(Duration = ifelse(Duration==""|is.na(Duration), DurationUnknown, Duration),
                    GrowthHabitSub = ifelse(GrowthHabitSub==""|is.na(GrowthHabitSub), GrowthHabitUnknown, GrowthHabitSub))%>%
      dplyr::select(-c(VisitDate:ScientificName))
  }

  #Add a WetlandIndicatorStatus based on Region. First change all the species statuses that are blank to NR
  SpeciesList <- SpeciesList%>%
    dplyr::mutate(AW_WetStatus = ifelse(Species.y!=""&AW_WetStatus=="","NR",AW_WetStatus))%>%
    dplyr::mutate(WMVC_WetStatus = ifelse(Species.y!=""&WMVC_WetStatus=="","NR", WMVC_WetStatus))%>%
    dplyr::mutate(WetStatus = ifelse(Region=="Arid West", AW_WetStatus, WMVC_WetStatus))%>%
    dplyr::select(-c(ends_with("_NOX"), ends_with("_C.Value"), ends_with("_WetStatus"), Species.y))

  SpeciesCover <- pct_AbsoluteSpeciesCover(lpi_tall, masterspecieslist)

  SpeciesHeight <- height_metrics(height_tall, woody_tall, method = "mean", by_species = T)

  SpeciesAnnualUse <- use_metrics(header, annualuse_tall, woody_tall, masterspecieslist, by_species = T)

  SpeciesAgeClass <- ageclass_metrics(header, woody_tall, masterspecieslist, by_species = T)

  SpeciesList <- SpeciesList%>%
    dplyr::left_join(., SpeciesCover, by = c("PlotID", "PlotKey", "Species" = "Code", "Scientific.Name", "UnknownCodeKey"))%>%
    dplyr::left_join(., SpeciesHeight, by = c("PlotID", "PlotKey", "Species"))%>%
    dplyr::left_join(., SpeciesAnnualUse, by = c("PlotID", "PlotKey", "Species"))%>%
    dplyr::left_join(., SpeciesAgeClass, by = c("PlotID", "PlotKey","Species"= "RiparianWoodySpecies"))

  return(SpeciesList)

}

#'@export allmetrics_byplot
#'@rdname allmetrics
allmetrics_byplot <- function(header, spp_inventory, lpi_tall, height_tall, woody_tall, annualuse_tall, hummocks, unknowncodes, masterspecieslist){

  absolutecovermetrics <- CombineAbsoluteCoverMetrics(header, lpi_tall, masterspecieslist, unknowncodes)%>%
    dplyr::rename_with(stringr::str_replace,matches("Absolute"), "Absolute", "")

  communitymetrics <- Community_Metrics(header = header, spp_inventory = spp_inventory, lpi_tall = lpi_tall, masterspecieslist = masterspecieslist)

  heightmetrics <- height_metrics(height_tall, woody_tall, method = "mean")

  ageclassmetrics <- ageclass_metrics(header, woody_tall, masterspecieslist)

  usemetrics <- use_metrics(header, annualuse_tall, woody_tall, masterspecieslist)

  hummocksmetrics <- hummocks_metrics(hummocks)

  allmetrics <- suppressMessages(
    dplyr::left_join(header, communitymetrics)%>%
    dplyr::left_join(., absolutecovermetrics)%>%
    dplyr::left_join(., heightmetrics)%>%
    dplyr::left_join(., ageclassmetrics)%>%
    dplyr::left_join(., usemetrics)%>%
    dplyr::left_join(., hummocksmetrics)
  )

  allmetrics <- allmetrics%>%
    dplyr::mutate(CountHummocks = ifelse(is.na(CountHummocks), 0, CountHummocks),
                  PctHummocks = ifelse(is.na(PctHummocks), 0, PctHummocks))

  return(allmetrics)
}