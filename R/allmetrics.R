#'Combine species-level indicator information into one data.frame.
#'@param header data.frame produced by \code{gather_header_lentic}.
#'@param spp_inventory data.frame produced by \code{gather_spp_inventory_lentic}.
#'@param lpi_tall tall data.frame of LPI detail data produced by \code{gather_lpi_lentic}
#'@param height_tall tall data.frame of height data.
#'@param woody_tall tall data.frame of woody use data.
#'@param annualuse_tall tall data.frame of annual use data.
#'@param hummocks tall data frame of hummock data.
#'@param unknowncodelist optional dataframe. Unknown species list matching unknown codes to their duration and
#'Growth habit. This is used to fill in duration and growth habit for plants in LPI never identified to a
#'species or genus with those fields specified. If argument is unused, all unknown species without duration or
#'Growth Habit specified will be filtered out before being passed on to pct_cover_lentic.
#'@param masterspecieslist data.frame containing the full species list.
#'@returns data.frame of species-based indicator data.


#'@export allmetrics_byspecies
#'@rdname allmetrics
allmetrics_byspecies <- function(header, spp_inventory, lpi_tall, height_tall, woody_tall, annualuse_tall, masterspecieslist, unknowncodelist){

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

  if(!missing(unknowncodelist)){
    SpeciesList <- SpeciesList%>%
      dplyr::left_join(., unknowncodelist%>%
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

  SpeciesHeight <- summarize_height(height_tall, woody_tall, method = "mean", by_species = T)

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
allmetrics_byplot <- function(header, spp_inventory, lpi_tall, height_tall, woody_tall, annualuse_tall, hummocks, unknowncodelist, masterspecieslist){

  absolutecovermetrics <- CombineAbsoluteCoverMetrics(header, lpi_tall, masterspecieslist, unknowncodelist)%>%
    dplyr::rename_with(stringr::str_replace,matches("Absolute"), "Absolute", "")

  communitymetrics <- Community_Metrics(header = header, spp_inventory = spp_inventory, lpi_tall = lpi_tall, masterspecieslist = masterspecieslist)

  heightmetrics <- summarize_height(height_tall, woody_tall, method = "mean")

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
