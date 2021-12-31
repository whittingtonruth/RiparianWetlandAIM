#' Pivoting functions to transform raw data tables from wide to tall format for use in indicator calculations.
#'
#' @description This group of functions allow you to transform Survey123 data uploaded to
#' AGOL from wide format to tall format. It contains seven functions for transforming species inventory,
#' unknown plants, LPI, heights from LPI and woody species, woody species, annual use, and hummocks detail tables.
#' @param dsn Character string. The full filepath and filename (including file extensions) of the geodatabase containing the table of interest.
#' @importFrom magrittr %>%
#' @name gather_RiparianWetland
#' @return tall Data frame containing the data from a detail table from a Riparian and Wetland AIM file geodatabase.
#'

#' @export gather_lpi_lentic
#' @rdname gather_riparianwetland
## Function to transform LPI data into tall format.
gather_lpi_lentic <- function(dsn){
  #read in LPI header and detail tables
  lpi_detail <- suppressWarnings(sf::st_read(dsn = dsn,
                                             layer = "lpiDetail",
                                             stringsAsFactors = F))


  lpi_header <- suppressWarnings(sf::st_read(dsn = dsn,
                                             layer = "LPI",
                                             stringsAsFactors = F)) %>%
    sf::st_drop_geometry()

  #Make a tall table of the hit and all point identifying information
  lpi_hits_tall <- lpi_detail %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::select(RecKey,
                  PointNbr,
                  PointLoc,
                  TopCanopy, dplyr::matches("^Lower"), SoilSurface) %>%

    tidyr::pivot_longer(
      cols = -c(RecKey, PointNbr, PointLoc),
      names_to = "layer",
      values_to = "code") %>%

    #remove all rows with NA values
    dplyr::filter(
      !is.na(code))

  #Make a tall table of checkbox data and remove all NAs
  lpi_chkbox_tall <- lpi_detail %>%
    dplyr::select(RecKey,
                  PointNbr,
                  PointLoc,
                  dplyr::matches("^Chkbox")) %>%
    tidyr::pivot_longer(
      cols = -c(RecKey, PointNbr, PointLoc),
      names_to = "layer",
      values_to = "Chkbox")

  #Remove Woody, Woody2, and Herbaceous from chkbox data
  lpi_chkbox_tall <- lpi_chkbox_tall%>%
    dplyr::filter(
      !(layer %in% c("ChkboxWoody",
                     "ChkboxWoody2", #used in 2020 but not in 2021, won't hurt to keep it in.
                     "ChkboxHerbaceous")))

  #Rename the checkbox layer names so they match
  lpi_chkbox_tall$layer <- gsub(lpi_chkbox_tall$layer,
                                pattern = "^Chkbox",
                                replacement = "")

  lpi_chkbox_tall$layer[lpi_chkbox_tall$layer == "Top"] <- "TopCanopy"
  lpi_chkbox_tall$layer[lpi_chkbox_tall$layer == "Basal"] <- "SoilSurface"

  #Make a tall table of unknown code key data and remove all NAs
  lpi_unknowncode_tall <- lpi_detail %>%
    dplyr::select(RecKey,
                  PointNbr,
                  PointLoc,
                  dplyr::matches("^UnknownCode.*Key$|^UnknownCode.*Key2$")) %>%
    tidyr::pivot_longer(
      cols = -c(RecKey, PointNbr, PointLoc),
      names_to = "layer",
      values_to = "UnknownCodeKey")%>%
    dplyr::filter(
      !layer %in% c("UnknownCodeWoodyKey",
                    "UnknownCodeWoodyKey2", #used in 2020 but not in 2021, won't hurt to keep it in.
                    "UnknownCodeHerbaceousKey",
                    "UnknownCodeStubbleKey")) #used in 2020 but not in 2021, won't hurt to keep it in.

  #replace layer names so they match
  lpi_unknowncode_tall$layer <- gsub(lpi_unknowncode_tall$layer,
                               pattern = "^UnknownCode|Key$",
                               replacement = "")

  lpi_unknowncode_tall$layer[lpi_unknowncode_tall$layer == "Top"] <- "TopCanopy"
  lpi_unknowncode_tall$layer[lpi_unknowncode_tall$layer == "Basal"] <- "SoilSurface"

  #join all three tables
  lpi_tall <- suppressWarnings(dplyr::left_join(lpi_hits_tall,
                                                lpi_chkbox_tall) %>%
                                 dplyr::left_join(., lpi_unknowncode_tall)%>%
                                 dplyr::left_join(x = dplyr::select(lpi_header, "PlotID", "EvaluationID", "LineKey":"LineLengthCM"),
                                                  by = c("LineKey"= "RecKey")))

  return(lpi_tall)
}

#' @export gather_species_inventory_lentic
#' @rdname gather_riparianwetland
gather_species_inventory_lentic <- function(dsn) {

  # Read in the files from the geodatabase
  species_inventory_detail <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "SpecRichDetail",
    stringsAsFactors = FALSE
    ))
  species_inventory_header <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "SpeciesInventory",
    stringsAsFactors = FALSE
    ))%>%
    sf::st_drop_geometry()

  # Make the species detail table tall
  species_detail_tall <- species_inventory_detail %>%
    dplyr::filter(!(Species %in% c("", NA))) %>%
    dplyr::select(
      "SpecRichDetailEvaluationID",
      "Species",
      "UnknownCodeKey",
      "abundance")

  # Join the detail table to the header and remove any NAs
  species_inventory_tall <- dplyr::left_join(
    x = dplyr::select(species_inventory_header,
                      "PlotID":"EvaluationID", "FormDate", "Observer"),
    y = species_detail_tall,
    by = c("EvaluationID" = "SpecRichDetailEvaluationID")
  ) %>%
    subset(!is.na(Species))

  return(species_inventory_tall)
}

#' @export gather_unknowns_lentic
#' @rdname gather_riparianwetland
gather_unknowns_lentic <- function(dsn) {

  # Read in the files from the geodatabase
  UnknownPlants_detail <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "UnknownCodes",
    stringsAsFactors = FALSE
  ))
  UnknownPlants_header <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "UnknownPlants",
    stringsAsFactors = FALSE
  )%>%
    sf::st_drop_geometry())

  # Make the species detail table tall
  unknown_detail_tall <- UnknownPlants_detail %>%
    dplyr::filter(IdentificationStatus == "Lower Level Final") %>%
    dplyr::select(
      "UnknownCodesEvaluationID",
      "UnknownCodeKey",
      "GrowthHabit",
      "Duration",
      "ScientificName"
    )

  # Join the detail table to the header and remove any NAs
  UnknownPlants_tall <- dplyr::right_join(
    x = dplyr::select(UnknownPlants_header,
                      "PlotID":"VisitDate"),
    y = unknown_detail_tall,
    by = c("EvaluationID" = "UnknownCodesEvaluationID"),
    )

  UnknownPlants_tall$Duration[is.na(UnknownPlants_tall$Duration)] <- ""

  return(UnknownPlants_tall)
}

#' @export gather_height_lentic
#' @rdname gather_riparianwetland
gather_height_lentic <- function(dsn){
  # Read in LPI files from geodatabase
  lpi_detail <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "LPIDetail",
    stringsAsFactors = FALSE
  ))
  lpi_header <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "LPI",
    stringsAsFactors = FALSE
  ))%>%
    sf::st_drop_geometry()

  # We only want to carry a subset of the lpi_header fields forward
  lpi_header <- dplyr::select(lpi_header,
                              PlotID,
                              EvaluationID,
                              LineKey:LineLengthCM)

  lpi_height_tall_woody <- dplyr::select(
    .data = lpi_detail,
    RecKey,
    PointLoc,
    PointNbr,
    dplyr::matches("Woody$|WoodyKey$|^WoodyHeightClass$")
  ) %>% dplyr::mutate(type = "Woody", GrowthHabit_measured = "Woody")
  # Strip out the extra name stuff so woody and herbaceous variable names match
  names(lpi_height_tall_woody) <- stringr::str_replace_all(
    string = names(lpi_height_tall_woody),
    pattern = "Woody",
    replacement = ""
  )

  lpi_height_tall_herb <- dplyr::select(
    .data = lpi_detail,
    PointLoc,
    PointNbr,
    RecKey,
    dplyr::matches("Herbaceous$|HerbaceousKey$")
  ) %>% dplyr::mutate(type = "Herbaceous", GrowthHabit_measured = "Herbaceous")
  names(lpi_height_tall_herb) <- stringr::str_replace_all(
    string = names(lpi_height_tall_herb),
    pattern = "Herbaceous",
    replacement = ""
  )

  lpi_depth_litter <- lpi_detail %>%
    dplyr::select(PointLoc,
                  PointNbr,
                  RecKey,
                  LitterOrThatchDepth,
                  LitterType)%>%
    dplyr::mutate(GrowthHabit_measured = "LitterThatch")%>%
    dplyr::rename(type = LitterType,
                  Height = LitterOrThatchDepth)

  lpi_depth_water <- lpi_detail %>%
    dplyr::select(PointLoc,
                  PointNbr,
                  RecKey,
                  WaterDepth)%>%
    dplyr::mutate(type = "Water",
                  GrowthHabit_measured = "Water")%>%
    dplyr::rename(Height = WaterDepth)

  #Merge all three plant height tables together
  lpi_height <- dplyr::bind_rows(
    lpi_height_tall_woody,
    lpi_height_tall_herb,
    lpi_depth_litter,
    lpi_depth_water
  ) %>% dplyr::full_join(
    x = lpi_header, y = ., by = c("LineKey" = "RecKey")) %>%
    dplyr::select(-c(CollectionNumber, UnknownCode))%>%
    subset(., !is.na(Height))

  # Output the woody/herbaceous level data
  return(lpi_height)
}

#' @export gather_annualuse
#' @rdname gather_riparianwetland
gather_annualuse <- function(dsn){
  annualuse_detail <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "AnnualUsePointsRepeat",
    stringsAsFactors = FALSE
  ))
  annualuse_header <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "WoodyStructureAnnualUse",
    stringsAsFactors = FALSE
  ))%>%
    sf::st_drop_geometry()

  # We only want to carry a subset of the annualuse_header fields forward
  annualuse_header <- dplyr::select(annualuse_header,
                              PlotID,
                              EvaluationID,
                              LineKey:AnnualUseCollected,
                              interval)

  annualuse_tall <- annualuse_detail %>%
    dplyr::select(
      AnnualUsePointsEvaluationID,
      RecKey,
      PointNbr,
      PointLoc,
      StubbleHeightDominantSpecies,
      UnknownCodeStubbleKey,
      StubbleHeight,
      Grazed,
      SoilAlteration
    ) %>%
    dplyr::filter(StubbleHeightDominantSpecies != "")

  annualuse <- dplyr::left_join(x = annualuse_header,
                                y = annualuse_tall,
                                by = c("LineKey" = "RecKey", "EvaluationID" = "AnnualUsePointsEvaluationID"))
  return(annualuse)
}

#' @export gather_woodyspecies
#' @rdname gather_riparianwetland
gather_woodyspecies <- function(dsn){

  woody_header <- suppressWarnings(
    sf::st_read(dsn = dsn,
                layer = "WoodyStructureAnnualUse",
                stringsAsFactors = F))%>%
    dplyr::select(PlotID,
                  EvaluationID,
                  LineKey:AnnualUseCollected,
                  interval)%>%
    sf::st_drop_geometry()

  woody_detail <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "WoodyStructureRepeat",
    stringsAsFactors = F
  ))%>%
    dplyr::select(WoodyStructureEvaluationID,
                  WoodyStructureRecKey,
                  WoodyStructurePointNbr,
                  RiparianWoodySpecies,
                  UnknownCodeKey:HeightClass)

  woody_tall <- woody_header%>%
    dplyr::right_join(., woody_detail,
                     by = c("EvaluationID" = "WoodyStructureEvaluationID",
                            "LineKey" = "WoodyStructureRecKey")
  )

  return(woody_tall)
}

#' @export gather_hummocks
#' @rdname gather_riparianwetland
gather_hummocks <- function(dsn){

  hummocks_header <- suppressWarnings(
    sf::st_read(dsn = dsn,
              layer = "Hummocks",
              stringsAsFactors = F)%>%
      sf::st_drop_geometry())%>%
    dplyr::select(PlotID:LineKey,
                  HummocksPresentLine)

  hummocks_detail <- suppressWarnings(
    sf::st_read(dsn = dsn,
                layer = "HummockDetail",
                stringsAsFactors = F))%>%
    dplyr::select(HummockDetailEvaluationID:VegCover)

  hummocks <- dplyr::left_join(hummocks_header,
                               hummocks_detail,
                               by = c("EvaluationID" = "HummockDetailEvaluationID", "LineKey" = "RecKey"))

  return(hummocks)
}
