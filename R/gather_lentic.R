#' Species gathering functions
#'
#' @description This group of functions allow you to transform Survey123 data uploaded to
#' AGOL from wide format to long format. It contains six functions for transforming Species Richness,
#' Unknown Plants, LPI, Heights from LPI, and Woody Species detail tables.
#' @param dsn Character string. The full filepath and filename (including file extensions) of the geodatabase containing the table of interest.
#' @importFrom magrittr %>%
#' @name gather_lentic
#' @return Data frame containing the data from a lentic AIM form
#'

#' @export gather_lpi_lentic
#' @rdname gather_lentic
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
                     "ChkboxWoody2",
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
                    "UnknownCodeWoodyKey2",
                    "UnknownCodeHerbaceousKey",
                    "UnknownCodeStubbleKey"))

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
                                 dplyr::left_join(x = dplyr::select(lpi_header, "PlotID", "PlotKey", "LineKey":"LineLengthCM"),
                                                  by = c("LineKey"= "RecKey")))

  return(lpi_tall)
}

#' @export gather_species_inventory_lentic
#' @rdname gather_lentic
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
    ))

  # Make the species detail table tall
  species_detail_tall <- species_inventory_detail %>%
    dplyr::filter(!(Species %in% c("", NA))) %>%
    dplyr::select(
      "RecKey",
      "Species",
      "UnknownCode",
      "abundance")

  # Join the detail table to the header and remove any NAs
  species_inventory_tall <- dplyr::left_join(
    x = dplyr::select(species_inventory_header,
                      "PlotID":"CrewNumber", "Observer"),
    y = species_detail_tall,
    by = c("PlotKey" = "RecKey")
  ) %>%
    subset(!is.na(Species))

  return(species_inventory_tall)
}

#' @export gather_unknowns_lentic
#' @rdname gather_lentic
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
    dplyr::filter(IdentificationStatus == "Not Identified") %>%
    dplyr::select(
      "UnknownCodeKey",
      "GrowthHabit",
      "Duration",
      "ScientificName"
    )%>%
    dplyr::mutate("PlotKey" = paste(sapply(strsplit(UnknownCodeKey, "_"), '[', 1), sapply(strsplit(UnknownCodeKey, "_"), '[', 2), sep = "_"))

  # Join the detail table to the header and remove any NAs
  UnknownPlants_tall <- dplyr::left_join(
    x = dplyr::select(UnknownPlants_header,
                      "PlotID":"VisitDate"),
    y = unknown_detail_tall)

  UnknownPlants_tall$Duration[UnknownPlants_tall$Duration=="Unknown"] <- ""

  return(UnknownPlants_tall)
}

#' @export gather_height_lentic
#' @rdname gather_lentic
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
                              PlotKey,
                              LineKey:LineLengthCM)

  lpi_height_tall_woody <- dplyr::select(
    .data = lpi_detail,
    RecKey,
    PointLoc,
    PointNbr,
    dplyr::matches("Woody$|WoodyKey$")
  ) %>% dplyr::mutate(type = "woody", GrowthHabit_measured = "Woody")
  # Strip out the extra name stuff so woody and herbacious variable names match
  names(lpi_height_tall_woody) <- stringr::str_replace_all(
    string = names(lpi_height_tall_woody),
    pattern = "Woody",
    replacement = ""
  )

  lpi_height_tall_woody2 <- dplyr::select(
    .data = lpi_detail,
    PointLoc,
    PointNbr,
    RecKey,
    dplyr::matches("Woody2$|WoodyKey2$")
  ) %>% dplyr::mutate(type = "woody2", GrowthHabit_measured = "Woody")
  # Strip out the extra name stuff so woody and herbacious variable names match
  names(lpi_height_tall_woody2) <- stringr::str_replace_all(
    string = names(lpi_height_tall_woody2),
    pattern = "Woody2$|2$|Woody",
    replacement = ""
  )

  lpi_height_tall_herb <- dplyr::select(
    .data = lpi_detail,
    PointLoc,
    PointNbr,
    RecKey,
    dplyr::matches("Herbaceous$|HerbaceousKey$")
  ) %>% dplyr::mutate(type = "NonWoody", GrowthHabit_measured = "NonWoody")
  names(lpi_height_tall_herb) <- stringr::str_replace_all(
    string = names(lpi_height_tall_herb),
    pattern = "Herbaceous",
    replacement = ""
  )

  #Merge all three together
  lpi_height <- rbind(
    lpi_height_tall_woody,
    lpi_height_tall_woody2,
    lpi_height_tall_herb
  ) %>% dplyr::full_join(
    x = lpi_header, y = ., by = c("LineKey" = "RecKey")) %>%
    subset(., !is.na(Height))%>%
    dplyr::select(-c(CollectionNumber, UnknownCode))

  # Output the woody/herbaceous level data
  return(lpi_height)
}

#' @export gather_annualuse
#' @rdname gather_lentic
gather_annualuse <- function(dsn){
  lpi_detail <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "LPIDetail",
    stringsAsFactors = FALSE
  ))
  lpi_header <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "LPI",
    stringsAsFactors = FALSE
  ))

  # We only want to carry a subset of the lpi_header fields forward
  lpi_header <- dplyr::select(lpi_header,
                              PlotID,
                              PlotKey,
                              LineKey:LineLengthCM)

  annualuse_tall <- lpi_detail %>%
    dplyr::select(
      RecKey,
      PointNbr,
      PointLoc,
      StubbleHeightDominantSpecies,
      StubbleHeight,
      Grazed,
      SoilAlteration
    ) %>%
    dplyr::filter(StubbleHeightDominantSpecies != "")

  annualuse <- dplyr::left_join(x = lpi_header,
                                y = annualuse_tall,
                                by = c("LineKey" = "RecKey"))
  return(annualuse)
}

#' @export gather_woodyspecies
#' @rdname gather_lentic
gather_woodyspecies <- function(dsn){
  woody_header <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "WoodySpecies",
    stringsAsFactors = F
  ))
  woody_detail <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "WoodySpeciesRepeat",
    stringsAsFactors = F
  ))

  woody_header <- woody_header %>%
    dplyr::select(PlotID:Recorder,
                 Direction)

  woody_detail <- woody_detail %>%
    dplyr::select(RecKey,
                  RiparianWoodySpecies,
                  RiparianWoodySpeciesLiveDead,
                  Rhizomatous:UseClass
  )

  woody_tall <- dplyr::left_join(x = woody_header,
                                 y = woody_detail,
                                 by = c("LineKey" = "RecKey")
  )

  return(woody_tall)
}
