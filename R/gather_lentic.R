#' Gathering functions
#'
#' @description This group of functions allow you to transform Survey123 data uploaded to AGOL from wide format to long format.
#' @param dsn Character string. The full filepath and filename (including file extensions) of the geodatabase containing the table of interest.
#' @importFrom magrittr %>%
#' @name gather_lentic
#' @return Data frame containing the data from a lentic AIM form
#'

#' @export gather_lpi_lentic
#' @rdname gather_lentic
gather_lpi_lentic <- function(dsn) {

  # Read LPI information from AGOL fgdb
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

  # Make a tall data frame with the hit codes by layer and the checkbox designation
  lpi_hits_tall <- lpi_detail %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::select(
      "RecKey",
      "PointNbr",
      "PointLoc",
      "TopCanopy",
      "SoilSurface", dplyr::matches("^Lower")
    ) %>%

    tidyr::gather(
      key = "layer",
      value = "code",
      "TopCanopy", "SoilSurface", dplyr::matches("^Lower")
    )

    # Remove all records where no hit was recorded (e.g., "None", "NA")
  lpi_hits_tall <- dplyr::filter(
    .data = lpi_hits_tall,
    !is.na(code),
    code != "",
    code != "None",
    !is.na(RecKey)
  )

  ## Make a tall data frame of the checkbox status by layer
  lpi_chkbox_tall <- lpi_detail %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::select(
      "RecKey",
      "PointNbr",
      "PointLoc",
      dplyr::matches("^Chkbox")
    ) %>%

    tidyr::gather(
      key = "layer",
      value = "chkbox",
      dplyr::matches("^Chkbox")
    )

  # Remove woody and herbaceous checkboxes
  lpi_chkbox_tall <- lpi_chkbox_tall[!(lpi_chkbox_tall$layer %in%
                                         c("ChkboxWoody",
                                           "ChkboxWoody2",
                                           "ChkboxHerbaceous"
                                         )), ]

  ## Make the names in the layer variable match
  lpi_chkbox_tall$layer <- gsub(lpi_chkbox_tall$layer,
                                pattern = "^Chkbox",
                                replacement = ""
                                )

  lpi_chkbox_tall$layer[lpi_chkbox_tall$layer == "Top"] <- "TopCanopy"
  lpi_chkbox_tall$layer[lpi_chkbox_tall$layer == "Basal"] <- "SoilSurface"

  # Print update because this can take a while
  message("Merging LPI Header and LPI Detail tables")

  # Merge checkbox and hit data as well as the header data
  lpi_tall <- suppressWarnings(dplyr::left_join(
    x = lpi_hits_tall,
    y = lpi_chkbox_tall,
    all.x = TRUE,
    by = c("RecKey", "PointLoc", "PointNbr", "layer")
  ) %>%
    dplyr::left_join(
      x = dplyr::select(
        lpi_header, "PlotID", "PlotKey", "LineKey":"LineLengthCM"
      ),
      y = .,
      by = c("LineKey" = "RecKey")
    )
  )

  ## Output the list
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
      "Species")


  # Join the detail table to the header and remove any NAs
  species_inventory_tall <- dplyr::left_join(x = species_inventory_header,
                                             y = species_detail_tall,
                                             by = c("PlotKey" = "RecKey")
  ) %>%
    dplyr::select(., -globalid) %>%
    subset(!is.na(Species))

  return(species_inventory_tall)
}

 #' @export gather_height_lentic
#' @rdname gather_lentic
gather_height_lentic <- function(dsn){
  # Make sure geodatabase file exists
  if (!file.exists(dsn)) {
    stop("dsn must be a valid filepath to a geodatabase containg LPIDetail and LPI")
  }

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
  ))

  # We only want to carry a subset of the lpi_header fields forward
  lpi_header <- dplyr::select(lpi_header, PlotID, PlotKey, LineKey:LineLengthCM)

  lpi_height_tall_woody <- dplyr::select(
    .data = lpi_detail,
    PointLoc,
    PointNbr,
    RecKey,
    dplyr::matches("Woody$")
  ) %>% dplyr::mutate(type = "woody", GrowthHabit_measured = "Woody")
  # Strip out the extra name stuff so woody and herbacious variable names match
  names(lpi_height_tall_woody) <- stringr::str_replace_all(
    string = names(lpi_height_tall_woody),
    pattern = "Woody$",
    replacement = ""
  )

  lpi_height_tall_woody2 <- dplyr::select(
    .data = lpi_detail,
    PointLoc,
    PointNbr,
    RecKey,
    dplyr::matches("Woody2$")
  ) %>% dplyr::mutate(type = "woody2", GrowthHabit_measured = "Woody")
  # Strip out the extra name stuff so woody and herbacious variable names match
  names(lpi_height_tall_woody2) <- stringr::str_replace_all(
    string = names(lpi_height_tall_woody2),
    pattern = "Woody2$",
    replacement = ""
  )

  lpi_height_tall_herb <- dplyr::select(
    .data = lpi_detail,
    PointLoc,
    PointNbr,
    RecKey,
    dplyr::matches("Herbaceous$")
  ) %>% dplyr::mutate(type = "NonWoody", GrowthHabit_measured = "NonWoody")
  names(lpi_height_tall_herb) <- stringr::str_replace_all(
    string = names(lpi_height_tall_herb),
    pattern = "Herbaceous$",
    replacement = ""
  )

  #Merge all three together
  lpi_height <- rbind(
    lpi_height_tall_woody,
    lpi_height_tall_woody2,
    lpi_height_tall_herb
  ) %>% dplyr::full_join(
    x = lpi_header, y = ., by = c("LineKey" = "RecKey")) %>%
    subset(., !is.na(Height))

  # Add NA to fields with no species (i.e. the species that were "None")
  lpi_height$Species[lpi_height$Species=="N"] <- NA

  # Remove orphaned records and duplicates, if they exist
  lpi_height <- unique(lpi_height)
  lpi_height <- lpi_height[!is.na(lpi_height$globalid)]

  # Remove the globalid from the form
  lpi_height <- dplyr::select(lpi_height, -globalid)

  # Output the woody/herbaceous level data
  return(lpi_height)
}
