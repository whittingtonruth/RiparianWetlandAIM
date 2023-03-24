#' Pivoting functions to transform raw data tables from wide to tall format for use in indicator calculations.
#'
#' @description This group of functions allow you to transform Survey123 data uploaded to
#' AGOL from wide format to tall format. It contains seven functions for transforming species inventory,
#' unknown plants, LPI, heights from LPI and woody species, woody species, annual use, and hummocks detail tables.
#' @param dsn Character string. The full filepath and filename (including file extensions) of the geodatabase containing the table of interest.
#' @param source Character string. The source and schema of the data being analyzed. Default is SDE, but other options are AGOL and
#' @param familygenuslist data.frame. Only required in gathering Unknown Plant form for data loaded from the online feature service. Should be
#' an exhaustive list of all possible family and genus names ('ScientificName'), their associated codes ('Code'), and the taxonomic level
#' ('Level'), i.e. "Family" or "Genus".
#' @importFrom magrittr %>%
#' @name gather_RiparianWetland
#' @return tall Data frame containing the data from a detail table from a Riparian and Wetland AIM file geodatabase.
#'

#' @export gather_lpi_lentic
#' @rdname gather_riparianwetland
## Function to transform LPI data into tall format.
gather_lpi_lentic <- function(dsn, source = "SDE"){

  #read in LPI header and detail tables
  if(source == "GDB"){

    lpi_detail <- suppressWarnings(sf::st_read(dsn = dsn,
                                               layer = "lpiDetail",
                                               stringsAsFactors = F))


    lpi_header <- suppressWarnings(sf::st_read(dsn = dsn,
                                             layer = "LPI",
                                             stringsAsFactors = F)) %>%
      sf::st_drop_geometry()

    message("Gathering LPI data from GDB into LPI tall table. ")

  }

  else if(source == "AGOL"){
    fc <- arcgisbinding::arc.open(dsn)@children$FeatureClass
    rs <- arcgisbinding::arc.open(dsn)@children$Table

    lpi_header <- arc.data2sf(arc.select(arc.open(paste(dsn, fc[stringr::str_which(fc, "LPI")], sep = "/"))))%>%
      sf::st_drop_geometry()

    lpi_detail <- arc.select(arc.open(paste(dsn, rs[stringr::str_which(rs, "LPI")], sep = "/")))

    message("Gathering LPI data from a live ArcGIS Online feature service into LPI tall table. ")
  }
  else if(source == "SDE"){
    lpi_detail <- suppressWarnings(sf::st_read(dsn = dsn,
                                               layer = "F_LPIDetail",
                                               stringsAsFactors = F))


    lpi_header <- suppressWarnings(sf::st_read(dsn = dsn,
                                               layer = "F_LPI",
                                               stringsAsFactors = F)) %>%
      sf::st_drop_geometry()

    message("Gathering LPI data from the SDE into LPI tall table. ")
  }
  #Check that the source is one of the appropriate options
  else{
    stop("source must be 'SDE', 'GDB' or 'AGOL'.")
  }

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
gather_species_inventory_lentic <- function(dsn, source = "SDE") {

  #Load data from either .gdb or directly from a feature service on ArcGIS online.
  if(source == "GDB"){
    species_inventory_detail <- suppressWarnings(sf::st_read(
      dsn = dsn,
      layer = "SpecRichDetail",
      stringsAsFactors = FALSE
    ))%>%
      dplyr::rename("EvaluationID" = "SpecRichDetailEvaluationID")

    species_inventory_header <- suppressWarnings(sf::st_read(
      dsn = dsn,
      layer = "SpeciesInventory",
      stringsAsFactors = FALSE
    ))%>%
      sf::st_drop_geometry()

    message("Gathering Species Inventory from GDB. ")

  }
  else if(source == "AGOL"){
    fc <- arcgisbinding::arc.open(dsn)@children$FeatureClass
    rs <- arcgisbinding::arc.open(dsn)@children$Table

    species_inventory_header <- arc.data2sf(arc.select(arc.open(paste(dsn, fc[stringr::str_which(fc, "SpeciesInventory")], sep = "/"))))%>%
      sf::st_drop_geometry()

    species_inventory_detail <- arc.select(arc.open(paste(dsn, rs[stringr::str_which(rs, "SpecRichDetail")], sep = "/")))%>%
      dplyr::rename("EvaluationID" = "SpecRichDetailEvaluationID")

    message("Downloading and gathering Species Inventory from ArcGIS Online live feature service.")
  }
  else if(source == "SDE"){
    species_inventory_detail <- suppressWarnings(sf::st_read(
      dsn = dsn,
      layer = "F_SpecRichDetail",
      stringsAsFactors = FALSE
    ))

    species_inventory_header <- suppressWarnings(sf::st_read(
      dsn = dsn,
      layer = "F_SpeciesInventory",
      stringsAsFactors = FALSE
    ))

    message("Gathering Species Inventory from SDE.")
  }
  #Check that the source is one of the appropriate options
  else{
    stop("source must be 'SDE', 'GDB' or 'AGOL'.")
  }

  # Make the species detail table tall
  species_detail_tall <- species_inventory_detail %>%
    dplyr::filter(!(Species %in% c("", NA))) %>%
    dplyr::select(
      "EvaluationID",
      "Species",
      "UnknownCodeKey",
      "abundance")

  # Join the detail table to the header and remove any NAs
  species_inventory_tall <- dplyr::left_join(
    x = dplyr::select(species_inventory_header,
                      "PlotID":"EvaluationID", "FormDate", "Observer"),
    y = species_detail_tall,
    by = c("EvaluationID")
  ) %>%
    subset(!is.na(Species))

  return(species_inventory_tall)
}

#' @export gather_unknowns_lentic
#' @rdname gather_riparianwetland
gather_unknowns_lentic <- function(dsn, familygenuslist, source = "SDE") {

  # Read in the files from the geodatabase
  if(source == "GDB"){
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
    UnknownPlants_tall <- UnknownPlants_detail %>%
      dplyr::filter(IdentificationStatus == "Lower Level Final") %>%
      dplyr::select(
        "UnknownCodesEvaluationID",
        "UnknownCodeKey",
        "GrowthHabit",
        "Duration",
        "ScientificName")%>%
      # Join the detail table to the header and remove any NAs
      dplyr::right_join(x = UnknownPlants_header%>%dplyr::select("PlotID":"VisitDate"),
                        y = .,
                        by = c("EvaluationID" = "UnknownCodesEvaluationID"))

    UnknownPlants_tall$Duration[is.na(UnknownPlants_tall$Duration)] <- ""

    message("File Geodatabase data type is being downloaded and gathered into unknown plant table. Only unknown codes not identified to species will be maintained. ")

  }

  else if(source == "AGOL"){
    message("ArcGIS Online live feature service data type is being downloaded and gathered into unknown table. All unknown species will be maintained in the list for use in correcting uningested data. ")

    if(missing(familygenuslist)){
      stop("If loading during-season data, the unknown species list must be corrected with a provided genus and family code list. ")
    }

    fc <- arcgisbinding::arc.open(dsn)@children$FeatureClass
    rs <- arcgisbinding::arc.open(dsn)@children$Table

    UnknownPlants_header <- arc.data2sf(arc.select(arc.open(paste(dsn, fc[stringr::str_which(fc, "UnknownPlants")], sep = "/"))))%>%
      sf::st_drop_geometry()

    UnknownPlants_detail <- arc.select(arc.open(paste(dsn, rs[stringr::str_which(rs, "UnknownCodes")], sep = "/")))

    UnknownPlants_tall <- UnknownPlants_detail %>%
      dplyr::select(
        "UnknownCodesEvaluationID",
        "UnknownCodeKey",
        "GrowthHabit",
        "Duration",
        "Family",
        "Genus",
        "ScientificName",
        "IdentificationStatus"
      )%>%
      right_join(x = UnknownPlants_header%>%dplyr::select("PlotID":"VisitDate"),
                 y = .,
                 by = c("EvaluationID" = "UnknownCodesEvaluationID"))

    UnknownPlants_tall <- UnknownPlants_tall%>%
      filter(!is.na(UnknownCodeKey))%>%
      dplyr::left_join(.,
                       familygenuslist%>%dplyr::filter(Level=="Genus")%>%dplyr::select(GenusCode = Code, ScientificName),
                       by = c("Genus" = "ScientificName"))%>%
      dplyr::left_join(.,
                       familygenuslist%>%dplyr::filter(Level=="Family")%>%dplyr::select(FamilyCode = Code, ScientificName),
                       by = c("Family" = "ScientificName"))%>%
      dplyr::mutate(IdentificationStatus = ifelse(is.na(IdentificationStatus), "Not Identified", IdentificationStatus),
                    GrowthHabitCode = dplyr::case_when(GrowthHabit=="Graminoid" ~ "G",
                                                       GrowthHabit=="Forb" ~ "F",
                                                       GrowthHabit=="Shrub"~ "SH",
                                                       GrowthHabit=="Tree" ~ "TR",
                                                       is.na(GrowthHabit) ~ "UNK",
                                                       TRUE ~ "NV"),
                    DurationCode = dplyr::case_when(Duration == "Annual" ~ "A",
                                                    Duration == "Perennial" ~ "P",
                                                    TRUE ~ "U"),
                    IDLevel = dplyr::case_when(!is.na(ScientificName) ~ "Species",
                                               !is.na(Genus) & Genus != "" & Genus != "Unknown" ~ "Genus",
                                               !is.na(Family) & Family != "" & Family != "Unknown" ~ "Family",
                                               TRUE ~ "GrowthHabit"),
                    ScientificName = dplyr::case_when(!is.na(ScientificName) ~ ScientificName,
                                                      !is.na(Genus) & Genus != "" & Genus != "Unknown" ~ GenusCode,
                                                      !is.na(Family) & Family != "" & Family != "Unknown" ~ FamilyCode,
                                                      GrowthHabitCode == "G"|GrowthHabitCode=="F" ~ paste(DurationCode, GrowthHabitCode, sep = ""),
                                                      TRUE ~ GrowthHabitCode))%>%
      dplyr::select(-c(GrowthHabitCode, DurationCode, GenusCode, FamilyCode, Family, Genus))

  }
  else if(source == "SDE"){
    UnknownPlants_detail <- suppressWarnings(sf::st_read(
      dsn = dsn,
      layer = "F_UnknownCodes",
      stringsAsFactors = FALSE
    ))
    UnknownPlants_header <- suppressWarnings(sf::st_read(
      dsn = dsn,
      layer = "F_UnknownPlants",
      stringsAsFactors = FALSE
    ))

    # Make the species detail table tall
    UnknownPlants_tall <- UnknownPlants_detail %>%
      dplyr::filter(IdentificationStatus == "Lower Level Final") %>%
      dplyr::select(
        "EvaluationID",
        "UnknownCodeKey",
        "GrowthHabit",
        "Duration",
        "ScientificName")%>%
      # Join the detail table to the header and remove any NAs
      dplyr::right_join(x = UnknownPlants_header%>%dplyr::select("EvaluationID":"VisitDate"),
                        y = .,
                        by = c("EvaluationID"))

    UnknownPlants_tall$Duration[is.na(UnknownPlants_tall$Duration)] <- ""

    message("Unknown Plants data is being gathered into Unknown Plants table from the SDE. Only unknown codes not identified to species will be maintained. ")
  }
  #Check that the source is one of the appropriate options
  else{
    stop("source must be 'SDE', 'GDB' or 'AGOL'.")
  }

  return(UnknownPlants_tall)
}

#' @export gather_height_lentic
#' @rdname gather_riparianwetland
gather_height_lentic <- function(dsn, source = "SDE"){
  # Read in LPI files from geodatabase
  if(source == "GDB"){
    lpi_detail <- suppressWarnings(sf::st_read(dsn = dsn,
                                               layer = "lpiDetail",
                                               stringsAsFactors = F))


    lpi_header <- suppressWarnings(sf::st_read(dsn = dsn,
                                               layer = "LPI",
                                               stringsAsFactors = F)) %>%
      sf::st_drop_geometry()

    message("Gathering LPI data from GDB into LPI Heights tall table. ")

  }

  else if(source == "AGOL"){
    fc <- arcgisbinding::arc.open(dsn)@children$FeatureClass
    rs <- arcgisbinding::arc.open(dsn)@children$Table

    lpi_header <- arc.data2sf(arc.select(arc.open(paste(dsn, fc[stringr::str_which(fc, "LPI")], sep = "/"))))%>%
      sf::st_drop_geometry()

    lpi_detail <- arc.select(arc.open(paste(dsn, rs[stringr::str_which(rs, "LPI")], sep = "/")))

    message("Gathering LPI data from ArcGIS Online live feature service into LPI tall table. ")
  }
  else if(source == "SDE"){
    lpi_detail <- suppressWarnings(sf::st_read(dsn = dsn,
                                               layer = "F_LPIDetail",
                                               stringsAsFactors = F))


    lpi_header <- suppressWarnings(sf::st_read(dsn = dsn,
                                               layer = "F_LPI",
                                               stringsAsFactors = F)) %>%
      sf::st_drop_geometry()

    message("Gathering LPI data from the SDE into LPI tall table. ")
  }
  #Check that the source is one of the appropriate options
  else{
    stop("source must be 'SDE', 'GDB' or 'AGOL'.")
  }

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
gather_annualuse <- function(dsn, source = "SDE"){
  #read in LPI header and detail tables
  if(source == "GDB"){
    annualuse_detail <- suppressWarnings(sf::st_read(
      dsn = dsn,
      layer = "AnnualUsePointsRepeat",
      stringsAsFactors = FALSE
    ))%>%
      dplyr::rename("EvaluationID" = "AnnualUsePointsEvaluationID")

    annualuse_header <- suppressWarnings(sf::st_read(
      dsn = dsn,
      layer = "WoodyStructureAnnualUse",
      stringsAsFactors = FALSE
    ))%>%
      sf::st_drop_geometry()

    message("File Geodatabase data type is being downloaded and gathered into annual use tall table. ")

  }

  else if(source == "AGOL"){
    fc <- arcgisbinding::arc.open(dsn)@children$FeatureClass
    rs <- arcgisbinding::arc.open(dsn)@children$Table

    annualuse_header <- arc.data2sf(arc.select(arc.open(paste(dsn, fc[stringr::str_which(fc, "WoodyStructureAnnualUse")], sep = "/"))))%>%
      sf::st_drop_geometry()

    annualuse_detail <- arc.select(arc.open(paste(dsn, rs[stringr::str_which(rs, "AnnualUsePointsRepeat")], sep = "/")))%>%
      dplyr::rename("EvaluationID" = "AnnualUsePointsEvaluationID")

    message("ArcGIS Online live feature service data type is being downloaded and gathered into annual use tall table. ")
  }
  else if(source == "SDE"){
    annualuse_detail <- suppressWarnings(sf::st_read(
      dsn = dsn,
      layer = "F_AnnualUsePointsRepeat",
      stringsAsFactors = FALSE
    ))

    annualuse_header <- suppressWarnings(sf::st_read(
      dsn = dsn,
      layer = "F_WoodyStructureAnnualUse",
      stringsAsFactors = FALSE
    ))

    message("Gathering tables from the SDE into annual use tall table. ")
  }
  #Check that the source is one of the appropriate options
  else{
    stop("source must be 'SDE', 'GDB' or 'AGOL'.")
  }

  # We only want to carry a subset of the annualuse_header fields forward
  annualuse_header <- dplyr::select(annualuse_header,
                              PlotID,
                              EvaluationID,
                              LineKey:AnnualUseCollected,
                              interval)

  annualuse_tall <- annualuse_detail %>%
    dplyr::select(
      EvaluationID,
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
                                by = c("LineKey" = "RecKey", "EvaluationID"))
  return(annualuse)
}

#' @export gather_woodyspecies
#' @rdname gather_riparianwetland
gather_woodyspecies <- function(dsn, source = "SDE"){

  #read in woody header and detail tables
  if(source == "GDB"){
    woody_header <- suppressWarnings(
      sf::st_read(dsn = dsn,
                  layer = "WoodyStructureAnnualUse",
                  stringsAsFactors = F))%>%
      sf::st_drop_geometry()

    points_header <- suppressWarnings(
      sf::st_read(dsn = dsn,
                  layer = "AnnualUsePointsRepeat",
                  stringsAsFactors = F))%>%
      dplyr::rename("EvaluationID" = "AnnualUsePointsEvaluationID")

    woody_detail <- suppressWarnings(sf::st_read(
      dsn = dsn,
      layer = "WoodyStructureRepeat",
      stringsAsFactors = F
    ))%>%
      dplyr::rename("EvaluationID" = "WoodyStructureEvaluationID",
                    "RecKey" = "WoodyStructureRecKey",
                    "PointNbr" = "WoodyStructurePointNbr")

    message("File Geodatabase data type is being downloaded and gathered into Woody tall table. ")
  }

  else if(source == "AGOL"){
    fc <- arcgisbinding::arc.open(dsn)@children$FeatureClass
    rs <- arcgisbinding::arc.open(dsn)@children$Table

    woody_header <- arc.data2sf(arc.select(arc.open(paste(dsn, fc[stringr::str_which(fc, "WoodyStructureAnnualUse")], sep = "/"))))%>%
      sf::st_drop_geometry()

    points_header <- arc.select(arc.open(paste(dsn, rs[stringr::str_which(rs, "AnnualUsePointsRepeat")], sep = "/")))%>%
      dplyr::rename("EvaluationID" = "AnnualUsePointsEvaluationID")

    woody_detail <- arc.select(arc.open(paste(dsn, rs[stringr::str_which(rs, "WoodyStructureRepeat")], sep = "/")))%>%
      dplyr::rename("EvaluationID" = "WoodyStructureEvaluationID",
                    "RecKey" = "WoodyStructureRecKey",
                    "PointNbr" = "WoodyStructurePointNbr")

    message("ArcGIS Online live feature service data type is being downloaded and gathered into Woody tall table. ")
  }
  else if(source == "SDE"){
    woody_header <- suppressWarnings(
      sf::st_read(dsn = dsn,
                  layer = "F_WoodyStructureAnnualUse",
                  stringsAsFactors = F))

    points_header <- suppressWarnings(
      sf::st_read(dsn = dsn,
                  layer = "F_AnnualUsePointsRepeat",
                  stringsAsFactors = F))

    woody_detail <- suppressWarnings(sf::st_read(
      dsn = dsn,
      layer = "F_WoodyStructureRepeat",
      stringsAsFactors = F
    ))

    message("Gathering tables from the SDE into woody structure tall table. ")
  }
  #Check that the source is one of the appropriate options
  else{
    stop("source must be 'SDE', 'GDB' or 'AGOL'.")
  }

  woody_detail <- woody_detail%>%
    # dplyr::select(EvaluationID,
    #               RecKey,
    #               PointNbr,
    #               RiparianWoodySpecies,
    #               UnknownCodeKey:AgeClass)%>%
    dplyr::left_join(points_header%>%
                       dplyr::select(EvaluationID,
                                     RecKey,
                                     PointNbr),
                     .,
                     by = c("EvaluationID", "RecKey", "PointNbr"))

  woody_tall <- woody_header%>%
    dplyr::select(PlotID,
                  EvaluationID,
                  LineKey:WoodyStructureCollected,
                  interval,
                  WoodySpeciesPresent)%>%
    dplyr::left_join(., woody_detail,
                     by = c("EvaluationID", "LineKey" = "RecKey")
  )%>%
    dplyr::filter(WoodyStructureCollected == "Yes")

  return(woody_tall)
}

#' @export gather_hummocks
#' @rdname gather_riparianwetland
gather_hummocks <- function(dsn, source = "SDE"){

  if(source == "GDB"){
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
      dplyr::select("EvaluationID"="HummockDetailEvaluationID",
                    RecKey:VegCover)

    message("File Geodatabase data type is being downloaded and gathered into a hummock tall table. ")
  }

  else if(source == "AGOL"){
    fc <- arcgisbinding::arc.open(dsn)@children$FeatureClass
    rs <- arcgisbinding::arc.open(dsn)@children$Table

    hummocks_header <- arc.data2sf(arc.select(arc.open(paste(dsn, fc[stringr::str_which(fc, "Hummocks")], sep = "/"))))%>%
      sf::st_drop_geometry()%>%
      dplyr::select(PlotID:LineKey, HummocksPresentLine)

    hummocks_detail <- arc.select(arc.open(paste(dsn, rs[stringr::str_which(rs, "HummockDetail")], sep = "/")))%>%
      dplyr::select("EvaluationID"="HummockDetailEvaluationID",
                    RecKey:VegCover)

    message("ArcGIS Online live feature service data type is being downloaded and gathered into a hummock tall table. ")
  }
  else if(source == "SDE"){
    hummocks_header <- suppressWarnings(
      sf::st_read(dsn = dsn,
                  layer = "F_Hummocks",
                  stringsAsFactors = F)%>%
      dplyr::select(EvaluationID:LineKey,
                    HummocksPresentLine))

    hummocks_detail <- suppressWarnings(
      sf::st_read(dsn = dsn,
                  layer = "F_HummockDetail",
                  stringsAsFactors = F))

    message("Gathering tables from the SDE into woody structure tall table. ")
  }
  #Check that the source is one of the appropriate options
  else{
    stop("source must be 'SDE', 'GDB' or 'AGOL'.")
  }

  hummocks <- dplyr::left_join(hummocks_header,
                               hummocks_detail,
                               by = c("EvaluationID", "LineKey" = "RecKey"))

  return(hummocks)
}

#' @export gather_gap
#' @rdname gather_riparianwetland
gather_gap <- function(dsn, source = "SDE"){

  if(source == "GDB"){
    gap_header <- suppressWarnings(
      sf::st_read(dsn = dsn,
                  layer = "Gap",
                  stringsAsFactors = F)%>%
        sf::st_drop_geometry())%>%
      dplyr::select(PlotID:LineKey,
                    HummocksPresentLine)

    gap_detail <- suppressWarnings(
      sf::st_read(dsn = dsn,
                  layer = "GapDetail",
                  stringsAsFactors = F))%>%
      dplyr::select("EvaluationID"="HummockDetailEvaluationID",
                    RecKey:VegCover)

    message("File Geodatabase data type is being downloaded and gathered into a hummock tall table. ")
    }
  else if(source == "AGOL"){
    fc <- arcgisbinding::arc.open(dsn)@children$FeatureClass
    rs <- arcgisbinding::arc.open(dsn)@children$Table

    gap_header <- arc.data2sf(arc.select(arc.open(paste(dsn, fc[stringr::str_which(fc, "Hummocks")], sep = "/"))))%>%
      sf::st_drop_geometry()%>%
      dplyr::select(PlotID:LineKey, HummocksPresentLine)

    gap_detail <- arc.select(arc.open(paste(dsn, rs[stringr::str_which(rs, "HummockDetail")], sep = "/")))%>%
      dplyr::select("EvaluationID"="HummockDetailEvaluationID",
                    RecKey:VegCover)

    message("ArcGIS Online live feature service data type is being downloaded and gathered into a hummock tall table. ")
    }
  else if(source == "SDE"){
    gap_header <- suppressWarnings(
      sf::st_read(dsn = dsn,
                  layer = "F_Gap",
                  stringsAsFactors = F, quiet = T))

    gap_detail <- suppressWarnings(
      sf::st_read(dsn = dsn,
                  layer = "F_GapDetail",
                  stringsAsFactors = F, quiet = T))

    message("Gathering tables from the SDE into woody structure tall table. ")
  }

  # Join the detail table to the header.
  gap_tall <- dplyr::left_join(gap_header,
                               gap_detail,
                               by = c("EvaluationID", "LineKey" = "RecKey"))

  gap_tall[gap_tall$NoCanopyGaps == "No", ] <- gap_tall %>%
    dplyr::filter(NoCanopyGaps == "No") %>%
    tidyr::replace_na(list(
      RecType = "C",
      GapStart = 0,
      GapEnd = 0,
      Gap = 0
    ))

  return(gap_tall)
}

#' @export gather_soilstab
#' @rdname gather_riparianwetland
gather_soilstab <- function(dsn, source = "SDE"){

  if(source == "GDB"){
    gap_header <- suppressWarnings(
      sf::st_read(dsn = dsn,
                  layer = "SoilStability",
                  stringsAsFactors = F)%>%
        sf::st_drop_geometry())

    message("File Geodatabase data type is being downloaded and gathered into a hummock tall table. ")
  }
  else if(source == "AGOL"){
    fc <- arcgisbinding::arc.open(dsn)@children$FeatureClass

    soilstab <- arc.data2sf(arc.select(arc.open(paste(dsn, fc[stringr::str_which(fc, "SoilStability")], sep = "/"))))%>%
      sf::st_drop_geometry()

    message("ArcGIS Online live feature service data type is being downloaded and gathered into a hummock tall table. ")
  }
  else if(source == "SDE"){
    soilstab <- suppressWarnings(
      sf::st_read(dsn = dsn,
                  layer = "F_SoilStability",
                  stringsAsFactors = F, quiet = T))

    message("Gathering tables from the SDE into woody structure tall table. ")
  }

  gathered <- soilstab%>%
    dplyr::select(-c(PlotID, AdminState, FormDate, Observer, Notes))%>%
    tidyr::pivot_longer(.,c(Veg1:Hydro18))%>%
    filter(value!="")%>%
    dplyr::mutate(key = stringr::str_extract(string = name,
                                             pattern = "^[A-z]+"),
                  Position = stringr::str_extract(string = name,
                                                  pattern = "[0-9]+"))%>%
    dplyr::select(-c(name))%>%
    dplyr::filter(!(key=="Hydro"&value != 0))%>%
    tidyr::pivot_wider(., id_cols = c("EvaluationID", "Position"), names_from = key, values_from = value)%>%

    # Change all Unsampleable points to NA instead of 0
    dplyr::mutate(Rating = ifelse(Veg == "U", NA, Rating))

  return(gathered)

}
