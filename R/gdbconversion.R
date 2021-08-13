#'Convert FGDB into separate .csv files
#'
#'Function converts all separate tables found in a FGDB into separate csv files to be
#'more accessible to others.
#'
#'@param dsn location of the File Geodatabase
#'@param RawDataFolder location where all csvs will be stored.
#'@return Dataframe of specified perce cover of different categories by plot.
#'


#'@export gdbconversion
gdbconversion <- function(dsn, RawDataFolder){

  plots <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "Plots",
    stringsAsFactors = FALSE
  ))%>%
    sf::st_drop_geometry()

    write.csv(plots, paste(RawDataFolder, "Plots.csv", sep = "/"), row.names = F)

  locver  <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "LocationVerification",
    stringsAsFactors = FALSE
  ))%>%
    sf::st_drop_geometry()

  write.csv(locver, paste(RawDataFolder, "LocationVerification.csv", sep = "/"), row.names = F)

  plotchar <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "PlotCharacterization",
    stringsAsFactors = FALSE
  ))%>%
    sf::st_drop_geometry()%>%
    dplyr::mutate(WetlandType = stringr::str_replace_all(WetlandType, "(.)([[:upper:]])([[:lower:]])", "\\1 \\2\\3"),
                  CowardinAttribute = paste(CowardinSystem,
                                            CowardinClass,
                                            ifelse(!(WaterRegime %in% c("", NA)), WaterRegime,""),
                                            ifelse(!(Modifiers %in% c("", NA, "None")), Modifiers, ""), sep = ""),
                  HydroGeoSubType = case_when(HydroGeoSlopeType != "" ~ HydroGeoSlopeType,
                                              HydroGeoRiverineType != "" ~ HydroGeoRiverineType,
                                              HydroGeoDepType != "" ~ HydroGeoDepType,
                                              HydroGeoFlatType != "" ~ HydroGeoFlatType,
                                              LacustrineSubclass != "" ~ LacustrineSubclass),
                  HydroGeoSubType = stringr::str_replace_all(HydroGeoSubType, "(.)([[:upper:]])([[:lower:]])", "\\1 \\2\\3"))%>%
    dplyr::relocate(CowardinAttribute, .before = CowardinSystem)%>%
    dplyr::relocate(HydroGeoSubType, .after = HydroGeoType)

  write.csv(plotchar, paste(RawDataFolder, "PlotCharacterization.csv", sep = "/"), row.names = F)

  hydro <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "Hydrology",
    stringsAsFactors = FALSE
  ))%>%
    sf::st_drop_geometry()

  write.csv(hydro, paste(RawDataFolder, "Hydrology.csv", sep = "/"), row.names = F)

  soil <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "Soils",
    stringsAsFactors = FALSE
  ))%>%
    sf::st_drop_geometry()

  write.csv(soil, paste(RawDataFolder, "Soils.csv", sep = "/"), row.names = F)

  textures <- data.frame(Code = c("C", "CL","L","S","SC","SCL","SI","SIC","SICL","SIL","SL", "LS", "FO","HO","SO"),
                         Description = c("Clay", "Clay Loam", "Loam", "Sand", "Sandy Clay", "Sandy Clay Loam", "Silt",
                                         "Silty Clay", "Silty Clay Loam", "Silt Loam", "Sandy Loam", "Loamy Sand", "Fibric Organic",
                                         "Hemic Organic", "Sapric Organic"))

  soilhorizons <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "SoilPitHorizons",
    stringsAsFactors = FALSE
  ))%>%
    dplyr::mutate(MatrixColor = paste(MatrixHue, " ", MatrixValue, "/", MatrixChroma, sep = ""), .before = MatrixHue)%>%
    dplyr::mutate(PrimaryRedoxColor = if_else(PrimaryRedoxFeatures == "Yes", paste(PrimaryRedoxHue, " ", PrimaryRedoxValue, "/", PrimaryRedoxChroma, sep = ""), ""), .before = PrimaryRedoxHue)%>%
    dplyr::mutate(SecondaryRedoxColor = if_else(SecondaryRedoxFeatures == "Yes", paste(SecondaryRedoxHue, " ", SecondaryRedoxValue, "/", SecondaryRedoxChroma, sep = ""), ""), .before = SecondaryRedoxHue)%>%
    dplyr::left_join(., textures, by = c("Texture" = "Code"))%>%
    dplyr::select(-c(MatrixHue, MatrixValue, MatrixChroma, PrimaryRedoxHue, PrimaryRedoxValue, PrimaryRedoxChroma, SecondaryRedoxHue, SecondaryRedoxValue, SecondaryRedoxChroma, Texture))%>%
    dplyr::relocate(Texture = Description, .before = RockFragmentsOver2mm)

  write.csv(soilhorizons, paste(RawDataFolder, "SoilPitHorizons.csv", sep = "/"), row.names = F)

  disturb <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "HumanandNaturalDisturbance",
    stringsAsFactors = FALSE
  ))%>%
    sf::st_drop_geometry()

  write.csv(disturb, paste(RawDataFolder, "HumanandNaturalDisturbance.csv", sep = "/"), row.names = F)

  stressors <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "Stressors",
    stringsAsFactors = FALSE
  ))

  write.csv(stressors, paste(RawDataFolder, "StressorsDetail.csv", sep = "/"), row.names = F)

  sppinv <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "SpeciesInventory",
    stringsAsFactors = FALSE
  ))%>%
    sf::st_drop_geometry()

  write.csv(sppinv, paste(RawDataFolder, "SpeciesInventory.csv", sep = "/"), row.names = F)

  specrichdetail <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "SpecRichDetail",
    stringsAsFactors = FALSE
  ))

  write.csv(specrichdetail, paste(RawDataFolder, "SpecRichDetail.csv", sep = "/"), row.names = F)

  unknown <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "UnknownPlants",
    stringsAsFactors = FALSE
  ))%>%
    sf::st_drop_geometry()

  write.csv(unknown, paste(RawDataFolder, "UnknownPlants.csv", sep = "/"), row.names = F)

  unknowndetail <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "UnknownCodes",
    stringsAsFactors = FALSE
  ))

  write.csv(unknowndetail, paste(RawDataFolder, "UnknownCodes.csv", sep = "/"), row.names = F)

  lpi <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "LPI",
    stringsAsFactors = FALSE
  ))%>%
    sf::st_drop_geometry()

  write.csv(lpi, paste(RawDataFolder, "LPI.csv", sep = "/"), row.names = F)

  lpidetail <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "LPIDetail",
    stringsAsFactors = FALSE
  ))

  write.csv(lpidetail, paste(RawDataFolder, "LPIDetail.csv", sep = "/"), row.names = F)

  woodyspecies <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "WoodySpecies",
    stringsAsFactors = FALSE
  ))%>%
    sf::st_drop_geometry()

  write.csv(woodyspecies, paste(RawDataFolder, "WoodySpecies.csv", sep = "/"), row.names = F)

  pointsrepeat <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "PointsRepeat",
    stringsAsFactors = FALSE
  ))

  woodyspeciesdetail <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "WoodySpeciesRepeat",
    stringsAsFactors = FALSE
  ))%>%
    dplyr::left_join(pointsrepeat%>%
                       dplyr::select(globalid, PointNbr, PointLoc),
                     ., by = c("globalid" = "parentglobalid"))%>%
    dplyr::relocate(RecKey, .before = PointNbr)%>%
    dplyr::select(-c(globalid, globalid.y))

  write.csv(woodyspeciesdetail, paste(RawDataFolder, "WoodySpeciesDetail.csv", sep = "/"), row.names = F)

  hummocks <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "Hummocks",
    stringsAsFactors = FALSE
  ))%>%
    sf::st_drop_geometry()

  write.csv(hummocks, paste(RawDataFolder, "Hummocks.csv", sep = "/"), row.names = F)

  hummockdetail <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "HummockDetail",
    stringsAsFactors = FALSE
  ))

  write.csv(hummockdetail, paste(RawDataFolder, "HummockDetail.csv", sep = "/"), row.names = F)

  waterquality <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "WaterChemistry",
    stringsAsFactors = FALSE
  ))%>%
    sf::st_drop_geometry()

  write.csv(waterquality, paste(RawDataFolder, "WaterChemistry.csv", sep = "/"), row.names = F)

  waterqualitydetail <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "HydroChemDetail",
    stringsAsFactors = FALSE
  ))%>%
    sf::st_drop_geometry()

  write.csv(waterqualitydetail, paste(RawDataFolder, "HydroChemDetail.csv", sep = "/"), row.names = F)

  knownerror <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "KnownErrors",
    stringsAsFactors = FALSE
  ))%>%
    sf::st_drop_geometry()

  write.csv(knownerror, paste(RawDataFolder, "KnownErrors.csv", sep = "/"), row.names = F)

  photos <- suppressWarnings(sf::st_read(
    dsn = dsn,
    layer = "Photos",
    stringsAsFactors = FALSE
  ))%>%
    sf::st_drop_geometry()

  write.csv(photos, paste(RawDataFolder, "Photos.csv", sep = "/"), row.names = F)
}
