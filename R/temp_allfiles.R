#script to store a file of all feature classes
#delete this file when package is complete.

#file path to gdb
dsntemp <- "C:/Users/whitt/Documents/CNHP/2020 data/FGDBcopies/COmaps_2021-01-04.gdb"
#Feature Classes
temp_plots <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "Plots",
  stringsAsFactors = FALSE
))

temp_plotchar <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "PlotCharacterization",
  stringsAsFactors = FALSE
))

temp_locver <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "LocationVerification",
  stringsAsFactors = FALSE
))

temp_hydro <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "Hydrology",
  stringsAsFactors = FALSE
))

temp_waterchem <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "HydroChemDetail",
  stringsAsFactors = FALSE
))

temp_specrich <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "SpeciesInventory",
  stringsAsFactors = FALSE
))

temp_lpiheader <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "LPI",
  stringsAsFactors = FALSE
))

temp_soils <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "Soils",
  stringsAsFactors = FALSE
))

temp_woodyspecies <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "WoodySpecies",
  stringsAsFactors = FALSE
))

temp_hummocks <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "Hummocks",
  stringsAsFactors = FALSE
))

temp_humanandnat <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "HumanandNaturalDisturbance",
  stringsAsFactors = FALSE
))

temp_photos <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "Photos",
  stringsAsFactors = FALSE
))

temp_knownerrors <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "KnownErrors",
  stringsAsFactors = FALSE
))

temp_unknownplants <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "UnknownPlants",
  stringsAsFactors = FALSE
))

temp_hydrochemdetail <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "HydroChemDetail",
  stringsAsFactors = FALSE
))


#feature tables
temp_lpidetail <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "LPIDetail",
  stringsAsFactors = FALSE
))

temp_pointsrepeat <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "PointsRepeat",
  stringsAsFactors = FALSE
))

temp_woodyspeciesrepeat <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "WoodySpeciesRepeat",
  stringsAsFactors = FALSE
))

temp_hummockdetail <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "HummockDetail",
  stringsAsFactors = FALSE
))

temp_soilpitdetail <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "SoilPitHorizons",
  stringsAsFactors = FALSE
))

temp_specrichdetail <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "SpecRichDetail",
  stringsAsFactors = FALSE
))

temp_stressors <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "Stressors",
  stringsAsFactors = FALSE
))

temp_unknowncodes <- suppressWarnings(sf::st_read(
  dsn = dsntemp,
  layer = "UnknownCodes",
  stringsAsFactors = FALSE
))

