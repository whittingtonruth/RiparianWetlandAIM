#'Convert FGDB into separate .csv files
#'
#'Function converts all separate tables found in a FGDB into separate csv files to be more accessible to others. Some schema changes are accomplished to ensure output is interpretable outside of a relational geodatabase format.
#'
#'@param dsn location of the File Geodatabase
#'@param RawDataFolder location where all files will be stored. If exporting to csv, this should be a preexisting folder where all csvs will be placed. If exporting to geopackage, the location must end in '.gpkg' and the file will be created by the script.
#'@param filetype String. Export format. Can be `csv` or `gpkg`. Defaults to `csv`.
#'


#'@export gdbconversion
gdbconversion <- function(dsn, RawDataFolder, filetype = "csv"){

  if(!(filetype %in% c("csv", "gpkg"))){
    stop("Output filetype must be csv or gpkg. ")
  }

  if(filetype == "gpkg" & !endsWith(RawDataFolder, ".gpkg")){
    stop("Geopackage output folder must end in correct extension - .gpkg.")
  }

  dsn_o <- arcgisbinding::arc.open(dsn)
  dsn_fc <- dsn_o@children$FeatureClass
  dsn_t <- dsn_o@children$Table

  for(layer in dsn_fc){
    fc <- arcgisbinding::arc.data2sf(
      arcgisbinding::arc.select(
        arcgisbinding::arc.open(paste(dsn, layer, sep = "/"))))

    if(filetype == "gpkg"){

      sf::st_write(fc, dsn = RawDataFolder, layer = layer, driver="GPKG")

      }else if(filetype == "csv"){

        fc$Geometry <- sf::st_as_text(sf::st_geometry(fc))
        fc <- sf::st_drop_geometry(fc)

        write.csv(fc, file = paste(RawDataFolder, paste(layer, ".csv", sep = ""), sep = "/"), na = "", row.names = F)
    }
  }

  for(table in dsn_t){
    tab <- arcgisbinding::arc.select(
        arcgisbinding::arc.open(paste(dsn, table, sep = "/")))

    if(filetype == "gpkg"){

      sf::st_write(tab, dsn = RawDataFolder, layer = table, driver="GPKG")

    }else if(filetype == "csv"){

      write.csv(tab, file = paste(RawDataFolder, paste(table, ".csv", sep = ""), sep = "/"), na = "", row.names = F)
    }
  }

  message("All files found in dsn have been exported to the specified data folder. ")
}
