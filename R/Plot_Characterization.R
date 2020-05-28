#' Header for lentic projects
#' @description Headers build from the plot characterization form to match to each entry from all tall tables.
#' @param dsn File path to the file geodatabase where all raw data from AGOL is stored.

#' @export header_build_lentic
#' @rdname Plot_Characterization
header_build_lentic <- function(dsn, ...) {
  # Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  # tblPlots provides the link between species tables
  # (LPI, Height, Species Richness) and tblStateSpecies
  header <- sf::st_read(dsn = dsn, layer = "PlotChar",
                          stringsAsFactors = FALSE) %>%
    as.data.frame() %>%

    # Filter using the filtering expression specified by user
    dplyr::filter(!!!filter_exprs) %>%

      # Select the field names we need in the final feature class
    dplyr::select(PlotID, PlotKey,
                    SiteName, SamplingApproach,Latitude_NAD83 =Northing, Longitude_NAD83 = Easting, State = StateCode,
                    DateEstablished = CreationDate
                  ) #%>%

    # If there are any Sites with no PrimaryKeys, delete them -->not necessary as of 5/28/20
    #subset(!is.na(PrimaryKey))

    # Return the header file
    return(header)
}
