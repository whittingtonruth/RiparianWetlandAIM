#' Build AIM Indicators Tables and Feature Classes
#' @param dsn String File path to the TerrADat database.
#' @param ... Query in grepl format that subsets plots.
#' @return A \code{tbl} of header information on each evaluated plot.

# Build the header portion of the Lentic SDE table
#' @export header_build_lentic
#' @rdname header_build
header_build_lentic <- function(dsn, ...) {
  # Set up filter expression (e.g., filter on PlotKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  # tblPlots provides the link between species tables
  # (LPI, Height, Species Richness) and tblStateSpecies
  header <- sf::st_read(dsn = dsn, layer = "Plots",
                        stringsAsFactors = FALSE) %>%
    as.data.frame() %>%

    # Filter using the filtering expression specified by user
    dplyr::filter(!!!filter_exprs, EvalStatus == "Eval")%>%

    # Remove SamplingApproach from Plots, as format in PlotChar is closer to what we want to use.
    dplyr::select(-SamplingApproach)

  #some fields can be brought in from Plot Characterization to expand information
  plotcharfields <- sf::st_read(dsn = dsn, layer = "PlotCharacterization",
                                stringsAsFactors = FALSE) %>%
    as.data.frame() %>%

    dplyr::select(PlotKey,
                  SiteName,
                  SamplingApproach,
                  Northing,
                  Easting,
                  PlotLayout,
                  ElevationCtr)

  # Join two tables and remove unnecessary fields.
  header <- dplyr::left_join(header, plotcharfields) %>%
    dplyr::select(PlotID,
                  PlotKey,
                  SiteName,
                  SamplingApproach,
                  AdminState,
                  Region,
                  DistrictOffice = District,
                  FieldOffice = Office,
                  VisitDate,
                  LatWGS = Northing,
                  LongWGS = Easting,
                  Elevation = ElevationCtr
    )

  # Return the header file
  return(header)
}
