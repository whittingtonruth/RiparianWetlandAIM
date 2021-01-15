#' Build AIM Indicators Tables and Feature Classes
#' @param dsn String File path to the TerrADat database.
#' @param header Dataframe. Plot header containing plot metadata
#' @param source String. Specifies data source, \code{"AIM", "LMF"}
#' @param ... Query in grepl format that subsets plots.
#' @return A \code{tbl} of indicators of either tall or wide format.


# Build the header portion of the Lentic SDE table
#' @export header_build_lentic
#' @rdname header_build
header_build_lentic <- function(dsn, ...) {
  # Set up filter expression (e.g., filter on DBKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  # tblPlots provides the link between species tables
  # (LPI, Height, Species Richness) and tblStateSpecies
  header <- sf::st_read(dsn = dsn, layer = "Plots",
                        stringsAsFactors = FALSE) %>%
    as.data.frame() %>%

    # Filter using the filtering expression specified by user
    dplyr::filter(!!!filter_exprs) %>%

    # Select the field names we need in the final feature class
    dplyr::select(PlotID,
                  PlotKey,
                  SiteName = Name,
                  LatWGS,
                  LongWGS,
                  FieldOffice = Office,
                  DistrictOffice = Region,
                  State = AdminState,
                  SamplingApproach
    )

  # Return the header file
  return(header)
}
