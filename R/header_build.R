#' Build AIM Indicators Tables and Feature Classes
#' @param dsn String File path to a Riparian and Wetland File Geodatabase.
#' @param ... Query in grepl format that subsets plots.
#' @name header_build
#' @return A \code{tbl} of header information on each evaluated plot.

# Build the header portion of the Lentic SDE table
#' @export header_build_lentic
#' @rdname header_build
header_build_lentic <- function(dsn, ...) {
  # Set up filter expression (e.g., filter on PlotKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  #FieldVists has the correct visit date
  fieldvisits <- sf::st_read(dsn = dsn, layer = "FieldVisits",
                             stringsAsFactors = FALSE)%>%
    as.data.frame()%>%
    dplyr::filter(VisitType=="Full Sample Visit"|VisitType=="Calibration Visit")%>%
    dplyr::distinct(StaticEvaluationID, .keep_all = T)

  # tblPlots provides the link between species tables
  # (LPI, Height, Species Richness) and tblStateSpecies
  header <- sf::st_read(dsn = dsn, layer = "Plots",
                        stringsAsFactors = FALSE) %>%
    as.data.frame() %>%

    # Filter using the filtering expression specified by user
    dplyr::filter(!!!filter_exprs, EvalStatus == "Sampled")%>%
    dplyr::select(-SiteName)%>%

    #Add field visits
    dplyr::left_join(.,
                     fieldvisits%>%dplyr::select(PlotID, EvaluationID = StaticEvaluationID, FieldEvalDate, VisitType),
                     by = c("PlotID" = "PlotID"))

  #some fields can be brought in from Plot Characterization to expand information
  plotcharfields <- sf::st_read(dsn = dsn, layer = "PlotCharacterization",
                                stringsAsFactors = FALSE) %>%
    as.data.frame() %>%

    dplyr::select(PlotID,
                  EvaluationID,
                  SiteName,
                  Latitude,
                  Longitude,
                  PlotLayout,
                  ElevationCtr)

  # Join two tables and remove unnecessary fields.
  header <- dplyr::left_join(header, plotcharfields, by = c("PlotID", "EvaluationID")) %>%
    dplyr::select(PlotID,
                  EvaluationID,
                  SiteName,
                  SamplingApproach,
                  AdminState,
                  SpeciesState,
                  WetlandIndicatorRegion,
                  DistrictOffice,
                  FieldOffice,
                  FieldEvalDate,
                  LatWGS = Latitude,
                  LongWGS = Longitude,
                  Elevation = ElevationCtr
    )

  # Return the header file
  return(header)
}
