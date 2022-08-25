#' Build AIM Indicators Tables and Feature Classes
#' @param dsn String File path to a Riparian and Wetland File Geodatabase.
#' @param ... Query in grepl format that subsets plots.
#' @name header_build
#' @return A \code{tbl} of header information on each evaluated plot.

# Build the header portion of the Lentic SDE table
#' @export header_build_lentic
#' @rdname header_build
header_build_lentic <- function(dsn, ...) {
  #read in LPI header and detail tables
  if(endsWith(dsn, ".gdb")){
    fieldvisits <- sf::st_read(dsn = dsn, layer = "FieldVisits",
                               stringsAsFactors = FALSE)%>%
      sf::st_drop_geometry()

    plots <- sf::st_read(dsn = dsn, layer = "Plots",
                         stringsAsFactors = FALSE)%>%
      sf::st_drop_geometry()

    plotchar <- sf::st_read(dsn = dsn, layer = "PlotCharacterization",
                            stringsAsFactors = FALSE)%>%
      sf::st_drop_geometry()

    message("File Geodatabase data type is being downloaded and gathered into LPI tall table. ")

  }

  else if(startsWith(dsn, "https://")){
    fc <- arcgisbinding::arc.open(dsn)@children$FeatureClass
    rs <- arcgisbinding::arc.open(dsn)@children$Table

    fieldvisits <- arc.data2sf(arc.select(arc.open(paste(dsn, fc[str_which(fc, "FieldVisits")], sep = "/"))))%>%
      sf::st_drop_geometry()

    plots <- arc.data2sf(arc.select(arc.open(paste(dsn, fc[str_which(fc, "Plots")], sep = "/"))))%>%
      sf::st_drop_geometry()

    plotchar <- arc.data2sf(arc.select(arc.open(paste(dsn, fc[str_which(fc, "PlotCharacterization")], sep = "/"))))%>%
      sf::st_drop_geometry()

    message("ArcGIS Online live feature service data type is being downloaded and gathered into LPI tall table. ")
  }
  else{
    stop("dsn string does not match expected pattern. Must start with 'https://' or end with '.gdb'. ")
  }

  # Set up filter expression (e.g., filter on PlotKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  #FieldVists has the correct visit date
  fieldvisits <- fieldvisits%>%
    as.data.frame()%>%
    dplyr::filter(VisitType=="Full Sample Visit"|VisitType=="Calibration Visit"|VisitType=="Annual Use Visit"|VisitType=="AK Full Sample Visit")%>%
    dplyr::distinct(StaticEvaluationID, .keep_all = T)

  # tblPlots provides the link between species tables
  # (LPI, Height, Species Richness) and tblStateSpecies
  header <-  plots%>%

    # Filter using the filtering expression specified by user
    dplyr::filter(!!!filter_exprs, EvalStatus == "Sampled"|EvalStatus=="Sampled - Data reviewed")%>%
    dplyr::select(-SiteName)%>%

    #Add field visits
    dplyr::left_join(.,
                     fieldvisits%>%dplyr::select(PlotID, EvaluationID = StaticEvaluationID, FieldEvalDate, VisitType),
                     by = c("PlotID" = "PlotID"))

  #some fields can be brought in from Plot Characterization to expand information
  plotchar <- plotchar %>%

    dplyr::select(PlotID,
                  EvaluationID,
                  SiteName,
                  Latitude,
                  Longitude,
                  PlotLayout,
                  ElevationCtr)


  # Join two tables and remove unnecessary fields.
  header <- dplyr::left_join(header, plotchar, by = c("PlotID", "EvaluationID"))

  # Create a list of fields to keep, then test whether they are present in the header table.
  finalfields <- rlang::quos(PlotID,
                             EvaluationID,
                             SiteName,
                             VisitType,
                             SamplingApproach,
                             AdminState,
                             SpeciesState,
                             WetlandIndicatorRegion,
                             DistrictOffice,
                             FieldOffice,
                             FieldEvalDate,
                             LatWGS = Latitude,
                             LongWGS = Longitude,
                             Elevation = ElevationCtr)
  finalfieldnames <- sapply(finalfields, rlang::quo_text)

  # Identify the fields to remove.
  removefields <- numeric()
  for (i in 1:length(finalfieldnames)){
    test <- finalfieldnames[i] %in% colnames(header)
    if(!test){removefields <- append(removefields, i)}
  }

  finalfields <- finalfields[!finalfields %in% c(removefields)]

  header<- header%>%
    dplyr::select(!!!finalfields)

  # Return the header file
  return(header)
}
