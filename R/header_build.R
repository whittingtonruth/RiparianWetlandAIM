#' Build AIM Indicators Tables and Feature Classes
#'
#' @param dsn String File path to a Riparian and Wetland File Geodatabase.
#' @param source String. The source from which data is being drawn from. Can be "SDE", "GDB", or "AGOL". Defaults to "SDE".
#' @param annualuse_tall data.frame. Optional. Tall table exported from `gather_annualuse` used to create entries in header for annual use only visits. Without the table, header will only include visits in Plot Characterization.
#' @param ... Query in grepl format that subsets plots.
#' @name header_build
#' @return A \code{tbl} of header information on each evaluated plot.

#' @export header_build_lentic
#' @rdname header_build
header_build_lentic <- function(dsn, source = "SDE", annualuse_tall, ...) {
  #read in LPI header and detail tables
  if(source == "GDB"){
    fieldvisits <- sf::st_read(dsn = dsn, layer = "FieldVisits",
                               stringsAsFactors = FALSE)%>%
      sf::st_drop_geometry()

    plotchar <- sf::st_read(dsn = dsn, layer = "PlotCharacterization",
                            stringsAsFactors = FALSE)

    message("File Geodatabase data type is being downloaded and gathered into header table used to select sites for analysis. ")
  }

  else if(source == "AGOL"){
    fc <- arcgisbinding::arc.open(dsn)@children$FeatureClass
    rs <- arcgisbinding::arc.open(dsn)@children$Table

    fieldvisits <- arc.data2sf(arc.select(arc.open(paste(dsn, fc[stringr::str_which(fc, "FieldVisits")], sep = "/"))))%>%
      sf::st_drop_geometry()

    plotchar <- arc.data2sf(arc.select(arc.open(paste(dsn, fc[stringr::str_which(fc, "PlotCharacterization")], sep = "/"))))

    message("ArcGIS Online live feature service data type is being downloaded and gathered into header table used to select sites for analysis. ")
  }
  if(source == "SDE"){
    plotchar <- sf::st_read(dsn = dsn, layer = "F_PlotCharacterization",
                            stringsAsFactors = FALSE)

    message("The SDE PlotChar table is being gathered into header table used to select sites for analysis. ")
  }

  # Set up filter expression (e.g., filter on PlotKey, SpeciesState, etc)
  filter_exprs <- rlang::quos(...)

  plotchar <- plotchar%>%
    {if(!"SpeciesState" %in% names(.)) dplyr::mutate(., SpeciesState = AdminState) else .}%>%
    dplyr::mutate(FieldEvalDate = as.Date(stringr::str_extract(plotchar$EvaluationID, "(?<=_)[:digit:]{4}-[:digit:]{2}-[:digit:]{2}"))+
             lubridate::hours(12),
           #VisitType = ifelse(AdminState == "AK", "AK Full Sample Visit", "Full Sample Visit"),
           StateCode = SpeciesState,
           PlotArea_m2 = dplyr::case_when(PlotLayout == "Spoke"~2827,
                                          #Use actual plot length when smaller than the max.
                                          PlotLayout %in% c("Transverse", "Diagonal", "Linear") & MaxPlotLengthCalc>ActualPlotLength~AvgWidthArea*ActualPlotLength,
                                          #Use max when actual is larger
                                          PlotLayout %in% c("Transverse", "Diagonal", "Linear")~AvgWidthArea*MaxPlotLengthCalc,
                                          PlotLayout == "Mixed Layout"~NA))

  # Create a list of fields to keep, then test whether they are present in the header table.
  finalfields <- rlang::quos(PlotID,
                             EvaluationID,
                             SiteName,
                             VisitType,
                             SamplingApproach,
                             PlotLayout,
                             PlotArea_m2,
                             CowardinAttribute,
                             HGMClass,
                             WetlandType,
                             EcotypeAlaska = AlaskaEcotypeClassification,
                             AdminState,
                             StateCode,
                             SpeciesState,
                             WetlandIndicatorRegion,
                             FieldEvalDate,
                             LatitudeWGS84,
                             LongitudeWGS84,
                             Elevation_m)
  finalfieldnames <- sapply(finalfields, rlang::quo_text)

  # Identify the fields to remove.
  removefields <- numeric()
  for (i in 1:length(finalfieldnames)){
    test <- finalfieldnames[i] %in% colnames(plotchar)
    if(!test){removefields <- append(removefields, i)}
  }

  if(length(removefields) > 0){
    finalfields <- finalfields[-removefields]
  }


  header<- plotchar%>%
    dplyr::select(!!!finalfields)

  if(!missing(annualuse_tall)){
    annualusevisits <- annualuse_tall%>%
      dplyr::select(PlotID,
                    EvaluationID, AdminState, SpeciesState)%>%
      dplyr::distinct(EvaluationID, .keep_all = T)%>%
      dplyr::filter(!(EvaluationID %in% header$EvaluationID))%>%
      dplyr::mutate(VisitType = "Annual Use Visit",
                    FieldEvalDate = as.Date(stringr::str_extract(EvaluationID, "(?<=_)[:digit:]{4}-[:digit:]{2}-[:digit:]{2}")) + lubridate::hours(12),
                    State = SpeciesState)%>%
      dplyr::left_join(.,
                       header%>%dplyr::select(PlotID,
                                              SiteName,
                                              SamplingApproach,
                                              WetlandIndicatorRegion,
                                              LatitudeWGS84,
                                              LongitudeWGS84)%>%
                         dplyr::distinct(PlotID, .keep_all = T),
                       by = c("PlotID"))

    header <- header%>%
      bind_rows(., annualusevisits)
  } else {
    warning("annualuse_tall table was not used to create header. All Annual Use Only visits will be excluded from analysis. ")
  }

  if(nrow(header%>%filter(duplicated(EvaluationID)))>0){
    warning("EvaluationIDs in header are not unique! There may be multiple Plot Characterization forms for one or more site visits. ")
  }

  # Return the header file
  return(header)
}
