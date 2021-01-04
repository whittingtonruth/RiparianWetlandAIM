#' Gather LPI data from Lentic FGDB.
#' @description Given a list of data frames containing tblLPIHeader and tblLPIDetail, create a tall format data frame for canopy data from LPI and one for heights from the specialized height fields.
#' @param dsn Character string. The full filepath and filename (including file extension) of the geodatabase containing the table of interest.
#' @importFrom magrittr %>%
#' @name gather_lpi
#' @family <gather>
#' @return A data frames containing the data from the LPI pin intercepts.

#' @export gather_lpi_lentic
#' @rdname gather_lpi_lentic
## Function to transform LPI data into tall format.
gather_lpi_lentic <- function(dsn){
  #read in LPI header and detail tables
  lpi_detail <- suppressWarnings(sf::st_read(dsn = dsn,
                                            layer = "lpiDetail",
                                            stringsAsFactors = F))

  lpi_header <- suppressWarnings(sf::st_read(dsn = dsn,
                                             layer = "LPI",
                                             stringsAsFactors = F))

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
                     "ChkboxWoody2",
                     "ChkboxHerbaceous")))

  #Rename the checkbox layer names so they match
  lpi_chkbox_tall$layer <- gsub(lpi_chkbox_tall$layer,
                                pattern = "^Chkbox",
                                replacement = "")

  lpi_chkbox_tall$layer[lpi_chkbox_tall$layer == "Top"] <- "TopCanopy"
  lpi_chkbox_tall$layer[lpi_chkbox_tall$layer == "Basal"] <- "SoilSurface"

  lpi_tall <- suppressWarnings(dplyr::left_join(
    lpi_hits_tall,
    lpi_chkbox_tall)) %>%

    suppressWarnings(dplyr::left_join(x = dplyr::select(lpi_header, "LineKey":"LineLengthCM"),
                     y = .,
                     by = c("LineKey" = "RecKey")))

  return(lpi_tall)
  }
