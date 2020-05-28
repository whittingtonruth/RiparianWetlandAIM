#' Check species data
#' @description Functions which build from LPI, Height, and Species Inventory tall tables to perform species checks.
#' @param dsn_tall A file path to tall tables produced from gathering functions
#' @param species_list_file The file path to the species list. If contained within the dsn, then specify the layer name within the dsn.

#' @export species_list_check_lentic
#' @rdname species_QC_lentic
species_list_check_lentic <- function(dsn_tall = dsn_tall,
                                      species_list_file,
                                      ...){
  # Setup filter expression (i.e. to filter on DBKey, SpeciesState, etc.)
  filter_exprs <- rlang::quos(...)

  # Read in header information to provide subset and link to the species list.
  header <-  readRDS(paste(dsn_tall, "header.Rdata", sep = "/"))
  header_sub <- header %>% dplyr::filter(!!!filter_exprs)


}
