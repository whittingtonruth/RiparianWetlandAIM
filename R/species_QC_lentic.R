#' Check species data
#' @description Functions which build from LPI, Height, and Species Inventory tall tables to perform species checks.
#' @param dsn_tall A file path to tall tables produced from gathering functions
#' @param species_list_file The file path to the species list. If contained within the dsn, then specify the layer name within the dsn.
#' @param ... Filters can be added which should be based on the header column names.

#' @export species_list_check_lentic
#' @rdname species_QC_lentic
species_list_check_lentic <- function(dsn_tall = dsn_tall,
                                      species_list_file,
                                      ...){
  # Setup filter expression (i.e. to filter on DBKey, SpeciesState, etc.)
  filter_exprs <- rlang::quos(...)

  # Read in header information to provide subset and link to the species list.
  header <-  readRDS(paste(dsn_tall, "header.Rdata", sep = ""))
  header_sub <- header %>% dplyr::filter(!!!filter_exprs)

  # Next we want to load in all the tall files with species data (i.e. LPI, height, and Species Inventory)
  ## Read in LPI
  lpi <- readRDS(paste(dsn_tall, "lpi_tall.Rdata", sep = "")) %>%
    dplyr::select(PlotKey, Species = code) %>%
    dplyr::left_join(header_sub, .)

  ## Read in height
  height <- readRDS(paste(dsn_tall, "height_tall.Rdata", sep = "")) %>%
    dplyr::left_join(header_sub, .)

  ## Read in Species inventory
  spp_inventory <- readRDS(paste(dsn_tall, "spp_inventory_tall.Rdata", sep = "")) %>%
    dplyr::select(PlotKey, Species) %>%
    dplyr::left_join(header_sub, .)

  # Merge all the species lists together
  species_all <- dplyr::bind_rows(lpi,
                                  height %>% dplyr::select(PlotKey, Species, State),
                                  spp_inventory) %>%
    subset(nchar(Species) >=3 & Species != "None") %>%
    dplyr::distinct() %>%

    #and then join to the information from the species list.
    species_join_lentic()

}
