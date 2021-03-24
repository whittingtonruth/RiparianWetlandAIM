#'Build a species list of all species seen during the field season.
#' @param lpi_tall Source of lpi Rdata file
#' @param height_tall Source of height Rdata file
#' @param species_inventory_tall Source of species inventory Rdata file
#' @param species_file File path to species file if you want species attributes or updated species. Geodatabase or csv allowed.
#' @param header Source of header Rdata file
#' @param ... Filtering expression to subset the number of plots
#' @examples
#' # Get a list of all species occurring on a plot across methods (LPI, height, species inventory)
#' # This method also adds cover and height by species. Be aware that sample sizes may be insufficient to make an accurate estimate


#' @export species_list_lentic
#' @rdname species_list_lentic
species_list_lentic <- function(header, spp_inventory_tall, lpi_tall, height_tall, woody_species){
  plot <- readRDS(header)
  spp_inventory <- readRDS(spp_inventory_tall) %>%
    dplyr::select
  lpi_detail <- readRDS(lpi_tall)

}
