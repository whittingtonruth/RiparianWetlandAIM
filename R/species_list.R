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

#' @export species_QC_lentic
#' @rdname species_QC_lentic
species_QC_lentic <- function(header, spp_inventory, lpi_tall, height_tall, woody_species){

  #Create a species list of all unique plot-species combinations
  specieslist <- paste(spp_inventory$PlotKey, spp_inventory$Species, sep = "_")

  #First double check that all species from LPI are accounted for in Species Inventory.
  lpi_missingspecies <- lpi_tall%>%
    dplyr::filter(!code %in% nonplantcodes$code)%>%
    dplyr::mutate("PlotSpecies" = paste(PlotKey, code, sep = "_"))%>%
    dplyr::distinct(PlotSpecies, .keep_all = F)%>%
    dplyr::filter(!PlotSpecies %in% specieslist)

  if(nrow(lpi_missingspecies > 0)){
    warning("Species were found in LPI that were missing from Species Richness. Fix this error before producing a full species list.")
    print(lpi_missingspecies)
  }

  #Check that all species in LPI heights are on the species list.
  height_missingspecies <- as.data.frame(height_tall)%>%
    dplyr::filter(Species != "N")%>%
    dplyr::mutate("PlotSpecies" = paste(PlotKey, Species, sep = "_"))%>%
    dplyr::distinct(PlotSpecies, .keep_all = F)%>%
    dplyr::filter(!PlotSpecies %in% specieslist)

  if(nrow(height_missingspecies > 0)){
    warning("Species were found in LPI heights that were missing from Species Richness. Fix this error before producing a full species list.")
    print(height_missingspecies)
  }

  #Check that all woody species are on the species list
  woody_missingspecies <- as.data.frame(woody_species)%>%
    dplyr::filter(RiparianWoodySpecies != "N"&RiparianWoodySpecies!="")%>%
    dplyr::mutate("PlotSpecies" = paste(PlotKey, RiparianWoodySpecies, sep = "_"))%>%
    dplyr::distinct(PlotSpecies, .keep_all = F)%>%
    dplyr::filter(!PlotSpecies %in% specieslist)

  if(nrow(woody_missingspecies > 0)){
    warning("Species were found in Woody Species Form that were missing from Species Richness. Fix this error before producing a full species list.")
    print(woody_missingspecies)
  }

  return("If no warning messages are returned, all species in LPI, height, and woody species are found in Species Richness.")

}


#' @export species_list_lentic
#' @rdname species_list_lentic
species_list_lentic <- function(header, spp_inventory_tall, masterspecies){

  masterlist <- masterspecies%>%
    dplyr::select(Symbol,
                  Scientific.Name)

  spp_inventory_join <- as.data.frame(spp_inventory_tall) %>%
    dplyr::select(PlotID,
                  PlotKey,
                  Species,
                  abundance,
                  UnknownCode)%>%
    dplyr::mutate(UnknownCode=replace(UnknownCode, is.na(UnknownCode), ""))%>%
    dplyr::left_join(masterlist, ., by = c("Symbol" = "Species"))%>%
    dplyr::left_join(as.data.frame(header), .)

  return(spp_inventory_join)

}

