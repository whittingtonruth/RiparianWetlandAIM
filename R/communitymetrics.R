#'Function used to summarize community metrics from plot Species Inventory data.
#'
#'@param spp_inventory source of species inventory data frame including the grouping variables
#'for which data is being summarized by.
#'@param masterspecieslist Character string. Full file path (including extension) to the file
#'containing the species list.
#'@param lpi_tall optional. If you wish to summarize by species in LPI
#'@param method character string. The method used for the produced summary table. Can
#'be \code{"percent"}, \code{"mean"}, or {"count"}. Defaults to \code{"percent"}.
#'

#'@export Community_Composition
Community_Composition <- function(spp_inventory, method = "percent", ...){

  if(!(method %in% c("percent", "mean", "count"))){
    stop("Method must be 'percent', 'mean' or 'count'.")
  }

  grouping_variables <- rlang::quos(...)

  totals <- spp_inventory%>%
    dplyr::


}


masterspecieslist <- masterspecieslist%>%
  dplyr::select(Symbol,
                Scientific.Name,
                Species,
                GrowthHabitSub,
                Duration,
                NativeStatus,
                ends_with("_WetStatus"),
                ends_with("_C.Value"),
                ends_with("_Nox"))

  dplyr::left_join(spp_inventory,
                   masterspecieslist,
                   by = c("Species" = "Symbol"))%>%
    dplyr::group_by(PlotKey, Species)%>%
    dplyr
