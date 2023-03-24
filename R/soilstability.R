#' Soil Stability Indicator Calculations
#' @param soil_stability_tall Dataframe Gathered soil stability data
#' @param all Logical. When \code{TRUE} Calculate soil stability for all samples. Defaults to \code{TRUE}
#' @param cover Logical. When \code{TRUE}, calculates soil stability for samples covered byperennial vegetation. Defaults to \code{TRUE}
#' @param uncovered. Logical. When \code{TRUE}, calculates soil stability for samples not covered by perennial vegetation. Defaults to \code{TRUE}
#' @param all_cover_types Logical. When \code{TRUE}, calculates soil stability for each indidual cover type. Defaults to \code{FALSE}
#' @param tall Logical. Indicates if output is tall/long or wide. Defaults to \code{TRUE}
#' @return Dataframe of calculated soil stability values by plot.

#' @export soil_stability
#' @rdname soil_stability
soil_stability <- function(soil_stability_tall,
                           all = TRUE,
                           cover = TRUE,
                           uncovered = TRUE,
                           all_cover_types = FALSE,
                           tall = FALSE) {
  soil_stability_rating <- list()

  # Remove NA Rating values
  # ensure that rating is numeric
  soil_stability_tall <- soil_stability_tall %>%
    dplyr::filter(!is.na(Rating)) %>%
    dplyr::mutate(Rating = as.numeric(Rating))

  # Calculate a mean rating for all cover types
  if (all == TRUE) {
    soil_stability_rating[["all"]] <- soil_stability_tall %>%
      dplyr::group_by(EvaluationID) %>%
      dplyr::summarize(rating = round(mean(Rating), digits = 2)) %>%
      dplyr::mutate(Veg = "All") %>%
      as.data.frame()
  }
  # Calculate mean rating for all covered soil samples
  if (cover == TRUE) {
    soil_stability_rating[["covered"]] <- soil_stability_tall %>%
      subset(Veg != "NC") %>%
      dplyr::group_by(EvaluationID) %>%
      dplyr::summarize(rating = round(mean(Rating), digits = 2)) %>%
      dplyr::mutate(Veg = "Protected") %>%
      as.data.frame()
  }
  # Calculate mean rating for all uncovered soil samples
  if (uncovered == TRUE) {
    soil_stability_rating[["uncovered"]] <- soil_stability_tall %>%
      subset(Veg == "NC") %>%
      dplyr::group_by(EvaluationID) %>%
      dplyr::summarize(rating = round(mean(Rating), digits = 2)) %>%
      dplyr::mutate(Veg = "Unprotected") %>%
      as.data.frame()
  }
  # Calculate mean rating for all cover types individually
  if (all_cover_types == TRUE) {
    soil_stability_rating[["all_cover_types"]] <- soil_stability_tall %>%
      dplyr::group_by(EvaluationID, Veg) %>%
      dplyr::summarize(rating = round(mean(Rating), digits = 2)) %>%
      as.data.frame()
  }

  # merge all soil stability rating calculations
  soil_stability_rating_all <- do.call("rbind", soil_stability_rating)

  # if tall=FALSE spread into a wide format
  if (!tall) {
    soil_stability_rating_all <- soil_stability_rating_all %>%
      tidyr::pivot_wider(id_cols = c("EvaluationID"), names_from = Veg, names_prefix = "SoilStability_", values_from = rating)
  }

  return(soil_stability_rating_all)
}
