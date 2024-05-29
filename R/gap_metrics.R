#' Calculate the number, length, and percent of gaps
#'
#' @description Calculate the number, length, and percent of gaps by plot or line. Calculations taken directly from Terradactyl package to match Terrestrial AIM indicator calculations.
#'
#' @param gap_tall Raw tables as imported from TerrADat use gather_gap .
#' @param tall Logical. If \code{TRUE} then the returned data frame will be tall rather than wide and will not have observations for non-existent values e.g., if no data fell into a group on a plot, there will be no row for that group on that plot. Defaults to \code{FALSE}.
#' @param by_line Logical. If \code{TRUR} then results will be reported further grouped by line using the \code{LineKey} field from the data forms. Defaults to \code{FALSE}.
#' @param breaks Vector of all break values. Defaults to \code{20,25, 51, 100, 200}
#' @param type String. Specifies the type of gap calculation \code{"canopy", "basal", "perennial canopy"}
#' @returns List of three data frames summarizing gap percent, count, and length (in cm) of gaps by their size category.

# Percent Gap
#' @export gap_cover
#' @rdname gap_cover
gap_cover <- function(gap_tall,
                      tall = FALSE,
                      breaks = c(20, 25, 51, 101, 201),
                      type = "canopy",
                      by_line = FALSE) {

  # For how deep to group. Always by plot, sometimes by line
  if (by_line) {
    level <- rlang::quos(EvaluationID, LineKey)
    level_colnames <- c("EvaluationID", "LineKey")
  } else {
    level <- rlang::quos(EvaluationID)
    level_colnames <- c("EvaluationID")
  }

  ## Convert the line lengths to the same units as the gaps
  # if metric (gap$Measure==1) then multiply by 100 to convert to centimeters
  gap_tall$LineLength <- gap_tall$LineLength*100

  ## Note if this is Basal or Canopy Gap by removing gaps from the opposite type.
  # "NA"s in RecType occur when there are no gaps
  if (type == "canopy") {
    gap_tall <- subset(gap_tall, RecType %in% "C")
  }
  if (type == "basal") {
    gap_tall <- subset(gap_tall, RecType %in% "B")
  }
  if (type == "perennial canopy") {
    gap_tall <- subset(gap_tall, RecType %in% "P")
  }

  # Summarize total line length for the plot
  gap_tall <- gap_tall %>%
    # get the distinct PrimaryKey-LineKey combinations
    dplyr::distinct(EvaluationID, LineKey, .keep_all = TRUE) %>%
    dplyr::group_by(!!!level) %>%
    unique() %>%
    dplyr::summarize(total_line_length = sum(LineLength)) %>%

    # Merge back with original gap data
    dplyr::left_join(gap_tall, ., by = level_colnames)

  # Check for site visits with less than 3 transects and create warning message
  # if there are any.
  shortgap <- gap_tall%>%
    dplyr::filter(total_line_length < 7500)%>%
    dplyr::pull(EvaluationID)

  if(length(shortgap) > 0 & by_line == F){
    warning("There are ", length(shortgap), " EvaluationIDs with a total line length less than 7500 m. ")
  }

  # Find the interval class for each gap
  breaks <- c(breaks, Inf)
  gap_tall$interval <- cut(gap_tall$Gap, breaks = breaks, right = FALSE)
  gap_tall$interval <- gap_tall$interval %>%
    as.character() %>%
    replace(., is.na(.), "NoGap")

  # Clean up the interval labels. They currently are formatted like "[25,51)" but we'd like them as "25-51"
  gap_tall$interval <- gsub(x = gap_tall$interval,
                            pattern = "^\\[",
                            replacement = "")
  gap_tall$interval <- gsub(x = gap_tall$interval,
                            pattern = ",",
                            replacement = "-")
  gap_tall$interval <- gsub(x = gap_tall$interval,
                            pattern = "\\)$",
                            replacement = "")

  # Summarize gaps by interval class
  gap_summary <- gap_tall %>%
    dplyr::group_by(!!!level, total_line_length, interval) %>%
    # calculate number of gaps,total length of gaps, and percent of gaps
    # in each indicator category
    dplyr::summarize(
      n = length(Gap),
      length = sum(Gap)
    ) %>%
    dplyr::mutate(., percent = round(100 * (length / total_line_length), digits = 2)) %>%
    dplyr::ungroup()

  # Subset the fields we need to output
  if (by_line) {
    gap_summary <- gap_summary %>%
      dplyr::select(EvaluationID, LineKey, total_line_length, interval, n, length, percent)
  } else {
    gap_summary <- gap_summary %>%
      dplyr::select(EvaluationID, total_line_length, interval, n, length, percent)
  }
  # quarantined by joe brehm 8/26, with all other rounding code. Need to make these optional parameters
  # constrain to 2 digits
  # gap_summary$percent <- round(gap_summary$percent, digits = 2)

  # Convert to wide format
  percent <- gap_summary %>%
    dplyr::select(., -n, -length) %>%
    tidyr::spread(key = interval, value = percent, fill = 0)
  n <- gap_summary %>%
    dplyr::select(., -percent, -length) %>%
    tidyr::spread(key = interval, value = n, fill = 0)
  length <- gap_summary %>%
    dplyr::select(., -n, -percent) %>%
    tidyr::spread(key = interval, value = length, fill = 0)

  # add absent columns
  if(!("20-25" %in% gap_summary$interval)){
    percent$`20-25` <- 0
    n$`20-25` <- 0
    length$`20-25` <- 0
  }

  if(!("25-51" %in% gap_summary$interval)){
    percent$`25-51` <- 0
    n$`25-51` <- 0
    length$`25-51` <- 0
  }

  if(!("51-101" %in% gap_summary$interval)){
    percent$`51-101` <- 0
    n$`51-101` <- 0
    length$`51-101` <- 0
  }

  if(!("101-201" %in% gap_summary$interval)){
    percent$`101-201` <- 0
    n$`101-201` <- 0
    length$`101-201` <- 0
  }

  if(!("201-Inf" %in% gap_summary$interval)){
    percent$`201-Inf` <- 0
    n$`201-Inf` <- 0
    length$`201-Inf` <- 0
  }

  ## If tall=FALSE, then convert to wide format
  if (!tall) {
    gap_summary <- list("percent" = percent, "n" = n, "length" = length)
  } else { # Convert back to tall, this adds zeros in needed columns
    gap_summary <- percent %>% tidyr::gather(
      key = gap_class,
      value = percent,
      -PrimaryKey,
      -total_line_length
    )
    gap_summary <- n %>%
      tidyr::gather(
        key = gap_class,
        value = n,
        -PrimaryKey,
        -total_line_length
      ) %>%
      merge(gap_summary, allow.cartesian = TRUE)
    gap_summary <- length %>%
      tidyr::gather(
        key = gap_class,
        value = length,
        -PrimaryKey,
        -total_line_length
      ) %>%
      merge(gap_summary, allow.cartesian = TRUE)
  }

  return(gap_summary)
}
