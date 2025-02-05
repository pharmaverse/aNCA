#' Update Last Click Data for Slope Selection
#'
#' Checks if the user clicked on a different plot or dataset and updates
#' `last_click_data` accordingly. If an update is needed, the function exits early.
#'
#' @param id The ID of the plotly object.
#' @param last_click_data A reactive Values object storing the last clicked data.
#' @param dynamic_values A named list containing values from the current click event.
#' @param idx_pnt The index of the clicked data point.
#' @param manual_slopes A reactive Values object storing the manually added slope rules.
#' @param slopes_groups A character vector of slope grouping column names.
#'
#' @returns Returns `NULL` and exits early if data was updated. Otherwise, continues execution.
#'
handle_plotly_click <- function(last_click_data, manual_slopes, slopes_groups) {
  click_data <- event_data("plotly_click")
  req(click_data, click_data$customdata)

  log_trace("slope_selector: plotly click detected")

  identifiers <- jsonlite::fromJSON(click_data$customdata)
  if (!all(names(identifiers) %in% c(slopes_groups, "IX"))) {
    stop("Error: Missing expected keys in customdata")
  }
  # Map identifiers dynamically
  dynamic_values <- setNames(
    lapply(slopes_groups, function(col) identifiers[[col]]),
    slopes_groups
  )

  # Extract additional information for idx_pnt
  idx_pnt <- identifiers$IX

  #Update data only if there is a change in the selection
  updated <- any(
    sapply(
      slopes_groups,
      function(col) dynamic_values[[col]] != last_click_data[[tolower(col)]]
    )
  )

  # Update last click data
  if (updated) {
    for (col in slopes_groups) {
      last_click_data[[tolower(col)]] <- dynamic_values[[col]]
    }
    last_click_data$idx_pnt <- idx_pnt
    return(NULL)
  }

  new_rule <- as.data.frame(
    lapply(
      c(
        dynamic_values,
        TYPE = if (idx_pnt != last_click_data$idx_pnt) "Selection" else "Exclusion",
        RANGE = paste0(last_click_data$idx_pnt, ":", idx_pnt),
        REASON = "[Graphical selection: Please add reason]"
      ),
      as.character  # Convert everything to character
    ),
    stringsAsFactors = FALSE
  )

  manual_slopes(.check_slope_rule_overlap(
    manual_slopes(),
    new_rule,
    slopes_groups
  )
  )

  # After adding new rule, reset last click data dynamically
  for (col in names(dynamic_values)) {
    last_click_data[[tolower(col)]] <- ""
  }
  last_click_data$idx_pnt <- ""
}
