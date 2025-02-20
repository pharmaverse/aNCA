#' Update Last Click Data for Slope Selection
#'
#' Checks if the user clicked on a different plot or dataset and updates
#' `last_click_data` accordingly. If an update is needed, the function exits early.
#'
#' @param last_click_data A reactive Values object storing the last clicked data.
#' @param manual_slopes A reactive Values object storing the manually added slope rules.
#' @param slopes_groups A character vector of slope grouping column names.
#' @param click_data A list containing the custom data from the plotly click event.
#'
#' @returns Returns a list with updated `last_click_data` and `manual_slopes`.
#'
handle_plotly_click <- function(last_click_data, manual_slopes, slopes_groups, click_data) {
  req(click_data, click_data$customdata)

  identifiers <- click_data$customdata
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
  
  # Create a copy of last_click_data 
  updated_click_data <- last_click_data
  
  # Check if the selection has changed
  updated <- any(
    sapply(
      slopes_groups,
      function(col) dynamic_values[[col]] != updated_click_data[[tolower(col)]]
    )
  )
  
  if (updated) {
    for (col in slopes_groups) {
      updated_click_data[[tolower(col)]] <- dynamic_values[[col]]
    }
    updated_click_data$idx_pnt <- idx_pnt
    
    # Return updated last_click_data, but do not modify global state
    return(list(last_click_data = updated_click_data, manual_slopes = manual_slopes()))
  }
  
  # Create new rule as a local object
  new_rule <- as.data.frame(
    lapply(
      c(
        dynamic_values,
        TYPE = if (idx_pnt != updated_click_data$idx_pnt) "Selection" else "Exclusion",
        RANGE = paste0(updated_click_data$idx_pnt, ":", idx_pnt),
        REASON = "[Graphical selection: Please add reason]"
      ),
      as.character  # Convert everything to character
    ),
    stringsAsFactors = FALSE
  )
  
  # Update manual_slopes without modifying it globally
  updated_slopes <- check_slope_rule_overlap(manual_slopes(), new_rule, slopes_groups)
  
  # Reset last_click_data dynamically
  for (col in names(dynamic_values)) {
    updated_click_data[[tolower(col)]] <- ""
  }
  updated_click_data$idx_pnt <- ""
  
  # Return updated values
  return(list(last_click_data = updated_click_data, manual_slopes = updated_slopes))
}

