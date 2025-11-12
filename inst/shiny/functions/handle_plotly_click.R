
#' Handle Plotly Click for Slope Selection
#'
#' This function processes a plotly click event in the slope selection UI. It determines whether
#' the double click should add a new exclusion or selection rule to the manual slopes table
#' (same point = exclusion, two points in same plot = selection).
#'
#' @param last_click_data A reactiveVal storing the last clicked plotly data (or NULL).
#' @param manual_slopes A reactiveVal storing the current manual slopes table (data.frame).
#' @param click_data The new plotly click event data (list with customdata).
#' @param pknca_data The current PKNCA data object (for context and group info).
#' @return List with updated last_click_data and manual_slopes.
#'
#' @details
#' - If the user clicks the same point twice, an exclusion rule is added for that point.
#' - If the user clicks two points in the same interval, a selection rule is added for the range.
#' - Otherwise, no rule is added and the click is just stored.
handle_plotly_click <- function(last_click_data, manual_slopes, click_data, pknca_data) {
  req(click_data, click_data$customdata)
  # If there is no previous click, store this click and do nothing else
  if (is.null(last_click_data())) {
    return(list(
      last_click_data = click_data,
      manual_slopes = manual_slopes()
    ))
  }

  # Extract info for current and last click (group, interval, time, etc.)
  pnt <- .extract_click_info(click_data, pknca_data)
  lstpnt <- .extract_click_info(last_click_data(), pknca_data)

  # Decide what rule to add based on click context
  new_rule <- pnt$group
  if (pnt$idx == lstpnt$idx) {
    # Same point clicked twice: add exclusion rule for that point
    new_rule$TYPE <- "Exclusion"
    new_rule$RANGE <- paste0(pnt$time)
    new_rule$REASON <- ""
  } else if (identical(pnt$int, lstpnt$int)) {
    # Two points in same interval: add selection rule for the range
    new_rule$TYPE <- "Selection"
    new_rule$RANGE <- paste0(
      sort(c(pnt$time, lstpnt$time)),
      collapse = ":"
    )
    new_rule$REASON <- ""
  } else {
    # Clicks not in same interval: just update last_click_data, no rule
    return(list(
      last_click_data = click_data,
      manual_slopes = manual_slopes()
    ))
  }

  # Add or update the rule in the manual_slopes table
  updated_slopes <- check_slope_rule_overlap(manual_slopes(), new_rule)

  # Return updated values (reset last_click_data to NULL to await next click)
  list(
    last_click_data = NULL,
    manual_slopes = updated_slopes
  )
}


#' Extract Click Info for Slope Selection
#'
#' Helper for handle_plotly_click. Given plotly click data and PKNCA data, returns a list with:
#' - idx: row index in conc data
#' - time: time value of click
#' - row: full row from conc data
#' - int: interval(s) in which the point falls
#' - group: grouping columns for the interval
#'
#' @param click_data List from plotly click event
#' @param pknca_data PKNCA data object
#' @return List with idx, time, row, int, group
.extract_click_info <- function(click_data, pknca_data) {
  idx <- as.numeric(click_data$customdata$ROWID)
  time <- as.numeric(click_data$x)
  row <- pknca_data$conc$data[idx, ]
  int <- pknca_data$intervals %>%
    merge(row, by = c(group_vars(pknca_data))) %>%
    filter(
      start <= row[[pknca_data$conc$columns$time]] &
        end >= row[[pknca_data$conc$columns$time]]
    ) %>%
    select(any_of(names(pknca_data$intervals)))
  group <- int %>%
    select(any_of(c(group_vars(pknca_data), "ATPTREF")))
  list(idx = idx, time = time, row = row, int = int, group = group)
}
