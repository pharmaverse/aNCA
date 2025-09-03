#' Update Last Click Data for Slope Selection
#'
#' Checks if the user clicked on a different plot or dataset and updates
#' `last_click_data` accordingly. If an update is needed, the function exits early.
#'
#' @param last_click_data A reactive Values object storing the last clicked data.
#' @param manual_slopes A reactive Values object storing the manually added slope rules.
#' @param click_data A list containing the custom data from the plotly click event.
#'
#' @returns Returns a list with updated `last_click_data` and `manual_slopes`.
#'
handle_plotly_click <- function(last_click_data, manual_slopes, click_data, pknca_data, plot_outputs) {
  req(click_data, click_data$customdata)
  print("current click:")
  print(click_data)
  print("last click:")
  print(last_click_data)
  # If there is no previous click to this one, store it and do nothing else
  if (is.null(last_click_data())) {
    return(list(
      last_click_data = click_data,
      manual_slopes = manual_slopes(),
      plot_outputs = plot_outputs
    ))
  }

  # Extract additional information of the point selected
  idx_pnt <- as.numeric(click_data$customdata$ROWID)
  time_pnt <- as.numeric(click_data$x)
  row_pnt <- pknca_data$conc$data[idx_pnt, ]
  int_pnt <- pknca_data$intervals %>%
    merge(row_pnt, by = intersect(names(.), names(row_pnt))) %>%
    filter(start <= row_pnt[[pknca_data$conc$columns$time]] &
             end >= row_pnt[[pknca_data$conc$columns$time]]) %>%
    select(any_of(names(pknca_data$intervals)))
  group_pnt <- int_pnt %>%
    select(any_of(c(group_vars(pknca_data), "NCA_PROFILE")))
  
  # Do the same for the previous click
  last_click_data <- last_click_data()
  idx_lstpnt <- as.numeric(last_click_data$customdata$ROWID)
  time_lstpnt <- as.numeric(last_click_data$x)
  row_lstpnt <- pknca_data$conc$data[idx_lstpnt, ]
  int_lstpnt <- pknca_data$intervals %>%
    merge(row_lstpnt, by = intersect(names(.), names(row_lstpnt))) %>%
    filter(start <= row_lstpnt[[pknca_data$conc$columns$time]] &
             end >= row_lstpnt[[pknca_data$conc$columns$time]]) %>%
    select(any_of(names(pknca_data$intervals)))
  group_lstpnt <- int_lstpnt %>%
    select(any_of(c(group_vars(pknca_data), "NCA_PROFILE")))
  
  # Get relevant columns from data
  excl_hl_col <- pknca_data$conc$columns$exclude_half.life
  incl_hl_col <- pknca_data$conc$columns$include_half.life
  time_col <- pknca_data$conc$columns$time

  # Depending on the last click decide if it is an exclusion or inclusion of the point
  new_rule <- group_pnt
  if (idx_pnt == idx_lstpnt) {
    new_rule$TYPE <- "Exclusion"
    new_rule$RANGE <- time_pnt
    new_rule$REASON <- ""

  } else if (int_pnt == int_lstpnt) {
    new_rule$TYPE <- "Inclusion"
    new_rule$RANGE <- paste0(sort(c(time_pnt, time_lstpnt)), collapse = ":")
    new_rule$REASON <- ""
  } else {
    # Do nothing
    return(list(
      last_click_data = click_data,
      manual_slopes = manual_slopes(),
      plot_outputs = plot_outputs
    ))
  }

  # Update the concentration data to add manual half life adjustments
  updated_pknca <- .apply_slope_rules(pknca_data, new_rule)
  
  # Update the specific plot affected
  updated_pknca$intervals <- int_pnt
  updated_plot <- suppressWarnings(get_halflife_plot(updated_pknca))
  plot_outputs[names(plot_outputs) %in% names(updated_plot)] <- updated_plot
  browser()
  # Update manual_slopes without modifying it globally
  updated_slopes <- check_slope_rule_overlap(manual_slopes(), new_rule)

  # Return updated values
  list(
    last_click_data = NULL, # Action was finished and has to be updated
    manual_slopes = updated_slopes,
    plot_outputs = plot_outputs
  )
}
