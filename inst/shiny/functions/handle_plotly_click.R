#' Update Last Click Data for Slope Selection
#'
#' Checks if the user clicked on a different plot or dataset and updates
#' `last_click_data` accordingly. If an update is needed, the function exits early.
#'
#' @param last_click_data A reactive Values object storing the last clicked data.
#' @param manual_slopes A reactive Values object storing the manually added slope rules.
#' @param click_data A list containing the custom data from the plotly click event.
#' @param pknca_data A PKNCA data object containing concentration data and intervals.
#' @param plot_outputs A list of current plot outputs to be updated if needed.
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

  # Extract the information on the last and current points clicked
  pnt <- .extract_click_info(click_data, pknca_data)
  lstpnt <- .extract_click_info(last_click_data(), pknca_data)

  # Get relevant columns from data
  excl_hl_col <- pknca_data$conc$columns$exclude_half.life
  incl_hl_col <- pknca_data$conc$columns$include_half.life
  time_col <- pknca_data$conc$columns$time
browser()
  # Depending on the last click decide what half life adjustment rule to apply
  new_rule <- pnt$group
  # If it is the same point, consider the point excluded
  if (pnt$idx == lstpnt$idx) {
    new_rule$TYPE <- "Exclusion"
    new_rule$RANGE <- paste0(pnt$time)
    new_rule$REASON <- ""

  # If it is in the same plot (interval), consider all points in the time range included
  } else if (all.equal(pnt$int, lstpnt$int)) {
    new_rule$TYPE <- "Selection"
    new_rule$RANGE <- paste0(sort(c(pnt$time, lstpnt$time)), collapse = ":")
    new_rule$REASON <- ""
  } else {
    # Do nothing
    return(list(
      last_click_data = click_data,
      manual_slopes = manual_slopes(),
      plot_outputs = plot_outputs
    ))
  }

  # # Update the specific plot (interval) affected
  # updated_pknca$intervals <- pnt$int
  # updated_plot <- suppressWarnings(get_halflife_plot(updated_pknca))
  # plot_outputs[names(plot_outputs) %in% names(updated_plot)] <- updated_plot

  # Update manual_slopes without modifying it globally
  updated_slopes <- check_slope_rule_overlap(manual_slopes(), new_rule)

  # Return updated values
  list(
    last_click_data = NULL, # Action was finished and has to be updated
    manual_slopes = updated_slopes,
    plot_outputs = plot_outputs # TODO: NOT NEEDED ANYMORE
  )
}

  #' Helper to extract click info for handle_plotly_click
  #'
  #' @param click_data List from plotly click event
  #' @param pknca_data PKNCA data object
  #' @return List with idx, time, row, int, group
  .extract_click_info <- function(click_data, pknca_data) {
    idx <- as.numeric(click_data$customdata$ROWID)
    time <- as.numeric(click_data$x)
    row <- pknca_data$conc$data[idx, ]
    int <- pknca_data$intervals %>%
      merge(row, by = c(group_vars(pknca_data), "NCA_PROFILE")) %>%
      filter(start <= row[[pknca_data$conc$columns$time]] &
               end >= row[[pknca_data$conc$columns$time]]) %>%
      select(any_of(names(pknca_data$intervals)))
    group <- int %>%
      select(any_of(c(group_vars(pknca_data), "NCA_PROFILE")))
    list(idx = idx, time = time, row = row, int = int, group = group)
  }
