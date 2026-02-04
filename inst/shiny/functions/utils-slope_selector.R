#' Slope Selector Utility Functions
#'
#' These helpers support the slope selection workflow by detecting changes in PKNCA data,
#' updating plots, and managing slope rule logic. Used internally by the slope selector module.

#' Detect changes between old and new PKNCA data
#'
#' Compares two PKNCA data objects to determine if the underlying data, half-life adjustments,
#' or selected intervals have changed. Used to decide when to update plots.
#' @param old Previous PKNCA data object
#' @param new New PKNCA data object
#' @return List with logicals: `in_data`, `in_hl_adj`, `in_selected_intervals`
detect_pknca_data_changes <- function(old, new) {
  excl_hl_col <- new$conc$columns$exclude_half.life
  incl_hl_col <- new$conc$columns$include_half.life
  list(
    in_data = if (is.null(old) & !is.null(new)) {
      TRUE
    } else {
      !identical(
        dplyr::select(
          old$conc$data,
          -any_of(c(excl_hl_col, incl_hl_col))
        ),
        dplyr::select(
          new$conc$data,
          -any_of(c(excl_hl_col, incl_hl_col))
        )
      )
    },
    in_hl_adj = !identical(
      old$conc$data[[excl_hl_col]],
      new$conc$data[[excl_hl_col]]
    ) |
      !identical(
        old$conc$data[[incl_hl_col]],
        new$conc$data[[incl_hl_col]]
      ),
    in_selected_intervals = !identical(new$intervals, old$intervals)
  )
}

#' Handle half-life adjustment changes
#'
#' Updates only the plots affected by changes in half-life inclusion/exclusion columns.
#' @param new_pknca_data New PKNCA data object
#' @param old_pknca_data Previous PKNCA data object
#' @param plot_outputs Current plot outputs (named list)
#' @return Updated plot_outputs (named list)
handle_hl_adj_change <- function(new_pknca_data, old_pknca_data, plot_outputs) {
  excl_hl_col <- new_pknca_data$conc$columns$exclude_half.life
  incl_hl_col <- new_pknca_data$conc$columns$include_half.life
  affected_groups <- anti_join(
    dplyr::select(
      new_pknca_data$conc$data,
      any_of(c(group_vars(new_pknca_data), "ATPTREF", excl_hl_col, incl_hl_col))
    ),
    dplyr::select(
      old_pknca_data$conc$data,
      any_of(c(group_vars(old_pknca_data), "ATPTREF", excl_hl_col, incl_hl_col))
    ),
    by = c(group_vars(new_pknca_data), "ATPTREF", excl_hl_col, incl_hl_col)
  ) %>%
    select(any_of(c(group_vars(new_pknca_data), "ATPTREF"))) %>%
    distinct()
  update_plots_with_pknca(new_pknca_data, plot_outputs, affected_groups)
}

#' Handle interval changes
#'
#' Updates plots when the set of selected intervals changes (e.g., analyte/profile selection).
#' @param new_pknca_data New PKNCA data object
#' @param old_pknca_data Previous PKNCA data object
#' @param plot_outputs Current plot outputs (named list)
#' @return Updated plot_outputs (named list)
handle_interval_change <- function(new_pknca_data, old_pknca_data, plot_outputs) {
  new_intervals <- anti_join(
    new_pknca_data$intervals,
    old_pknca_data$intervals,
    by = intersect(
      names(new_pknca_data$intervals),
      names(old_pknca_data$intervals)
    )
  )
  rm_intervals <- anti_join(
    old_pknca_data$intervals,
    new_pknca_data$intervals,
    by = intersect(
      names(new_pknca_data$intervals),
      names(old_pknca_data$intervals)
    )
  )
  if (nrow(new_intervals) > 0) {
    affected_groups <- new_intervals %>%
      select(any_of(c(group_vars(new_pknca_data), "start", "end"))) %>%
      merge(unique(PKNCA::getGroups(new_pknca_data$conc)), all.x = TRUE) %>%
      select(any_of(c(group_vars(new_pknca_data), "start", "end"))) %>%
      distinct()
    plot_outputs <- update_plots_with_pknca(
      new_pknca_data,
      plot_outputs,
      affected_groups
    )
  }
  if (nrow(rm_intervals) > 0) {
    rm_plot_names <- rm_intervals %>%
      select(any_of(c(group_vars(new_pknca_data), "start", "end"))) %>%
      merge(unique(PKNCA::getGroups(new_pknca_data$conc)), all.x = TRUE) %>%
      select(any_of(c(group_vars(new_pknca_data), "start", "end"))) %>%
      distinct() %>%
      mutate(across(everything(), as.character)) %>%
      mutate(id = purrr::pmap_chr(
        .,
        function(...) {
          vals <- list(...)
          paste0(names(vals), "=", vals, collapse = "_")
        }
      )) %>%
      pull(id)
    plot_outputs <- plot_outputs[!names(plot_outputs) %in% rm_plot_names]
  }
  plot_outputs
}

#' Parse Plot Names to Data Frame
#'
#' Converts a named list of plots (with names in the format 'col1: val1, col2: val2, ...')
#' into a data frame with one row per plot and columns for each key.
#'
#' @param named_list A named list or vector, where names are key-value pairs separated by commas.
#' @return A data frame with columns for each key and a `PLOTID` column with the original names.
parse_plot_names_to_df <- function(named_list) {
  plot_names <- names(named_list)
  parsed <- lapply(plot_names, function(x) {
    pairs <- strsplit(x, "_\\s*")[[1]]
    kv <- strsplit(pairs, "=\\s*")
    setNames(
      vapply(kv, function(y) y[2], character(1)),
      vapply(kv, function(y) y[1], character(1))
    )
  })
  as.data.frame(
    do.call(rbind, parsed),
    stringsAsFactors = FALSE
  ) %>%
    mutate(PLOTID = names(named_list))
}

#' Check overlap between existing and new slope rulesets
#'
#' Takes in tables with existing and incoming selections and exclusions, finds any overlap and
#' differences, edits the ruleset table accordingly.
#' @param existing Data frame with existing selections and exclusions.
#' @param new      Data frame with new rule to be added or removed.
#' @param .keep    Whether to force keep fully overlapping rulesets. If FALSE, it will be assumed
#'                 that the user wants to remove rule if new range already exists in the dataset.
#'                 If TRUE, in that case full range will be kept.
#' @return Data frame with full ruleset, adjusted for new rules.
#' @export
check_slope_rule_overlap <- function(existing, new, .keep = FALSE) {
  slope_groups <- setdiff(names(new), c("TYPE", "RANGE", "REASON"))

  # Helper to match group columns between existing and new
  is_matching_cols <- function(cols_to_match, existing, new) {
    Reduce(
      `&`,
      lapply(cols_to_match, function(col) {
        existing[[col]] == new[[col]]
      })
    )
  }

  if (new$TYPE == "Exclusion") {
    # If is the same exclusion rule as an existing, remove the existing
    rows_with_same_cols <- is_matching_cols(c(slope_groups, "TYPE", "RANGE"), existing, new)
    if (sum(rows_with_same_cols) > 0) {
      existing <- existing[!rows_with_same_cols, ]
      return(existing)
    }
    # Otherwise, just add the new exclusion
    existing <- bind_rows(existing, new)
  }
  
  if (new$TYPE == "Selection") {
    # If it is a selection rule within the same group as an existing, modify the existing
    rows_with_same_groups <- is_matching_cols(c(slope_groups, "TYPE"), existing, new)
    if (sum(rows_with_same_groups) > 0) {
      existing <- existing[!rows_with_same_groups, ]
    }
    # Otherwise, just add the new selection
    existing <- bind_rows(existing, new)
  }
  
  return(existing)
}

#' Update plots with PKNCA data (for affected intervals)
#'
#' Regenerates plots for the specified intervals in the plot_outputs list.
#' @param pknca_data PKNCA data object
#' @param plot_outputs Named list of current plot outputs
#' @param intervals_to_update Data frame of intervals to update (default: all in pknca_data)
#' @return Updated plot_outputs (named list)
update_plots_with_pknca <- function(pknca_data, plot_outputs, intervals_to_update = NULL) {
  if (is.null(intervals_to_update)) {
    intervals_to_update <- pknca_data$intervals %>%
      select(any_of(c(group_vars(pknca_data), "start", "end"))) %>%
      distinct()
  }
  if (nrow(intervals_to_update) == 0) return(plot_outputs)
  # Get the intervals of the plots affected by the current rules
  pknca_data$intervals <- inner_join(
    intervals_to_update,
    pknca_data$intervals,
    by = intersect(names(intervals_to_update), names(pknca_data$intervals))
  )
  updated_plots <- suppressWarnings(get_halflife_plots(pknca_data)[["plots"]])
  plot_outputs[names(updated_plots)] <- updated_plots
  plot_outputs
}
