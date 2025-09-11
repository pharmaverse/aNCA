#' Slope Selector Utility Functions
#'
#' These helpers support the slope selection workflow by detecting changes in PKNCA data,
#' updating plots, and managing slope rule logic. Used internally by the slope selector module.

#' Detect changes between old and new PKNCA data
#'
#' Compares two PKNCA data objects to determine if the underlying data, half-life adjustment columns,
#' or selected intervals have changed. Used to decide when to update plots.
#' @param old Previous PKNCA data object
#' @param new New PKNCA data object
#' @param excl_hl_col Name of exclusion column
#' @param incl_hl_col Name of inclusion column
#' @return List with logicals: in_data, in_hl_adj, in_selected_intervals
detect_pknca_data_changes <- function(old, new, excl_hl_col, incl_hl_col) {
  list(
    in_data = if (is.null(old) & !is.null(new)) TRUE else {
      !identical(
        dplyr::select(old$conc$data, -any_of(c(excl_hl_col, incl_hl_col))),
        dplyr::select(new$conc$data, -any_of(c(excl_hl_col, incl_hl_col)))
      )
    },
    in_hl_adj = !identical(old$conc$data[[excl_hl_col]], new$conc$data[[excl_hl_col]]) |
      !identical(old$conc$data[[incl_hl_col]], new$conc$data[[incl_hl_col]]),
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
    dplyr::select(new_pknca_data$conc$data, any_of(c(group_vars(new_pknca_data), "NCA_PROFILE", excl_hl_col, incl_hl_col))),
    dplyr::select(old_pknca_data$conc$data, any_of(c(group_vars(old_pknca_data), "NCA_PROFILE", excl_hl_col, incl_hl_col))),
    by = c(group_vars(new_pknca_data), "NCA_PROFILE", excl_hl_col, incl_hl_col)
  ) %>%
    select(any_of(c(group_vars(new_pknca_data), "NCA_PROFILE"))) %>%
    distinct()
  .update_plots_with_pknca(new_pknca_data, plot_outputs, affected_groups)
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
    new_pknca_data$intervals, old_pknca_data$intervals,
    by = intersect(names(new_pknca_data$intervals), names(old_pknca_data$intervals))
  )
  rm_intervals <- anti_join(
    old_pknca_data$intervals, new_pknca_data$intervals,
    by = intersect(names(new_pknca_data$intervals), names(old_pknca_data$intervals))
  )
  if (nrow(new_intervals) > 0) {
    affected_groups <- new_intervals %>%
      select(any_of(c(group_vars(new_pknca_data), "start", "end"))) %>%
      distinct()
    plot_outputs <- .update_plots_with_pknca(new_pknca_data, plot_outputs, affected_groups)
  }
  if (nrow(rm_intervals) > 0) {
    rm_plot_names <- rm_intervals %>%
      select(any_of(c(group_vars(new_pknca_data), "start", "end"))) %>%
      distinct() %>%
      mutate(across(everything(), as.character)) %>%
      mutate(id = purrr::pmap_chr(
        .,
        function(...) {
          vals <- list(...)
          paste0(names(vals), ": ", vals, collapse = ", ")
        }
      )) %>%
      pull(id)
    plot_outputs <- plot_outputs[!names(plot_outputs) %in% rm_plot_names]
  }
  plot_outputs
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
  # check if any rule already exists for specific subject and profile
  existing_index <- which(
    existing$TYPE == new$TYPE &
      Reduce(`&`, lapply(slope_groups, function(col) {
        existing[[col]] == new[[col]]
      }))
  )
  if (length(existing_index) != 1) {
    if (length(existing_index) > 1)
      warning("More than one range for single subject, profile and rule type detected.")
    return(rbind(existing, new))
  }
  existing_range <- .eval_range(existing$RANGE[existing_index])
  new_range <- .eval_range(new$RANGE)
  is_inter <- length(intersect(existing_range, new_range)) != 0
  is_diff <- length(setdiff(new_range, existing_range)) != 0
  if (is_diff || .keep) {
    existing$RANGE[existing_index] <- unique(c(existing_range, new_range)) %>%
      .compress_range()
  } else if (is_inter) {
    existing$RANGE[existing_index] <- setdiff(existing_range, new_range) %>%
      .compress_range()
  }
  dplyr::filter(existing, !is.na(RANGE))
}
  

#' Apply Slope Rules to Update Data
#'
#' Iterates over the given slopes and updates the data$conc$data object by setting inclusion/exclusion flags.
#' @param data PKNCA data object
#' @param slopes Data frame of slope rules (TYPE, RANGE, REASON, group columns)
#' @return Modified data object with updated flags
.update_pknca_with_rules <- function(data, slopes) {
  slope_groups <- group_vars(data)
  time_col <- data$conc$columns$time
  exclude_hl_col <- data$conc$columns$exclude_half.life
  include_hl_col <- data$conc$columns$include_half.life
  for (i in seq_len(nrow(slopes))) {
    # Determine the time range for the points adjusted
    range <- strsplit(as.character(slopes$RANGE[i]), ":")[[1]] %>%
      as.numeric() %>%
      range()
    # Build the condition dynamically for group columns and time range
    pnt_idx <- which(
      Reduce(`&`, lapply(slope_groups, function(col) {
        data$conc$data[[col]] == slopes[[col]][i]
      })) &
        between(data$conc$data[[time_col]], range[[1]], range[[2]])
    )
    if (slopes$TYPE[i] == "Selection") {
      data$conc$data[[include_hl_col]][pnt_idx] <- TRUE
    } else if (slopes$TYPE[i] == "Exclusion") {
      data$conc$data[[exclude_hl_col]][pnt_idx] <- TRUE
    } else {
      stop("Unknown TYPE in slopes: ", slopes$TYPE[i])
    }
    data$conc$data$REASON[pnt_idx] <- paste0(
      data$conc$data$REASON[pnt_idx],
      rep(slopes$REASON[i], length(pnt_idx))
    )
  }
  data
}


#' Update plots with manual slope rules
#'
#' Updates the plot_outputs list by applying manual slopes to the PKNCA data and regenerating affected plots.
#' @param pknca_data PKNCA data object
#' @param manual_slopes Data frame of manual slope rules
#' @param plot_outputs Named list of current plot outputs
#' @param slopes_to_update Optional: data frame of intervals to update (default: all in manual_slopes)
#' @return Updated plot_outputs (named list)
.update_plots_with_rules <- function(pknca_data, manual_slopes, plot_outputs, slopes_to_update = NULL) {
  pknca_for_plots <- .update_pknca_with_rules(pknca_data, manual_slopes)
  if (is.null(slopes_to_update)) {
    slopes_to_update <- manual_slopes %>%
      select(any_of(c(group_vars(pknca_for_plots), "NCA_PROFILE"))) %>%
      distinct()
  }
  if (nrow(slopes_to_update) == 0) return(plot_outputs)
  .update_plots_with_pknca(pknca_for_plots, plot_outputs, intervals_to_update = slopes_to_update)
}

#' Update plots with PKNCA data (for affected intervals)
#'
#' Regenerates plots for the specified intervals in the plot_outputs list.
#' @param pknca_data PKNCA data object
#' @param plot_outputs Named list of current plot outputs
#' @param intervals_to_update Data frame of intervals to update (default: all in pknca_data)
#' @return Updated plot_outputs (named list)
.update_plots_with_pknca <- function(pknca_data, plot_outputs, intervals_to_update = NULL) {
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
  updated_plots <- suppressWarnings(get_halflife_plot(pknca_data))
  plot_outputs[names(updated_plots)] <- updated_plots
  plot_outputs
}
