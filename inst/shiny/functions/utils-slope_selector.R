#' Slope Selector Utility Functions
#'
#' These helpers support the slope selection workflow by detecting changes in PKNCA data,
#' updating plots, and managing slope rule logic. Used internally by the slope selector module.

#' Detect changes between old and new PKNCA data
#'
#' Compares two PKNCA data objects to determine if the underlying data, half-life adjustments,
#' options, or selected intervals have changed. Used to decide when to update plots.
#' @param old Previous PKNCA data object
#' @param new New PKNCA data object
#' @param reason_col Optional column name for reasons (default: "REASON")
#' to ignore in data comparison
#' @return List with logicals: `in_data`, `in_hl_adj`, `in_options`, `in_selected_intervals`
detect_pknca_data_changes <- function(old, new, reason_col = "REASON") {
  excl_hl_col <- new$conc$columns$exclude_half.life
  incl_hl_col <- new$conc$columns$include_half.life
  list(
    in_data = if (is.null(old) & !is.null(new)) {
      TRUE
    } else {
      !identical(
        dplyr::select(
          old$conc$data,
          -any_of(c(excl_hl_col, incl_hl_col, reason_col))
        ),
        dplyr::select(
          new$conc$data,
          -any_of(c(excl_hl_col, incl_hl_col, reason_col))
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
    in_options = !identical(old$options$min.hl.points, new$options$min.hl.points),
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
handle_hl_adj_change <- function(new_pknca_data, old_pknca_data,
                                 plot_outputs, profile_data = NULL) {
  excl_hl_col <- new_pknca_data$conc$columns$exclude_half.life
  incl_hl_col <- new_pknca_data$conc$columns$include_half.life
  new_concdata <- new_pknca_data$conc$data
  old_concdata <- old_pknca_data$conc$data

  ix_excl_changes <- which(new_concdata[[excl_hl_col]] != old_concdata[[excl_hl_col]])
  ix_incl_changes <- which(
    paste0(new_concdata[[incl_hl_col]]) != paste0(old_concdata[[incl_hl_col]])
  )

  if (length(c(ix_excl_changes, ix_incl_changes)) > 0) {
    time_col <- new_pknca_data$conc$columns$time
    affected_groups <- new_concdata[c(ix_excl_changes, ix_incl_changes), ] %>%
      select(any_of(c(group_vars(new_pknca_data), time_col))) %>%
      merge(new_pknca_data$intervals, by = group_vars(new_pknca_data)) %>%
      filter(!!sym(time_col) >= start, !!sym(time_col) <= end) %>%
      select(-any_of(time_col)) %>%
      distinct()
    return(update_plots_with_pknca(
      new_pknca_data, plot_outputs, profile_data, affected_groups
    ))
  }
  list(plots = plot_outputs, data = profile_data)
}

#' Handle interval changes
#'
#' Updates plots when the set of selected intervals changes (e.g., analyte/profile selection).
#' @param new_pknca_data New PKNCA data object
#' @param old_pknca_data Previous PKNCA data object
#' @param plot_outputs Current plot outputs (named list)
#' @return Updated plot_outputs (named list)
handle_interval_change <- function(new_pknca_data, old_pknca_data,
                                   plot_outputs, profile_data = NULL) {
  # Join on identity columns only (not parameter flags) so that
  # parameter selection changes are treated as updates, not add+remove.
  id_cols <- intersect(
    c(group_vars(new_pknca_data), "start", "end"),
    intersect(names(new_pknca_data$intervals), names(old_pknca_data$intervals))
  )
  new_intervals <- anti_join(
    new_pknca_data$intervals,
    old_pknca_data$intervals,
    by = id_cols
  )
  rm_intervals <- anti_join(
    old_pknca_data$intervals,
    new_pknca_data$intervals,
    by = id_cols
  )
  if (nrow(new_intervals) > 0) {
    affected_groups <- new_intervals %>%
      select(any_of(c(group_vars(new_pknca_data), "start", "end"))) %>%
      merge(unique(PKNCA::getGroups(new_pknca_data$conc)), all.x = TRUE) %>%
      select(any_of(c(group_vars(new_pknca_data), "start", "end"))) %>%
      distinct()
    result <- update_plots_with_pknca(
      new_pknca_data, plot_outputs, profile_data, affected_groups
    )
    plot_outputs <- result$plots
    profile_data <- result$data
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
    profile_data <- profile_data[!names(profile_data) %in% rm_plot_names]
  }
  list(plots = plot_outputs, data = profile_data)
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
  existing <- filter(existing, !is.na(TYPE))

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
  existing
}

#' Update plots with PKNCA data (for affected intervals)
#'
#' Regenerates plots for the specified intervals in the plot_outputs list.
#' @param pknca_data PKNCA data object
#' @param plot_outputs Named list of current plot outputs
#' @param intervals_to_update Data frame of intervals to update (default: all in pknca_data)
#' @return Updated plot_outputs (named list)
update_plots_with_pknca <- function(pknca_data, plot_outputs,
                                    profile_data = NULL,
                                    intervals_to_update = NULL) {
  if (is.null(intervals_to_update)) {
    intervals_to_update <- pknca_data$intervals %>%
      select(any_of(c(group_vars(pknca_data), "start", "end"))) %>%
      distinct()
  }
  if (nrow(intervals_to_update) == 0) {
    return(list(plots = plot_outputs, data = profile_data))
  }
  # Get the intervals of the plots affected by the current rules
  pknca_data$intervals <- inner_join(
    intervals_to_update,
    pknca_data$intervals,
    by = intersect(names(intervals_to_update), names(pknca_data$intervals))
  )
  hl_result <- suppressWarnings(
    get_halflife_plots(pknca_data, title_vars = "ATPTREF")
  )
  plot_outputs[names(hl_result[["plots"]])] <- hl_result[["plots"]]
  if (is.null(profile_data)) profile_data <- list()
  profile_data[names(hl_result[["data"]])] <- hl_result[["data"]]
  list(plots = plot_outputs, data = profile_data)
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

#' Arrange Plots by Group Columns
#'
#' Orders a named list of plots according to specified grouping columns.
#' Assumes a specific naming format (i.e, 'col1: val1, col2: val2, ...').
#'
#' @param named_list A named list of plots, with names in the format 'col1: val1, col2: val2, ...'.
#' @param group_cols Character vector of column names to sort by.
#' @importFrom dplyr arrange across all_of
#' @return A named list of plots, ordered by the specified group columns.
arrange_plots_by_groups <- function(named_list, group_cols) {
  if (length(named_list) == 0) return(named_list)
  plot_df <- parse_plot_names_to_df(named_list)
  arranged_df <- plot_df %>%
    arrange(across(all_of(group_cols)))
  named_list[arranged_df$PLOTID]
}

#' Identify profile IDs whose half-life results violate flag rules
#'
#' Evaluates each profile's NCA results against the checked flag rules
#' and returns the IDs of profiles that are flagged (i.e. at least one
#' rule is violated) or where lambda.z is NA (failed fit).
#'
#' @param profile_data Named list of per-profile data frames from
#'   `get_halflife_plots()`. Each data frame contains columns like
#'   `adj.r.squared`, `r.squared`, `span.ratio`, `lambda.z`.
#' @param flags Named list of flag rules from NCA settings. Each element
#'   has `is.checked` (logical) and `threshold` (numeric).
#' @return Character vector of flagged profile IDs (names from profile_data).
.get_flagged_profile_ids <- function(profile_data, flags) {
  # Map flag names to the corresponding column in profile data
  flag_col_map <- c(
    R2ADJ = "adj.r.squared",
    R2 = "r.squared",
    LAMZSPN = "span.ratio"
  )

  # Only evaluate checked flags that have a matching column
  active_flags <- purrr::keep(flags, function(f) isTRUE(f$is.checked))
  active_flags <- active_flags[names(active_flags) %in% names(flag_col_map)]

  if (length(active_flags) == 0) return(names(profile_data))

  vapply(names(profile_data), function(pid) {
    df <- profile_data[[pid]]
    # Always show profiles where lambda.z failed
    if (all(is.na(df$lambda.z))) return(TRUE)

    # Check each active flag: value below threshold means flagged
    any(vapply(names(active_flags), function(flag_name) {
      col <- flag_col_map[[flag_name]]
      if (!col %in% names(df)) return(FALSE)
      val <- df[[col]][1]
      if (is.na(val)) return(TRUE)
      val < active_flags[[flag_name]]$threshold
    }, logical(1)))
  }, logical(1)) |>
    Filter(isTRUE, x = _) |>
    names()
}

#' Identify profile IDs whose half-life results violate flag rules
#'
#' Evaluates each profile's NCA results against the checked flag rules
#' and returns the IDs of profiles that are flagged (i.e. at least one
#' rule is violated) or where lambda.z is NA (failed fit).
#'
#' @param profile_data Named list of per-profile data frames from
#'   `get_halflife_plots()`. Each data frame contains columns like
#'   `adj.r.squared`, `r.squared`, `span.ratio`, `lambda.z`.
#' @param flags Named list of flag rules from NCA settings. Each element
#'   has `is.checked` (logical) and `threshold` (numeric).
#' @return Character vector of flagged profile IDs (names from profile_data).
.get_flagged_profile_ids <- function(profile_data, flags) {
  # Map flag names to the corresponding column in profile data
  flag_col_map <- c(
    R2ADJ = "adj.r.squared",
    R2 = "r.squared",
    LAMZSPN = "span.ratio"
  )

  # Only evaluate checked flags that have a matching column
  active_flags <- purrr::keep(flags, function(f) isTRUE(f$is.checked))
  active_flags <- active_flags[names(active_flags) %in% names(flag_col_map)]

  if (length(active_flags) == 0) return(names(profile_data))

  vapply(names(profile_data), function(pid) {
    df <- profile_data[[pid]]
    # Always show profiles where lambda.z failed
    if (all(is.na(df$lambda.z))) return(TRUE)

    # Check each active flag: value below threshold means flagged
    any(vapply(names(active_flags), function(flag_name) {
      col <- flag_col_map[[flag_name]]
      if (!col %in% names(df)) return(FALSE)
      val <- df[[col]][1]
      if (is.na(val)) return(TRUE)
      val < active_flags[[flag_name]]$threshold
    }, logical(1)))
  }, logical(1)) |>
    Filter(isTRUE, x = _) |>
    names()
}

