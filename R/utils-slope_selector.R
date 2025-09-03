#' Filter dataset based on slope selections and exclusions
#'
#' This function filters main dataset based on provided slope selections an exclusions.
#'
#' @param data     Data to filter. Must be `PKNCAdata` list, containing the `conc` element with
#'                 `PKNCAconc` list and appropriate data frame included under data.
#' @param slopes   A data frame containing slope rules, including `TYPE`, `RANGE`,
#'        and `REASON` columns. May also have grouping columns (expected to match slope_groups)
#' @param profiles List with available profiles for each `SUBJECT`.
#' @param slope_groups List with column names that define the groups.
#' @param check_reasons Whether to check if all selections have REASONS stated. If this is `TRUE`
#'                      and not all selections have a reason provided, an error will be thrown.
#'
#' @returns Original dataset, with `is.included.hl`, `is.excluded.hl` and `exclude_half.life`
#'          columns modified in accordance to the provided slope filters.
#' @importFrom dplyr filter group_by mutate select all_of
#' @export
filter_slopes <- function(data, slopes, profiles, slope_groups, check_reasons = FALSE) {
  print("filter_slopes")
  if (is.null(data) || is.null(data$conc) || is.null(data$conc$data))
    stop("Please provide valid data.")

  # If there is no specification there is nothing to save #
  if (is.null(slopes) || nrow(slopes) == 0) {
    return(data)
  }

  if (check_reasons) {
    exclusions <- filter(slopes, TYPE == "Exclusion")
    if (any(exclusions$REASON == "")) {
      missing_reasons <- filter(exclusions, REASON == "") %>%
        select(-REASON) %>%
        apply(1, \(x) paste0(x, collapse = " "))

      stop(
        "No reason provided for the following exclusions:\n",
        missing_reasons
      )
    }
  }

  # Reset to 0 all previous (if done) changes #
  data$conc$data$is.included.hl <- FALSE
  data$conc$data$is.excluded.hl <- FALSE
  data$conc$data$exclude_half.life <- FALSE
  data$conc$data$include_half.life <- NA

  # Eliminate all rows with conflicting or blank values
  slopes <- slopes %>%
    semi_join(
      profiles,
      by = slope_groups
    ) %>%
    filter(all(!is.na(sapply(RANGE, .eval_range))))

  if (nrow(slopes) != 0) {
    # Go over all rules and check if there is no overlap - if there is, edit accordingly
    slopes <- purrr::reduce(
      split(slopes, seq_len(nrow(slopes))),
      .f = ~ check_slope_rule_overlap(.x, .y, slope_groups, .keep = TRUE)
    )
  }

  # Update the exclusion/selection data for Lambda based on the current exc/sel table
  data <- .update_pknca_with_rules(data, slopes, slope_groups)

  data$conc$data <- data$conc$data %>%
    group_by(!!!syms(slope_groups)) %>%
    mutate(exclude_half.life = {
      if (any(is.included.hl)) {
        is.excluded.hl | !is.included.hl
      } else {
        is.excluded.hl
      }
    },
    include_half.life = case_when(
      is.included.hl ~ TRUE,
      TRUE ~ NA
    )) %>%
    ungroup()

  data
}

#' Check overlap between existing and new slope rulesets
#'
#' Takes in tables with existing and incoming selections and exclusions, finds any overlap and
#' differences, edits the ruleset table accordingly.
#'
#' @param existing Data frame with existing selections and exclusions.
#' @param new      Data frame with new rule to be added or removed.
#' @param slope_groups List with column names that define the groups.
#' @param .keep    Whether to force keep fully overlapping rulesets. If FALSE, it will be assumed
#'                 that the user wants to remove rule if new range already exists in the dataset.
#'                 If TRUE, in that case full range will be kept.
#' @returns Data frame with full ruleset, adjusted for new rules.
#' @export
check_slope_rule_overlap <- function(existing, new, .keep = FALSE) {
  print("check_slope_rule_overlap")
  slope_groups <- setdiff(names(new), c("TYPE", "RANGE", "REASON"))

  # check if any rule already exists for specific subject and profile #
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
#' This function iterates over the given slopes and updates the `data$conc$data` object
#' by setting inclusion or exclusion flags based on the slope conditions.
#'
#' @param data A list containing concentration data (`data$conc$data`) with columns that
#'        need to be updated based on the slope rules.
#' @param slopes A data frame containing slope rules, including `TYPE`, `RANGE`,
#'        and `REASON` columns. May also have grouping columns (expected to match slope_groups)
#' @param slope_groups A character vector specifying the group columns used for filtering.
#'
#' @returns description The modified `data` object with updated inclusion/exclusion flags
#'         and reasons in `data$conc$data`.
.update_pknca_with_rules <- function(data, slopes) {
  print(".update_pknca_with_rules")
  browser()
  slope_groups <- group_vars(data)
  time_col <- data$conc$columns$time
  exclude_hl_col <- data$conc$columns$exclude_half.life
  include_hl_col <- data$conc$columns$include_half.life

  for (i in seq_len(nrow(slopes))) {
    # Determine the time range for the points adjusted
    range <- strsplit(as.character(slopes$RANGE[i]), ":")[[1]]
    if (length(range) == 1) range <- rep(range, 2)

    # Build the condition dynamically for group columns and time range
    pnt_idx <- which(
      Reduce(`&`, lapply(slope_groups, function(col) {
        data$conc$data[[col]] == slopes[[col]][i]
      })) &
        between(data$conc$data[[time_col]], as.numeric(range[[1]]), as.numeric(range[[2]]))
    )

    if (slopes$TYPE[i] == "Selection") {
      data$conc$data[[include_hl_col]][pnt_idx] <- TRUE
    } else {
      data$conc$data[[exclude_hl_col]][pnt_idx] <- TRUE
    }

    data$conc$data$REASON[pnt_idx] <- paste0(
      data$conc$data$REASON[pnt_idx],
      rep(slopes$REASON[i], length(pnt_idx))
    )
    print(".update_pknca_with_rules")
  }

  data
}

.update_plots_with_rules <- function(pknca_data, manual_slopes, plot_outputs) {
  print(".update_plots_with_rules")
  pknca_for_plots <- .update_pknca_with_rules(pknca_data, manual_slopes)
  browser()
  pknca_for_plots$intervals <- inner_join(
    manual_slopes %>% select(any_of(c(group_vars(pknca_for_plots), "NCA_PROFILE"))),
    pknca_for_plots$intervals,
    by = c(group_vars(pknca_for_plots), "NCA_PROFILE")
  )
  updated_plots <- suppressWarnings(get_halflife_plot(pknca_for_plots))
  plot_outputs[names(plot_outputs) %in% names(updated_plots)] <- updated_plots
  plot_outputs
  print(".update_plots_with_rules")
}
