#' Filter dataset based on slope selections and exclusions
#'
#' This function filters main dataset based on provided slope selections an exclusions.
#'
#' @param data     Data to filter. Must be `PKNCAdata` list, containing the `conc` element with
#'                 `PKNCAconc` list and appropriate data frame included under data.
#' @param slopes   Data frame with slopes selection rules, must contain the following columns:
#'                 `TYPE`, `PATIENT`, `PROFILE`, `RANGE`, `REASON`.
#' @param profiles List with available profiles for each `PATIENT`.
#' @param slope_groups List with column names that define the groups.
#'
#' @returns Original dataset, with `is.included.hl`, `is.excluded.hl` and `exclude_half.life`
#'          columns modified in accordance to the provided slope filters.
#' @importFrom dplyr filter group_by mutate
#' @export
filter_slopes <- function(data, slopes, profiles, slope_groups) {
  if (is.null(data) || is.null(data$conc) || is.null(data$conc$data))
    stop("Please provide valid data.")

  # Reset to 0 all previous (if done) changes #
  data$conc$data$is.included.hl <- FALSE
  data$conc$data$is.excluded.hl <- FALSE
  data$conc$data$exclude_half.life <- FALSE

  # If there is no specification there is nothing to save #
  if (is.null(slopes) || nrow(slopes) == 0) {
    return(data)
  }

  # Eliminate all rows with conflicting or blank values
  slopes <- slopes %>%
    semi_join(
      profiles
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
  data <- .apply_slope_rules(data, slopes, slope_groups)

  data$conc$data <- data$conc$data %>%
    group_by(!!!syms(slope_groups)) %>%
    mutate(exclude_half.life = {
      if (any(is.included.hl)) {
        is.excluded.hl | !is.included.hl
      } else {
        is.excluded.hl
      }
    })

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
check_slope_rule_overlap <- function(existing, new, slope_groups, .keep = FALSE) {

  # check if any rule already exists for specific patient and profile #
  existing_index <- which(
    existing$TYPE == new$TYPE &
      Reduce(`&`, lapply(slope_groups, function(col) {
        existing[[col]] == new[[col]]
      }))
  )

  if (length(existing_index) != 1) {
    if (length(existing_index) > 1)
      warning("More than one range for single patient, profile and rule type detected.")
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
#'        and `REASON` columns.
#' @param slope_groups A character vector specifying the group columns used for filtering.
#'
#' @returns description The modified `data` object with updated inclusion/exclusion flags
#'         and reasons in `data$conc$data`.
.apply_slope_rules <- function(data, slopes, slope_groups) {
  for (i in seq_len(nrow(slopes))) {
    # Build the condition dynamically for group columns
    selection_index <- which(
      Reduce(`&`, lapply(slope_groups, function(col) {
        data$conc$data[[col]] == slopes[[col]][i]
      })) &
        data$conc$data$IX %in% .eval_range(slopes$RANGE[i])
    )

    if (slopes$TYPE[i] == "Selection") {
      data$conc$data$is.included.hl[selection_index] <- TRUE
    } else {
      data$conc$data$is.excluded.hl[selection_index] <- TRUE
    }

    data$conc$data$REASON[selection_index] <- slopes$REASON[i]
  }

  data
}
