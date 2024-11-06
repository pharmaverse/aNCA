#' Filter dataset based on slope selections and exclusions
#'
#' This function filters main dataset based on provided slope selections an exclusions.
#'
#' @param data     Data to filter. Must be `PKNCAdata` list, containing the `conc` element with
#'                 `PKNCAconc` list and appropriate data frame included under data.
#' @param slopes   Data frame with slopes selection rules, must contain the following columns:
#'                 `TYPE`, `PATIENT`, `PROFILE`, `IXrange`, `REASON`.
#' @param profiles List with available profiles for each `PATIENT`.
#'
#' @returns Original dataset, with `is.included.hl`, `is.excluded.hl` and `exclude_half.life`
#'          columns modified in accordance to the provided slope filters.
#' @export
.filter_slopes <- function(data, slopes, profiles) {
  if (is.null(data) || is.null(data$conc) || is.null(data$conc$data))
    stop("Please provide valid data.")

  # Reset to 0 all previous (if done) changes #
  data$conc$data$is.included.hl <- FALSE
  data$conc$data$is.excluded.hl <- FALSE
  data$conc$data$exclude_half.life <- FALSE

  # If there is no specification there is nothing to save #
  if (nrow(slopes) == 0) {
    return(data)
  }

  # Eliminate all rows with conflicting or blank values
  slopes <- slopes %>%
    dplyr::filter(
      TYPE %in% c("Selection", "Exclusion"),
      PATIENT %in% names(profiles),
      PROFILE %in% unname(unlist(profiles[PATIENT])),
      all(!is.na(sapply(IXrange, function(x) .eval_range(x))))
    ) %>%
    # Eliminate duplicated records within the same profile
    dplyr::filter(
      !duplicated(
        paste0(PATIENT, PROFILE, IXrange, fromLast = TRUE),
        !(duplicated(paste0(PATIENT, PROFILE), fromLast = TRUE))
      )
    )

  # Update the exclusion/selection data for Lambda based on the current exc/sel table #
  for (i in seq_len(nrow(slopes))) {
    selection_index <- which(
      data$conc$data$USUBJID == slopes$PATIENT[i] &
        data$conc$data$DOSNO == slopes$PROFILE[i] &
        data$conc$data$IX %in% .eval_range(slopes$IXrange[i])
    )

    if (slopes$TYPE[i] == "Selection") {
      data$conc$data$is.included.hl[selection_index] <- TRUE
    } else {
      data$conc$data$is.excluded.hl[selection_index] <- TRUE
    }

    data$conc$data$REASON[selection_index] <- slopes$REASON[i]
  }

  data$conc$data <- data$conc$data %>%
    dplyr::group_by(STUDYID, USUBJID, PCSPEC, DOSNO) %>%
    dplyr::mutate(exclude_half.life = {
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
#' @returns Data frame with full ruleset, adjusted for new rules.
#' @export
.check_slope_rule_overlap <- function(existing, new) {
  # check if any rule already exists for specific patient and profile #
  existing_index <- which(
    existing$TYPE == new$TYPE &
      existing$PATIENT == new$PATIENT &
      existing$PROFILE == new$PROFILE
  )

  if (length(existing_index) != 1) {
    if (length(existing_index) > 1)
      log_warn("More than one range for single patient, profile and rule type detected.")
    return(rbind(existing, new))
  }

  existing_range <- .eval_range(existing$IXrange[existing_index])
  new_range <- .eval_range(new$IXrange)

  is_inter <- length(intersect(existing_range, new_range)) != 0
  is_diff <- length(setdiff(new_range, existing_range)) != 0

  if (is_diff) {
    existing$IXrange[existing_index] <- unique(c(existing_range, new_range)) %>%
      .compress_range()

  } else if (is_inter) {
    existing$IXrange[existing_index] <- setdiff(existing_range, new_range) %>%
      .compress_range()
  }

  dplyr::filter(existing, !is.na(IXrange))
}
