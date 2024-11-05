#' Filter dataset based on slope selections and exclusions
#'
#' This function filters main dataset based on provided slope selections an exclusions.
#'
#' @param data     Data to filter. Must be PKNCAdata list, containing the conc element with
#'                 PKNCAconc list and appropriate data frame included under data.
#' @param slopes   Data frame with slopes selection rules, must contain the following columns:
#'                 TYPE, PATIENT, PROFILE, IXrange, REASON.
#' @param profiles List with available profiles for each PATIENT.
#'
#' @returns Original dataset, with is.included.hl, is.excluded.hl and exclude_half.life columns
#'          modified in accordance to the provided slope filters.
filter_slopes <- function(data, slopes, profiles) {
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
    filter(
      PATIENT %in% names(profiles),
      PROFILE %in% unname(unlist(profiles[PATIENT])),
      all(!is.na(sapply(IXrange, function(x) eval(parse(text = x))))) &
        all(!is.null(sapply(IXrange, function(x) eval(parse(text = x))))),
    )  %>%
    # Eliminate duplicated records within the same profile
    filter(
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
        data$conc$data$IX %in% eval(parse(text = slopes$IXrange[i]))
    )

    if (slopes$TYPE[i] == "Selection") {
      data$conc$data$is.included.hl[selection_index] <- TRUE
    } else {
      data$conc$data$is.excluded.hl[selection_index] <- TRUE
    }

    data$conc$data$REASON[selection_index] <- slopes$REASON[i]
  }

  data$conc$data <- data$conc$data %>%
    group_by(STUDYID, USUBJID, PCSPEC, DOSNO) %>%
    mutate(exclude_half.life = {
      if (any(is.included.hl)) {
        is.excluded.hl | !is.included.hl
      } else {
        is.excluded.hl
      }
    })

  data
}
