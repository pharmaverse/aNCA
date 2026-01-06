#' Apply Slope Rules to Update Data
#'
#' Iterates over the given rules and updates the PKNCA object setting inclusion/exclusion flags.
#' @param data PKNCA data object
#' @param slopes Data frame of slope rules (TYPE, RANGE, REASON, group columns)
#' @return Modified data object with updated flags
update_pknca_with_rules <- function(data, slopes) {
  slope_groups <- intersect(group_vars(data), names(slopes))
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
