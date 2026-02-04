#' Apply Slope Rules to Update Data
#'
#' Iterates over the given rules and updates the PKNCA object setting inclusion/exclusion flags.
#' @param data PKNCA data object
#' @param slopes Data frame of slope rules (TYPE, RANGE, REASON, group columns)
#' @return Modified data object with updated flags
update_pknca_with_rules <- function(data, slopes) {
  browser()
  slope_groups <- intersect(group_vars(data), names(slopes))
  time_col <- data$conc$columns$time
  exclude_hl_col <- data$conc$columns$exclude_half.life
  include_hl_col <- data$conc$columns$include_half.life

  #####################################################
  # TODO: Make a better fix to understand why slopes is constructed 2 times
  # when adding exclusion, running NCA and then removing slope (#641)

  # Make sure when rows are removed no NA value is left
  slopes <- na.omit(slopes)
  #####################################################
  for (i in seq_len(nrow(slopes))) {
    # Determine the time range for the points adjusted
    range <- strsplit(as.character(slopes$RANGE[i]), ":")[[1]] %>%
      as.numeric() %>%
      range()
    # Build the condition dynamically for group columns and time range
    pnt_idx <- which(
      .are_points_in_groups(slopes, data) &
      .are_points_in_range(slopes$RANGE[i], data$conc$data[[time_col]])
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

.are_points_in_groups <- function(slopes, pknca_data) {
  slope_groups <- setdiff(names(slopes), c("TYPE", "RANGE", "REASON"))
  Reduce(`&`, lapply(slope_groups, function(col) {
    pknca_data$conc$data[[col]] == slopes[[col]]
    })
  )
}

.are_points_in_range <- function(range_str, time_vec) {
  parts <- strsplit(range_str, ",")[[1]]
  idx <- rep(FALSE, length(time_vec))
  for (part in parts) {
    if (grepl(":", part)) {
      bounds <- as.numeric(strsplit(part, ":")[[1]])
      idx <- idx | (time_vec >= bounds[1] & time_vec <= bounds[2])
    } else {
      val <- as.numeric(part)
      idx <- idx | (time_vec == val)
    }
  }
  idx
}
