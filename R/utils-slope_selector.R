#' Apply Slope Rules to Update Data
#'
#' Iterates over the given rules and updates the PKNCA object setting inclusion/exclusion flags.
#' @param data PKNCA data object
#' @param slopes Data frame of slope rules (TYPE, RANGE, REASON, group columns)
#' @returns Modified data object with updated flags
update_pknca_with_rules <- function(data, slopes) {
  slope_groups <- intersect(group_vars(data), names(slopes))
  time_col <- data$conc$columns$time
  exclude_hl_col <- data$conc$columns$exclude_half.life
  include_hl_col <- data$conc$columns$include_half.life

  # Defensive guard: drop any incomplete slope rules before applying them.
  # Editing the manual-slopes table (add exclusion -> run NCA -> remove a slope)
  # can momentarily leave rows with NA cells; na.omit keeps those partial rows
  # from producing invalid ranges below. The slope-selector reactivity was
  # reworked in #641, so the original "slopes constructed twice" behaviour may
  # no longer occur -- this guard is cheap and kept as a safety net.
  slopes <- na.omit(slopes)

  for (i in seq_len(nrow(slopes))) {
    # Determine the time range for the points adjusted
    range <- strsplit(as.character(slopes$RANGE[i]), ":")[[1]] %>%
      as.numeric() %>%
      range()
    # Build the condition dynamically for group columns and time range
    pnt_idx <- which(
      .are_points_in_groups(slopes[i, ], data) &
        .are_points_in_range(slopes$RANGE[i], data$conc$data[[time_col]])
    )

    if (slopes$TYPE[i] == "Selection") {
      data$conc$data[[include_hl_col]][pnt_idx] <- TRUE
    } else if (slopes$TYPE[i] == "Exclusion") {
      # Clear any inclusion on the same points to avoid PKNCA's
      # "cannot both include and exclude" error
      data$conc$data[[include_hl_col]][pnt_idx] <- NA
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

#' Resolve include/exclude half-life conflicts per profile
#'
#' PKNCA errors when both the `include_half.life` and `exclude_half.life`
#' columns contain non-NA values within the same interval. This resolves the
#' conflict on a per-profile basis (profiles are defined by the concentration
#' grouping columns): for each profile that has both flags in use, the inclusion
#' flag is cleared on the excluded points and that profile's exclude flags are
#' reset to `NA`. This turns mixed intent into include-only semantics (selected
#' points minus excluded points). Profiles without a conflict are left
#' untouched, so a standalone exclusion in one profile is never cleared just
#' because another profile has an inclusion.
#'
#' @param data PKNCA data object.
#' @returns The PKNCA data object with `conc$data` conflicts resolved.
resolve_hl_include_exclude_conflicts <- function(data) {  # nolint
  exclude_hl_col <- data$conc$columns$exclude_half.life
  include_hl_col <- data$conc$columns$include_half.life
  if (is.null(exclude_hl_col) || is.null(include_hl_col)) {
    return(data)
  }

  conc <- data$conc$data
  group_cols <- intersect(group_vars(data$conc), names(conc))

  # Identify each profile so conflicts are resolved within a profile only
  profile_id <- if (length(group_cols) > 0) {
    do.call(paste, c(conc[group_cols], sep = "\r"))
  } else {
    rep("1", nrow(conc))
  }

  for (id in unique(profile_id)) {
    rows <- which(profile_id == id)
    has_excl <- any(conc[[exclude_hl_col]][rows] %in% TRUE)
    has_incl <- any(conc[[include_hl_col]][rows] %in% TRUE)
    if (has_excl && has_incl) {
      excl_rows <- rows[conc[[exclude_hl_col]][rows] %in% TRUE]
      conc[[include_hl_col]][excl_rows] <- NA
      conc[[exclude_hl_col]][rows] <- NA
    }
  }

  data$conc$data <- conc
  data
}

.are_points_in_groups <- function(slopes, pknca_data) {
  slope_groups <- setdiff(names(slopes), c("TYPE", "RANGE", "REASON"))
  Reduce(`&`, lapply(slope_groups, function(col) {
    pknca_data$conc$data[[col]] == slopes[[col]]
  }))
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
