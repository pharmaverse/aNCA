#' Summary-exclusion marker used in TLG listings.
#' @noRd
.SUMMARY_EXCLUSION_MARKER <- "*"

#' NCA-exclusion marker used in TLG listings.
#' @noRd
.NCA_EXCLUSION_MARKER <- "#"

#' Split one exclusion-reason value into individual reasons.
#' @param value A scalar reason value, optionally semicolon-separated.
#' @return A character vector of non-empty reasons.
#' @noRd
.split_exclusion_reasons <- function(value) {
  if (length(value) == 0 || is.na(value) || !nzchar(trimws(as.character(value)))) {
    return(character(0))
  }
  reasons <- trimws(unlist(strsplit(as.character(value), ";\\s*")))
  unique(reasons[nzchar(reasons)])
}

#' Return CDISC/PKNCA exclusion-reason columns for a flag.
#' @param data A data frame.
#' @param flag_var Exclusion flag column.
#' @return Character vector of reason columns present in `data`.
#' @noRd
.exclusion_reason_columns <- function(data, flag_var) {
  preferred <- switch(
    flag_var,
    PKSUMXF = "PKSUM1RS",
    PPSUMXF = "PPSUMRSN",
    character(0)
  )
  nca_cols <- if (identical(flag_var, "NCAXFL")) {
    grep("^NCA[0-9]+XRS$", names(data), value = TRUE)
  } else {
    character(0)
  }
  raw_nca <- if (identical(flag_var, "NCAXFL")) "exclude" else character(0)
  intersect(c(preferred, nca_cols, raw_nca), names(data))
}

#' Build deterministic reason markers for one dataset.
#' @importFrom stats setNames
#' @param data A data frame.
#' @param flag_var Exclusion flag column.
#' @param marker_prefix Prefix used for generated markers.
#' @param summary_only If `TRUE`, only the explicit flag marks a row excluded.
#' @param dictionary Optional precomputed dataset-wide reason dictionary.
#' @return A list containing row reasons, markers, and the dictionary.
#' @noRd
.exclusion_details <- function(
  data, flag_var, marker_prefix, summary_only = FALSE, dictionary = NULL
) {
  reason_cols <- .exclusion_reason_columns(data, flag_var)
  row_reasons <- lapply(seq_len(nrow(data)), function(i) {
    unique(unlist(lapply(reason_cols, function(col) {
      .split_exclusion_reasons(data[[col]][i])
    })))
  })
  has_flag <- if (flag_var %in% names(data)) {
    !is.na(data[[flag_var]]) & data[[flag_var]] == "Y"
  } else {
    rep(FALSE, nrow(data))
  }
  excluded <- if (summary_only) has_flag else has_flag | lengths(row_reasons) > 0

  if (is.null(dictionary)) {
    reasons <- sort(unique(as.character(unlist(row_reasons[excluded]))), method = "radix")
    # Reserve the bare marker when a flagged record has no reason, so it never
    # means both a named reason and an unspecified exclusion in the same dataset.
    generic <- any(excluded & lengths(row_reasons) == 0)
    dictionary <- setNames(
      strrep(marker_prefix, seq_along(reasons) + generic),
      reasons
    )
  }

  row_markers <- vapply(seq_along(row_reasons), function(i) {
    if (!excluded[i]) return("")
    markers <- unname(dictionary[intersect(names(dictionary), row_reasons[[i]])])
    if (length(markers) == 0) marker_prefix else paste(markers, collapse = ", ")
  }, character(1))

  list(
    excluded = excluded,
    row_reasons = row_reasons,
    dictionary = dictionary,
    markers = row_markers,
    reason_cols = reason_cols
  )
}

#' Add reason-specific exclusion footnotes.
#' @param footnote Existing parsed footnote.
#' @param data A data frame.
#' @param flag_var Exclusion flag column.
#' @param marker_prefix Marker prefix (`*` or `#`).
#' @param label Footnote label.
#' @param summary_only If `TRUE`, only the explicit flag marks a row excluded.
#' @param dictionary Optional dataset-wide reason dictionary.
#' @return Footnote vector with exclusion notes appended.
#' @noRd
.add_exclusion_footnotes <- function(
  footnote, data, flag_var, marker_prefix, label, summary_only = FALSE,
  dictionary = NULL
) {
  details <- .exclusion_details(
    data, flag_var, marker_prefix, summary_only, dictionary
  )
  if (!any(details$excluded)) return(footnote)

  present_reasons <- names(details$dictionary)[vapply(
    names(details$dictionary),
    function(reason) {
      any(vapply(details$row_reasons[details$excluded], function(x) reason %in% x, logical(1)))
    },
    logical(1)
  )]
  notes <- vapply(present_reasons, function(reason) {
    source_col <- details$reason_cols[
      vapply(details$reason_cols, function(col) {
        any(vapply(data[[col]][details$excluded], function(value) {
          reason %in% .split_exclusion_reasons(value)
        }, logical(1)))
      }, logical(1))
    ][1]
    paste0(
      unname(details$dictionary[reason]), " ", label, ": ", source_col,
      " = \"", reason, "\"."
    )
  }, character(1))
  if (any(details$excluded & lengths(details$row_reasons) == 0)) {
    notes <- c(notes, paste0(
      marker_prefix, " ", label, " (", flag_var, " = \"Y\")."
    ))
  }
  notes <- unname(notes[nzchar(notes)])
  if (is.null(footnote) || length(footnote) == 0) return(notes)
  if (all(is.na(footnote) | !nzchar(footnote))) return(notes)
  c(footnote, setdiff(notes, footnote))
}

#' Add summary-exclusion listing footnotes.
#' @param footnote Existing parsed footnote.
#' @param data A data frame.
#' @param flag_var Summary-exclusion flag column.
#' @param dictionary Optional dataset-wide reason dictionary.
#' @return Footnote vector with summary-exclusion notes appended.
#' @noRd
.add_sum_excl_footnote <- function(footnote, data, flag_var, dictionary = NULL) {
  .add_exclusion_footnotes(
    footnote, data, flag_var, .SUMMARY_EXCLUSION_MARKER,
    "Summary and mean plots excluded", summary_only = TRUE,
    dictionary = dictionary
  )
}

#' Add NCA-exclusion listing footnotes.
#' @param footnote Existing parsed footnote.
#' @param data A data frame.
#' @param flag_var NCA-exclusion flag column.
#' @param dictionary Optional dataset-wide reason dictionary.
#' @return Footnote vector with NCA-exclusion notes appended.
#' @noRd
.add_nca_excl_footnote <- function(footnote, data, flag_var, dictionary = NULL) {
  .add_exclusion_footnotes(
    footnote, data, flag_var, .NCA_EXCLUSION_MARKER,
    "NCA excluded record", dictionary = dictionary
  )
}

#' Mark displayed values for flagged records.
#' @param data A data frame.
#' @param flag_var Exclusion flag column.
#' @param value_vars Candidate value columns, in display priority order.
#' @param marker Marker prefix to append.
#' @param na_str Display text for flagged missing values.
#' @param summary_only If `TRUE`, only the explicit flag marks a row excluded.
#' @param dictionary Optional dataset-wide reason dictionary.
#' @return The input data with the marker appended to the first value column.
#' @noRd
.mark_excl_vals <- function(
  data, flag_var, value_vars, marker, na_str = "NA", summary_only = FALSE,
  dictionary = NULL
) {
  value_col <- intersect(value_vars, names(data))[1]
  if (is.na(value_col)) {
    return(data)
  }

  details <- .exclusion_details(
    data, flag_var, marker, summary_only, dictionary
  )
  if (!any(details$excluded)) return(data)

  data[[value_col]] <- as.character(data[[value_col]])
  data[[value_col]][details$excluded & is.na(data[[value_col]])] <- na_str
  data[[value_col]][details$excluded] <- paste0(
    data[[value_col]][details$excluded], details$markers[details$excluded]
  )

  data
}

#' Mark displayed values excluded from summaries.
#' @param data A data frame.
#' @param flag_var Summary-exclusion flag column.
#' @param value_vars Candidate value columns, in display priority order.
#' @param na_str Display text for flagged missing values.
#' @return The input data with the marker appended to the first value column.
#' @noRd
.mark_sum_excl_vals <- function(
  data, flag_var, value_vars, na_str = "NA", dictionary = NULL
) {
  .mark_excl_vals(
    data, flag_var, value_vars, .SUMMARY_EXCLUSION_MARKER, na_str,
    summary_only = TRUE, dictionary = dictionary
  )
}

#' Mark displayed values excluded from NCA calculations.
#' @param data A data frame.
#' @param flag_var NCA-exclusion flag column.
#' @param value_vars Candidate value columns, in display priority order.
#' @param na_str Display text for flagged missing values.
#' @return The input data with the marker appended to the first value column.
#' @noRd
.mark_nca_excl_vals <- function(
  data, flag_var, value_vars, na_str = "NA", dictionary = NULL
) {
  .mark_excl_vals(
    data, flag_var, value_vars, .NCA_EXCLUSION_MARKER, na_str,
    dictionary = dictionary
  )
}
