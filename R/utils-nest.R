# Shared presentation boundary for the opt-in NEST pilots.
.nest_require <- function(packages) {
  minimum <- c(cards = "0.9.0", crane = "0.3.2", gtsummary = "2.6.1", gt = "1.4.0")
  available <- vapply(packages, function(pkg) {
    requireNamespace(pkg, quietly = TRUE) &&
      (!pkg %in% names(minimum) || utils::packageVersion(pkg) >= minimum[[pkg]])
  }, logical(1))
  missing <- packages[!available]
  if (length(missing)) {
    stop("NEST pilot requires: ", paste(missing, collapse = ", "),
         " at the minimum versions in DESCRIPTION. Install with install.packages(c(",
         paste(sprintf("'%s'", missing), collapse = ", "), ")).", call. = FALSE)
  }
}

.nest_annotations <- function(data, title, subtitle, footnote) {
  lapply(list(title = title, subtitle = subtitle, footnote = footnote), function(text) {
    gsub("<br>", "\n", paste(parse_annotation(data, text), collapse = "\n"), fixed = TRUE)
  })
}

.nest_label <- function(data, name) {
  label <- .get_var_label(data, name)
  if (is.null(label) || is.na(label) || !nzchar(label)) name else label
}

# Compact report headers without replacing custom labels or flat-file metadata.
.nest_header_label <- function(data, name) {
  label <- .nest_label(data, name)
  short <- c(TRT01A = "Treatment Group", USUBJID = "Subject", AVISIT = "Visit",
             PARAM = "PK Parameter", AVALU = "Unit")
  if (name %in% names(short) && label %in% c(name, get_label(name, "ADPP"))) {
    unname(short[[name]])
  } else {
    label
  }
}

# Each child key stays visible when any parent changes. Only the display is changed.
.nest_suppress_keys <- function(body, keys) {
  original <- body
  if (!nrow(body)) return(body)
  for (i in seq_along(keys)) {
    prefix <- original[keys[seq_len(i)]]
    same <- c(FALSE, vapply(seq_len(nrow(prefix) - 1L), function(j) {
      identical(unname(as.list(prefix[j, ])), unname(as.list(prefix[j + 1L, ])))
    }, logical(1)))
    body[[keys[i]]] <- as.character(original[[keys[i]]])
    body[[keys[i]]][same] <- NA_character_
  }
  body
}

.nest_attach <- function(table, data, annotations) {
  attr(table, "tlg_data") <- data
  attr(table, "tlg_annotations") <- lapply(annotations, function(x) {
    gsub("<br>", "\n", paste(x, collapse = "\n"), fixed = TRUE)
  })
  table
}

# One rendering path shared by the Shiny preview and HTML export.
.nest_gt <- function(table) {
  .nest_require(c("gtsummary", "gt"))
  notes <- attr(table, "tlg_annotations")
  result <- gtsummary::as_gt(table)
  if (nzchar(notes$title) || nzchar(notes$subtitle)) {
    result <- gt::tab_header(result, title = .nest_html_lines(notes$title),
                             subtitle = .nest_html_lines(notes$subtitle))
  }
  if (nzchar(notes$footnote)) {
    result <- gt::tab_source_note(result, .nest_html_lines(notes$footnote))
  }
  result
}

#' Keep annotation line breaks in gt, which collapses newlines in plain text.
#' @importFrom htmltools htmlEscape
#' @noRd
.nest_html_lines <- function(text) {
  gt::html(gsub("\n", "<br>", htmlEscape(text), fixed = TRUE))
}

# Keep the legacy first-row policy for equal repeated dose records. Reject
# ambiguous cells in the pilot instead of silently choosing a different value.
.nest_check_cells <- function(data, keys, value_var) {
  keys <- intersect(unique(keys), names(data))
  columns <- intersect(value_var, names(data))
  cells <- unique(data[c(keys, columns)])
  if (anyDuplicated(cells[keys])) {
    stop("NEST pilot: conflicting values or units within a reporting key (",
         paste(keys, collapse = ", "), "). Check for missing key values, or add ",
         "visit/dose/specimen variables to the grouping or split variables.",
         call. = FALSE)
  }
}

# Results are already rounded to three decimals. Format them without the session's
# significant-digit rounding; adding zero normalizes signed zero.
.nest_format_number <- function(x) {
  ifelse(is.na(x), "", trimws(formatC(x + 0, format = "f", digits = 3, drop0trailing = TRUE)))
}

# Interval identity must not depend on how many windows each subject retains.
# Only known urine parameters pool single collection intervals: their end times
# can differ by subject. Other parameters keep every supplied interval explicit.
.nest_interval_param <- function(data, param_var, keys) {
  if (!all(c(param_var, "PPSTINT", "PPENINT") %in% names(data))) return(data)
  interval <- !is.na(data$PPSTINT) & !is.na(data$PPENINT)
  keys <- intersect(unique(c(keys, param_var)), names(data))
  cells <- unique(data[interval, c(keys, "PPSTINT", "PPENINT"), drop = FALSE])
  repeated <- unique(cells[[param_var]][duplicated(cells[keys])])
  id <- rep(NA_character_, nrow(data))
  for (v in intersect(c("PARAMCD", "PPTESTCD", param_var), names(data))) {
    missing_id <- is.na(id) | !nzchar(trimws(id))
    id[missing_id] <- as.character(data[[v]])[missing_id]
  }
  urine <- metadata_nca_parameters[metadata_nca_parameters$TYPE %in% "Urine", ]
  urine_ids <- unlist(urine[c("PPTESTCD", "PPTEST", "PKNCA")], use.names = FALSE)
  rename <- interval & (!id %in% urine_ids | data[[param_var]] %in% repeated)
  param <- as.character(data[[param_var]])
  param[rename] <- paste0(param[rename], " (", data$PPSTINT[rename], "-",
                          data$PPENINT[rename], ")")
  attr(param, "label") <- attr(data[[param_var]], "label")
  data[[param_var]] <- param
  data
}
