# Inline CSS for the self-contained validation report (no external assets)
.CDISC_REPORT_CSS <- paste(
  "body{font-family:Arial,Helvetica,sans-serif;color:#222;margin:24px;}",
  "h1{font-size:20px;margin-bottom:4px;}",
  ".meta{color:#555;font-size:13px;margin-bottom:16px;}",
  ".banner{padding:12px 16px;border-radius:6px;font-weight:bold;",
  "font-size:16px;margin:16px 0;}",
  ".pass{background:#e6f4ea;color:#1e7e34;border:1px solid #a3d3af;}",
  ".fail{background:#fdecea;color:#a71d2a;border:1px solid #f0b3b8;}",
  "table{border-collapse:collapse;width:100%;font-size:13px;}",
  "th,td{border:1px solid #ddd;padding:6px 8px;text-align:left;",
  "vertical-align:top;}",
  "th{background:#f2f2f2;}",
  ".sev-error{color:#a71d2a;font-weight:bold;}",
  ".sev-warning{color:#946200;font-weight:bold;}",
  ".st-pass{color:#1e7e34;font-weight:bold;}",
  ".st-error{color:#a71d2a;font-weight:bold;}",
  ".st-unknown{color:#946200;font-weight:bold;}",
  ".st-skipped{color:#777;}",
  "h2{font-size:16px;margin-top:28px;}",
  ".section-note{color:#555;font-size:12px;margin:4px 0 10px;}",
  ".footer{color:#777;font-size:12px;margin-top:20px;",
  "border-top:1px solid #eee;padding-top:10px;}",
  sep = ""
)

# Escape a character vector for safe inclusion in HTML text
.cdisc_html_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

# Render a data frame as an HTML table. `class_fn(col, value)` optionally
# returns a CSS class for a given cell (or NA for none).
.cdisc_render_table <- function(df, class_fn = NULL) {
  header <- paste0(
    "<tr>",
    paste0("<th>", .cdisc_html_escape(names(df)), "</th>", collapse = ""),
    "</tr>"
  )

  rows <- vapply(seq_len(nrow(df)), function(i) {
    cells <- vapply(names(df), function(col) {
      val <- .cdisc_html_escape(df[[col]][i])
      cls <- if (!is.null(class_fn)) class_fn(col, df[[col]][i]) else NA_character_
      if (!is.null(cls) && !is.na(cls)) {
        sprintf("<td class=\"%s\">%s</td>", cls, val)
      } else {
        sprintf("<td>%s</td>", val)
      }
    }, character(1))
    paste0("<tr>", paste0(cells, collapse = ""), "</tr>")
  }, character(1))

  paste0("<table>", header, paste0(rows, collapse = ""), "</table>")
}

# Render the findings table body (or a conforming message) as an HTML string
.cdisc_report_body <- function(findings) {
  if (is.null(findings) || nrow(findings) == 0) {
    return("<p>All checked datasets conform to the declared data types.</p>")
  }
  sev_class <- function(col, value) {
    if (col != "Severity") return(NA_character_)
    if (identical(value, "error")) "sev-error" else "sev-warning"
  }
  .cdisc_render_table(findings, class_fn = sev_class)
}

# Count findings of a given severity (0 for empty/NULL)
.cdisc_count_severity <- function(findings, severity) {
  if (is.null(findings) || nrow(findings) == 0) return(0)
  sum(findings$Severity == severity, na.rm = TRUE)
}

# Build the PASS/FAIL banner (class + text) from the error/warning counts
.cdisc_report_banner <- function(n_error, n_warning) {
  if (n_error == 0) {
    list(
      class = "banner pass",
      text = sprintf("PASS - no data-type errors (%d warning(s)).", n_warning)
    )
  } else {
    list(
      class = "banner fail",
      text = sprintf("FAIL - %d error(s), %d warning(s).", n_error, n_warning)
    )
  }
}

# Build the report metadata block (project, datasets, timestamp) as HTML
.cdisc_report_meta <- function(project, datasets_checked, generated_at) {
  lines <- c(
    if (!is.null(project) && nzchar(project)) {
      sprintf("Project: %s", .cdisc_html_escape(project))
    },
    if (!is.null(datasets_checked) && length(datasets_checked) > 0) {
      sprintf(
        "Datasets checked: %s",
        .cdisc_html_escape(paste(datasets_checked, collapse = ", "))
      )
    },
    sprintf("Generated: %s", .cdisc_html_escape(format(generated_at)))
  )
  paste0("<div class=\"meta\">", paste0(lines, collapse = "<br/>"), "</div>")
}

# Render the optional per-variable summary section (empty string when absent)
.cdisc_summary_section <- function(summary) {
  if (is.null(summary)) return("")
  paste0(
    "<h2>Variable summary</h2>",
    "<p class=\"section-note\">Descriptive overview of every exported ",
    "variable: declared label and type, expected vs. observed class, ",
    "declared vs. observed length, and per-variable status.</p>",
    .cdisc_report_summary(summary)
  )
}

# Render the per-variable descriptive summary as an HTML string
.cdisc_report_summary <- function(summary_df) {
  if (is.null(summary_df) || nrow(summary_df) == 0) {
    return("<p>No variables to describe.</p>")
  }
  status_class <- function(col, value) {
    if (col != "Status") return(NA_character_)
    switch(
      as.character(value),
      pass = "st-pass",
      error = "st-error",
      unknown = "st-unknown",
      skipped = "st-skipped",
      NA_character_
    )
  }
  .cdisc_render_table(summary_df, class_fn = status_class)
}

#' Render CDISC validation findings as a self-contained HTML report
#'
#' Produces a styled, standalone HTML string (inline CSS, no external assets):
#' a PASS/FAIL banner, the problems found by [validate_cdisc_types()] as the
#' first section (so issues are seen immediately), an optional per-variable
#' descriptive summary as a second section, and a footer noting the
#' metadata-driven scope.
#'
#' @param findings A findings data frame as returned by
#'   [validate_cdisc_types()].
#' @param datasets_checked Optional character vector of dataset keys that were
#'   checked, used for the report metadata line. Defaults to `NULL`.
#' @param project Optional project name shown in the report header. Defaults to
#'   `NULL`.
#' @param generated_at Timestamp shown in the report. Defaults to
#'   `Sys.time()`.
#' @param summary Optional per-variable descriptive summary data frame as
#'   returned by [describe_cdisc_variables()]. When supplied, it is rendered as
#'   a second section after the problems. Defaults to `NULL`.
#'
#' @returns A single character string containing a complete HTML document.
#' @export
#'
#' @examples
#' findings <- validate_cdisc_types(list())
#' html <- cdisc_validation_report(findings)
cdisc_validation_report <- function(findings,
                                    datasets_checked = NULL,
                                    project = NULL,
                                    generated_at = Sys.time(),
                                    summary = NULL) {
  n_error <- .cdisc_count_severity(findings, "error")
  n_warning <- .cdisc_count_severity(findings, "warning")
  banner <- .cdisc_report_banner(n_error, n_warning)

  meta_html <- .cdisc_report_meta(project, datasets_checked, generated_at)

  footer <- paste0(
    "<div class=\"footer\">This report checks storage class and maximum ",
    "length as declared in the package metadata. It does not replace full ",
    "CDISC conformance validation (controlled terminology and structural ",
    "rules are out of scope).</div>"
  )

  problems_section <- paste0(
    "<h2>Problems</h2>",
    "<p class=\"section-note\">Issues that need attention. ",
    "Error-severity findings block saving.</p>",
    .cdisc_report_body(findings)
  )

  paste0(
    "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"/>",
    "<title>CDISC Data-Type Validation Report</title>",
    "<style>", .CDISC_REPORT_CSS, "</style></head><body>",
    "<h1>CDISC Data-Type Validation Report</h1>",
    meta_html,
    "<div class=\"", banner$class, "\">", banner$text, "</div>",
    problems_section,
    .cdisc_summary_section(summary),
    footer,
    "</body></html>"
  )
}

#' Validate CDISC data and write an HTML QC report for the export flow
#'
#' Convenience wrapper for the export/Save flow: runs
#' [validate_cdisc_types()], writes the HTML report from
#' [cdisc_validation_report()] into `target_dir`, and returns the findings
#' alongside the report path and a save-blocking flag.
#'
#' @param cdisc_data Named list of CDISC data frames keyed `pp`, `adpp`,
#'   and/or `adnca` (as returned by [export_cdisc()]).
#' @param target_dir Directory the report is written into. Created if it does
#'   not exist.
#' @param filename File name for the report. Defaults to
#'   `"cdisc_validation_report.html"`.
#' @param project Optional project name shown in the report. Defaults to
#'   `NULL`.
#' @param metadata Metadata data frame passed to [validate_cdisc_types()].
#'   Defaults to `metadata_nca_variables`.
#'
#' @returns A list with elements `findings` (the findings data frame),
#'   `summary` (the per-variable descriptive summary), `report_path` (path to
#'   the written HTML report), and `blocks_save` (logical from
#'   [cdisc_validation_blocks_save()]).
#' @export
#'
#' @examples
#' cdisc_data <- list(
#'   adnca = data.frame(STUDYID = "S1", AVAL = 1.2, stringsAsFactors = FALSE)
#' )
#' res <- write_cdisc_validation_report(cdisc_data, target_dir = tempdir())
#' res$blocks_save
write_cdisc_validation_report <- function(cdisc_data,
                                          target_dir,
                                          filename = "cdisc_validation_report.html",
                                          project = NULL,
                                          metadata = metadata_nca_variables) {
  findings <- validate_cdisc_types(cdisc_data, metadata = metadata)
  summary <- describe_cdisc_variables(cdisc_data, metadata = metadata)

  datasets_checked <- intersect(names(cdisc_data), names(CDISC_KEY_DS_MAP))

  html <- cdisc_validation_report(
    findings,
    datasets_checked = datasets_checked,
    project = project,
    summary = summary
  )

  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  }
  report_path <- file.path(target_dir, filename)
  writeLines(html, report_path)

  list(
    findings = findings,
    summary = summary,
    report_path = report_path,
    blocks_save = cdisc_validation_blocks_save(findings)
  )
}
