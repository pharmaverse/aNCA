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

# Render the findings table body (or a conforming message) as an HTML string
.cdisc_report_body <- function(findings) {
  if (is.null(findings) || nrow(findings) == 0) {
    return("<p>All checked datasets conform to the declared data types.</p>")
  }

  header <- paste0(
    "<tr>",
    paste0("<th>", .cdisc_html_escape(names(findings)), "</th>", collapse = ""),
    "</tr>"
  )

  rows <- vapply(seq_len(nrow(findings)), function(i) {
    sev <- findings$Severity[i]
    sev_class <- if (identical(sev, "error")) "sev-error" else "sev-warning"
    cells <- vapply(names(findings), function(col) {
      val <- .cdisc_html_escape(findings[[col]][i])
      if (col == "Severity") {
        sprintf("<td class=\"%s\">%s</td>", sev_class, val)
      } else {
        sprintf("<td>%s</td>", val)
      }
    }, character(1))
    paste0("<tr>", paste0(cells, collapse = ""), "</tr>")
  }, character(1))

  paste0("<table>", header, paste0(rows, collapse = ""), "</table>")
}

#' Render CDISC validation findings as a self-contained HTML report
#'
#' Produces a styled, standalone HTML string (inline CSS, no external assets)
#' summarising the findings from [validate_cdisc_types()]: a PASS/FAIL banner,
#' a per-finding table, and a footer noting the metadata-driven scope.
#'
#' @param findings A findings data frame as returned by
#'   [validate_cdisc_types()].
#' @param datasets_checked Optional character vector of dataset keys that were
#'   checked, used for the report metadata line. Defaults to `NULL`.
#' @param project Optional project name shown in the report header. Defaults to
#'   `NULL`.
#' @param generated_at Timestamp shown in the report. Defaults to
#'   `Sys.time()`.
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
                                    generated_at = Sys.time()) {
  n_error <- if (is.null(findings) || nrow(findings) == 0) {
    0
  } else {
    sum(findings$Severity == "error", na.rm = TRUE)
  }
  n_warning <- if (is.null(findings) || nrow(findings) == 0) {
    0
  } else {
    sum(findings$Severity == "warning", na.rm = TRUE)
  }

  passed <- n_error == 0
  banner_class <- if (passed) "banner pass" else "banner fail"
  banner_text <- if (passed) {
    sprintf("PASS - no data-type errors (%d warning(s)).", n_warning)
  } else {
    sprintf("FAIL - %d error(s), %d warning(s).", n_error, n_warning)
  }

  meta_lines <- c(
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
  meta_html <- paste0(
    "<div class=\"meta\">",
    paste0(meta_lines, collapse = "<br/>"),
    "</div>"
  )

  footer <- paste0(
    "<div class=\"footer\">This report checks storage class and maximum ",
    "length as declared in the package metadata. It does not replace full ",
    "CDISC conformance validation (controlled terminology and structural ",
    "rules are out of scope).</div>"
  )

  paste0(
    "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"/>",
    "<title>CDISC Data-Type Validation Report</title>",
    "<style>", .CDISC_REPORT_CSS, "</style></head><body>",
    "<h1>CDISC Data-Type Validation Report</h1>",
    meta_html,
    "<div class=\"", banner_class, "\">", banner_text, "</div>",
    .cdisc_report_body(findings),
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
#'   `report_path` (path to the written HTML report), and `blocks_save`
#'   (logical from [cdisc_validation_blocks_save()]).
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

  datasets_checked <- intersect(names(cdisc_data), names(CDISC_KEY_DS_MAP))

  html <- cdisc_validation_report(
    findings,
    datasets_checked = datasets_checked,
    project = project
  )

  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  }
  report_path <- file.path(target_dir, filename)
  writeLines(html, report_path)

  list(
    findings = findings,
    report_path = report_path,
    blocks_save = cdisc_validation_blocks_save(findings)
  )
}
