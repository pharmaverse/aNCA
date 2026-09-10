# Columns returned by validate_export_outputs(), in order
EXPORT_FINDING_COLS <- c(
  "Output", "Variable", "Check", "Severity", "Expected", "Observed", "Message"
)

# Known export nodes and the object kind each is expected to hold. Plot nodes
# must be a ggplot or plotly; table nodes must be a data frame. Nodes not listed
# here (e.g. user-named custom exploration snapshots) fall back to the generic
# "must be a saveable object" check.
EXPORT_PLOT_NODES <- c("individualplot", "meanplot", "qcplot", "boxplot")
EXPORT_TABLE_NODES <- c(
  "nca_pkparam", "nca_statistics", "pp", "adpp", "adnca",
  "matrix_ratios", "excretion_results"
)

# Is an object a saveable export leaf? Mirrors the leaf rule used by the export
# writer: a ggplot, data frame, plotly widget, or a length-1 code string (a
# character scalar under a `*_code` node).
.export_saveable <- function(x, name) {
  inherits(x, "ggplot") || is.data.frame(x) || inherits(x, "plotly") ||
    (is.character(x) && length(x) == 1L && grepl("_code$", name))
}

# Expected object kind for a known export node, or NA for unrecognised nodes
.export_expected_kind <- function(name) {
  if (grepl("_code$", name)) return("code")
  if (name %in% EXPORT_PLOT_NODES) return("plot")
  if (name %in% EXPORT_TABLE_NODES) return("table")
  NA_character_
}

# Observed object kind of an export leaf: "plot", "table", "code", or
# "unsupported" for anything the export writer cannot serialise
.export_observed_kind <- function(x, name) {
  if (is.character(x) && length(x) == 1L && grepl("_code$", name)) return("code")
  if (inherits(x, "ggplot") || inherits(x, "plotly")) return("plot")
  if (is.data.frame(x)) return("table")
  "unsupported"
}

# Build a single export-findings row as a one-row data frame
.export_finding_row <- function(output, variable, check, severity,
                                expected, observed, message) {
  data.frame(
    Output = output,
    Variable = variable,
    Check = check,
    Severity = severity,
    Expected = expected,
    Observed = observed,
    Message = message,
    stringsAsFactors = FALSE
  )
}

# Empty export-findings data frame with the canonical columns
.export_empty_findings <- function() {
  out <- data.frame(
    Output = character(0),
    Variable = character(0),
    Check = character(0),
    Severity = character(0),
    Expected = character(0),
    Observed = character(0),
    Message = character(0),
    stringsAsFactors = FALSE
  )
  out[EXPORT_FINDING_COLS]
}

# Human-readable class of an object for messages
.export_class_label <- function(x) {
  if (is.null(x)) return("NULL")
  paste(class(x), collapse = ", ")
}

# Validate a single export leaf, returning a findings row or NULL when it is a
# saveable object of the kind its node expects
.validate_one_export_leaf <- function(x, name, path) {
  observed <- .export_observed_kind(x, name)
  expected <- .export_expected_kind(name)

  if (observed == "unsupported") {
    exp_label <- if (is.na(expected)) "a saveable output" else expected
    return(.export_finding_row(
      path, NA_character_, "class", "error", exp_label,
      .export_class_label(x),
      sprintf(
        "Output '%s' is %s, which cannot be exported (expected %s).",
        path, .export_class_label(x), exp_label
      )
    ))
  }

  if (!is.na(expected) && observed != expected) {
    return(.export_finding_row(
      path, NA_character_, "class", "error", expected, observed,
      sprintf(
        "Output '%s' should be a %s but is a %s.", path, expected, observed
      )
    ))
  }

  NULL
}

# Should a leaf be validated given the writer's name filter? Mirrors the export
# writer: with no filter everything is written; otherwise only named leaves are.
.export_leaf_selected <- function(name, obj_names) {
  is.null(obj_names) || name %in% obj_names
}

# Is a name a known, declared output node (plot, table, or code)? Such nodes
# are always validated as leaves and never recursed into, so an object that is
# incidentally a list (e.g. an `lm` fit placed under a table node) is caught
# rather than walked.
.export_known_node <- function(name) {
  !is.na(.export_expected_kind(name))
}

# Walk the (possibly nested) export list, collecting object-class findings for
# every selected leaf. Recurses into container lists as the writer does, but
# treats declared output nodes as leaves so non-saveable objects are caught.
.walk_export_outputs <- function(output, obj_names, path) {
  findings <- list()
  for (name in names(output)) {
    x <- output[[name]]
    child_path <- if (nzchar(path)) paste0(path, "/", name) else name

    is_leaf <- .export_known_node(name) || .export_saveable(x, name) ||
      !is.list(x)

    if (!is_leaf) {
      findings <- c(findings, .walk_export_outputs(x, obj_names, child_path))
    } else if (.export_leaf_selected(name, obj_names)) {
      f <- .validate_one_export_leaf(x, name, child_path)
      if (!is.null(f)) findings <- c(findings, list(f))
    }
  }
  findings
}

# Map CDISC value-level findings from validate_cdisc_types() onto the export
# findings schema (Dataset -> Output, drop N_Affected)
.export_map_cdisc_findings <- function(cdisc_findings) {
  if (is.null(cdisc_findings) || nrow(cdisc_findings) == 0) {
    return(.export_empty_findings())
  }
  .export_finding_row(
    output = paste0("CDISC/", tolower(cdisc_findings$Dataset)),
    variable = cdisc_findings$Variable,
    check = cdisc_findings$Check,
    severity = cdisc_findings$Severity,
    expected = cdisc_findings$Expected,
    observed = cdisc_findings$Observed,
    message = cdisc_findings$Message
  )
}

# Value-level CDISC findings for the selected CDISC datasets, or an empty
# findings frame when none are present/selected
.export_cdisc_findings <- function(output, obj_names, metadata) {
  cdisc <- output[["CDISC"]]
  if (is.null(cdisc) || !is.list(cdisc) || length(cdisc) == 0) {
    return(.export_empty_findings())
  }
  if (!is.null(obj_names)) {
    cdisc <- cdisc[intersect(names(cdisc), obj_names)]
  }
  if (length(cdisc) == 0) return(.export_empty_findings())
  .export_map_cdisc_findings(validate_cdisc_types(cdisc, metadata = metadata))
}

#' Validate exported outputs before saving
#'
#' Checks every output that the export/Save flow would write, so a save can be
#' refused before any file is created. It combines two levels of checking:
#'
#' * **Object class** for all outputs: plot nodes must be a ggplot or plotly,
#'   table nodes must be a data frame, and `*_code` nodes must be a length-1
#'   character string. Anything the export writer cannot serialise is reported
#'   as an error.
#' * **Value level** for the CDISC datasets: the `pp`/`adpp`/`adnca` entries
#'   under `CDISC` are additionally checked against the metadata-declared data
#'   types and lengths via [validate_cdisc_types()].
#'
#' The walk mirrors the export writer: plain lists are recursed into, and when
#' `obj_names` is supplied only those named leaves (the ones that will actually
#' be written) are validated.
#'
#' @param output The export list as assembled for the Save flow: a named,
#'   possibly nested list of outputs (plots, tables, and `*_code` strings),
#'   with the CDISC datasets under a `CDISC` element keyed `pp`/`adpp`/`adnca`.
#' @param obj_names Optional character vector of leaf names that will be
#'   written. When `NULL` (the default) every leaf is validated.
#' @param metadata Metadata data frame passed to [validate_cdisc_types()] for
#'   the CDISC value-level checks. Defaults to `metadata_nca_variables`.
#'
#' @returns A tidy findings data frame with columns `Output`, `Variable`,
#'   `Check`, `Severity`, `Expected`, `Observed`, and `Message`. `Variable` is
#'   `NA` for object-class findings and holds the column name for CDISC
#'   value-level findings. Zero rows means every output is safe to export.
#' @export
#'
#' @examples
#' output <- list(
#'   nca_results = list(nca_pkparam = data.frame(AVAL = 1.2)),
#'   CDISC = list(adnca = data.frame(STUDYID = "S1", AVAL = 1.2))
#' )
#' validate_export_outputs(output)
validate_export_outputs <- function(output,
                                    obj_names = NULL,
                                    metadata = metadata_nca_variables) {
  if (is.null(output) || !is.list(output) || length(output) == 0) {
    return(.export_empty_findings())
  }

  class_findings <- .walk_export_outputs(output, obj_names, path = "")
  cdisc_findings <- .export_cdisc_findings(output, obj_names, metadata)

  parts <- c(class_findings, list(cdisc_findings))
  parts <- Filter(function(df) !is.null(df) && nrow(df) > 0, parts)
  if (length(parts) == 0) return(.export_empty_findings())

  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out[EXPORT_FINDING_COLS]
}

#' Do export findings block saving?
#'
#' Predicate used by the export/Save flow to refuse a save when any
#' `error`-severity finding is present in the output of
#' [validate_export_outputs()].
#'
#' @param findings A findings data frame as returned by
#'   [validate_export_outputs()].
#'
#' @returns `TRUE` when at least one `error`-severity finding exists,
#'   otherwise `FALSE`.
#' @export
#'
#' @examples
#' findings <- validate_export_outputs(list())
#' export_validation_blocks_save(findings)
export_validation_blocks_save <- function(findings) {
  if (is.null(findings) || !is.data.frame(findings) || nrow(findings) == 0) {
    return(FALSE)
  }
  any(findings$Severity == "error", na.rm = TRUE)
}
