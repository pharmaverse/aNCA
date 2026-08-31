# Map lowercase CDISC data keys to the Dataset names used in metadata
CDISC_KEY_DS_MAP <- c(pp = "PP", adpp = "ADPP", adnca = "ADNCA")

# Columns returned by validate_cdisc_types(), in order
CDISC_FINDING_COLS <- c(
  "Dataset", "Variable", "Check", "Severity",
  "Expected", "Observed", "N_Affected", "Message"
)

#' Map a metadata `Type` to an expected R storage class
#'
#' Translates the free-text `Type` values used in `metadata_nca_variables`
#' into the storage class that a conforming column should have. Types that
#' cannot be interpreted return `NA` so callers can skip them.
#'
#' @param type Character scalar with a metadata `Type` value.
#'
#' @returns One of `"character"`, `"numeric"`, or `NA_character_`.
#' @keywords internal
#' @noRd
.cdisc_expected_class <- function(type) {
  if (length(type) == 0 || is.na(type) || !nzchar(trimws(type))) {
    return(NA_character_)
  }
  switch(
    trimws(type),
    text = "character",
    Char = "character",
    Character = "character",
    dateTime = "character",
    duration = "character",
    float = "numeric",
    integer = "numeric",
    Num = "numeric",
    NA_character_
  )
}

# Does the observed column satisfy the expected class?
.cdisc_class_ok <- function(col, expected) {
  if (all(is.na(col))) return(TRUE)
  if (expected == "numeric") return(is.numeric(col))
  # character: accept character or factor
  is.character(col) || is.factor(col)
}

# Human-readable storage class of an observed column
.cdisc_observed_class <- function(col) {
  if (is.numeric(col)) return("numeric")
  if (is.factor(col)) return("factor")
  if (is.character(col)) return("character")
  if (is.logical(col) && all(is.na(col))) return("logical (all NA)")
  class(col)[1]
}

# Normalise a variable name to an index-agnostic key so that indexed CDISC
# variable families collapse to a single template. CDISC variable names are
# always uppercase, so any lowercase letter in a metadata name is an index
# placeholder (e.g. `NCAwXRS`, `STRATwD`); digit runs are index instances
# (e.g. `CRIT1`, `NCA2XRSN`). Both are replaced with a single `#` token.
.cdisc_normalise_name <- function(x, is_metadata = FALSE) {
  if (is_metadata) x <- gsub("[a-z]+", "#", x)
  gsub("[0-9]+", "#", x)
}

# Find the metadata spec row for a data column. Falls back from an exact name
# match to an index-agnostic template match (see .cdisc_normalise_name).
# Returns a one-row data frame, or NULL when nothing matches.
.match_cdisc_spec <- function(var, ds_meta) {
  if (nrow(ds_meta) == 0) return(NULL)

  exact <- ds_meta[ds_meta$Variable == var, , drop = FALSE]
  if (nrow(exact) > 0) return(exact[1, , drop = FALSE])

  var_key <- .cdisc_normalise_name(var)
  meta_keys <- .cdisc_normalise_name(ds_meta$Variable, is_metadata = TRUE)
  hit <- which(meta_keys == var_key)
  if (length(hit) == 0) return(NULL)
  ds_meta[hit[1], , drop = FALSE]
}

# Build a single findings row as a one-row data frame
.cdisc_finding_row <- function(dataset, variable, check, severity,
                               expected, observed, n_affected, message) {
  data.frame(
    Dataset = dataset,
    Variable = variable,
    Check = check,
    Severity = severity,
    Expected = expected,
    Observed = observed,
    N_Affected = n_affected,
    Message = message,
    stringsAsFactors = FALSE
  )
}

# Empty findings data frame with the canonical columns
.cdisc_empty_findings <- function() {
  out <- data.frame(
    Dataset = character(0),
    Variable = character(0),
    Check = character(0),
    Severity = character(0),
    Expected = character(0),
    Observed = character(0),
    N_Affected = integer(0),
    Message = character(0),
    stringsAsFactors = FALSE
  )
  out[CDISC_FINDING_COLS]
}

# Is a declared length usable for a length check?
.cdisc_has_length <- function(len) {
  !is.na(len) && is.numeric(len) && len > 0
}

# Unknown-variable finding for a column absent from the metadata
.cdisc_finding_unknown <- function(ds_name, var, n) {
  .cdisc_finding_row(
    ds_name, var, "unknown_variable", "warning",
    "declared in metadata", "not in metadata", n,
    sprintf("Column '%s' is not declared in the metadata for %s.", var, ds_name)
  )
}

# Class-mismatch finding for a column whose storage class is wrong
.cdisc_finding_class <- function(ds_name, var, col, expected_class) {
  .cdisc_finding_row(
    ds_name, var, "class", "error",
    expected_class, .cdisc_observed_class(col), sum(!is.na(col)),
    sprintf(
      "Column '%s' should be %s but is %s.",
      var, expected_class, .cdisc_observed_class(col)
    )
  )
}

# Length-violation finding, or NULL when the column is within the declared length
.cdisc_finding_length <- function(ds_name, var, col, max_len) {
  char_col <- as.character(col)
  too_long <- !is.na(char_col) & nchar(char_col) > max_len
  if (!any(too_long)) return(NULL)
  .cdisc_finding_row(
    ds_name, var, "length", "error",
    paste0("<= ", max_len), as.character(max(nchar(char_col[too_long]))),
    sum(too_long),
    sprintf(
      "Column '%s' has %d value(s) longer than the declared length %d.",
      var, sum(too_long), max_len
    )
  )
}

# Validate a single column, returning a findings row or NULL when it conforms
.validate_one_cdisc_column <- function(df, var, ds_name, ds_meta, check_length) {
  col <- df[[var]]
  specs <- .match_cdisc_spec(var, ds_meta)

  # Unknown variable: absent from metadata (including its indexed-variable
  # template, e.g. NCAwXRS covers NCA1XRS/NCA2XRS)
  if (is.null(specs)) return(.cdisc_finding_unknown(ds_name, var, nrow(df)))

  expected_class <- .cdisc_expected_class(specs$Type)
  if (is.na(expected_class)) return(NULL)

  if (!.cdisc_class_ok(col, expected_class)) {
    return(.cdisc_finding_class(ds_name, var, col, expected_class))
  }

  if (check_length && expected_class == "character" &&
        .cdisc_has_length(specs$Length)) {
    return(.cdisc_finding_length(ds_name, var, col, specs$Length))
  }

  NULL
}

# Validate a single dataset's columns against its metadata specs
.validate_one_cdisc_dataset <- function(df, ds_name, ds_meta, check_length) {
  findings <- lapply(names(df), function(var) {
    .validate_one_cdisc_column(df, var, ds_name, ds_meta, check_length)
  })
  findings <- Filter(Negate(is.null), findings)

  if (length(findings) == 0) return(.cdisc_empty_findings())
  do.call(rbind, findings)
}

# A data frame with at least one column that can be processed?
.cdisc_processable_df <- function(df) {
  !is.null(df) && is.data.frame(df) && ncol(df) > 0
}

# Apply `per_dataset(df, ds_name, ds_meta, check_length)` to each recognised
# CDISC dataset and row-bind the results. `empty` is returned when nothing is
# processed; `cols` fixes the output column order.

.iterate_cdisc_datasets <- function(cdisc_data, metadata, check_length,
                                    per_dataset, empty, cols) {
  if (is.null(cdisc_data) || length(cdisc_data) == 0) return(empty)

  keys <- intersect(names(cdisc_data), names(CDISC_KEY_DS_MAP))
  keys <- Filter(function(k) .cdisc_processable_df(cdisc_data[[k]]), keys)
  if (length(keys) == 0) return(empty)

  results <- lapply(keys, function(key) {
    ds_name <- CDISC_KEY_DS_MAP[[key]]
    ds_meta <- metadata[metadata$Dataset == ds_name, , drop = FALSE]
    per_dataset(cdisc_data[[key]], ds_name, ds_meta, check_length)
  })

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  out[cols]
}

#' Validate CDISC datasets against metadata-declared data types
#'
#' Checks each column of the supplied CDISC datasets against the expected
#' storage class and maximum length declared in `metadata_nca_variables`.
#' This is intentionally not a full CDISC conformance engine: it covers only
#' what the metadata can assert (class and length), and never fails on
#' metadata `Type` values it cannot interpret.
#'
#' Indexed CDISC variable families are resolved to their metadata template: a
#' data column falls back from an exact name match to an index-agnostic match,
#' so `NCA1XRS`/`NCA2XRS` match the `NCAwXRS` template and `CRIT2`/`CRIT3FL`
#' match the `CRIT1`/`CRIT1FL` family. An exact metadata entry always takes
#' precedence over the template.
#'
#' @param cdisc_data Named list of CDISC data frames keyed `pp`, `adpp`,
#'   and/or `adnca` (as returned by [export_cdisc()]). Entries with other
#'   names or that are not data frames are ignored.
#' @param metadata Metadata data frame with `Dataset`, `Variable`, `Type`,
#'   and `Length` columns. Defaults to `metadata_nca_variables`.
#' @param check_length Logical. Check character values against the declared
#'   `Length`? Defaults to `TRUE`.
#'
#' @returns A tidy findings data frame with columns `Dataset`, `Variable`,
#'   `Check`, `Severity`, `Expected`, `Observed`, `N_Affected`, and
#'   `Message`. Zero rows means the data conforms.
#' @export
#'
#' @examples
#' cdisc_data <- list(
#'   adnca = data.frame(STUDYID = "S1", AVAL = 1.2, stringsAsFactors = FALSE)
#' )
#' validate_cdisc_types(cdisc_data)
validate_cdisc_types <- function(cdisc_data,
                                 metadata = metadata_nca_variables,
                                 check_length = TRUE) {
  .iterate_cdisc_datasets(
    cdisc_data, metadata, check_length,
    per_dataset = .validate_one_cdisc_dataset,
    empty = .cdisc_empty_findings(),
    cols = CDISC_FINDING_COLS
  )
}

#' Does a set of validation findings block saving?
#'
#' Predicate used by the export/Save flow to refuse a save when any
#' `error`-severity finding is present. `warning`-severity findings (for
#' example unknown columns) do not block saving.
#'
#' @param findings A findings data frame as returned by
#'   [validate_cdisc_types()].
#'
#' @returns `TRUE` when at least one `error`-severity finding exists,
#'   otherwise `FALSE`.
#' @export
#'
#' @examples
#' findings <- validate_cdisc_types(list())
#' cdisc_validation_blocks_save(findings)
cdisc_validation_blocks_save <- function(findings) {
  if (is.null(findings) || !is.data.frame(findings) || nrow(findings) == 0) {
    return(FALSE)
  }
  any(findings$Severity == "error", na.rm = TRUE)
}

# Columns returned by describe_cdisc_variables(), in order
CDISC_SUMMARY_COLS <- c(
  "Dataset", "Variable", "Label", "Type", "Expected_Class",
  "Observed_Class", "Max_Length", "Longest_Value", "N_NonMissing",
  "Status", "Detail"
)

# Empty descriptive summary data frame with the canonical columns
.cdisc_empty_summary <- function() {
  out <- data.frame(
    Dataset = character(0),
    Variable = character(0),
    Label = character(0),
    Type = character(0),
    Expected_Class = character(0),
    Observed_Class = character(0),
    Max_Length = character(0),
    Longest_Value = integer(0),
    N_NonMissing = integer(0),
    Status = character(0),
    Detail = character(0),
    stringsAsFactors = FALSE
  )
  out[CDISC_SUMMARY_COLS]
}

# Longest character width in a column (0 when all missing)
.cdisc_longest <- function(col) {
  char_col <- as.character(col)
  if (all(is.na(char_col))) return(0L)
  as.integer(max(nchar(char_col), na.rm = TRUE))
}

# Build a one-row summary for a column that has no matching metadata spec
.describe_unknown_column <- function(ds_name, var, col) {
  data.frame(
    Dataset = ds_name, Variable = var, Label = NA_character_,
    Type = NA_character_, Expected_Class = NA_character_,
    Observed_Class = .cdisc_observed_class(col), Max_Length = NA_character_,
    Longest_Value = .cdisc_longest(col), N_NonMissing = as.integer(sum(!is.na(col))),
    Status = "unknown", Detail = "Not declared in metadata.",
    stringsAsFactors = FALSE
  )
}

# Does the column violate the declared character length?
.cdisc_over_length <- function(expected_class, specs, longest, check_length) {
  isTRUE(check_length) && identical(expected_class, "character") &&
    .cdisc_has_length(specs$Length) && longest > specs$Length
}

# Determine (status, detail) for a column that has a matching metadata spec
.describe_status <- function(col, specs, expected_class, longest, check_length) {
  if (is.na(expected_class)) {
    return(list(
      status = "skipped",
      detail = sprintf("Type '%s' not interpretable; not checked.", specs$Type)
    ))
  }
  if (!.cdisc_class_ok(col, expected_class)) {
    return(list(
      status = "error",
      detail = sprintf(
        "Expected %s, found %s.", expected_class, .cdisc_observed_class(col)
      )
    ))
  }
  if (.cdisc_over_length(expected_class, specs, longest, check_length)) {
    return(list(
      status = "error",
      detail = sprintf(
        "Longest value (%d) exceeds declared length %d.", longest, specs$Length
      )
    ))
  }
  if (all(is.na(col))) {
    return(list(status = "pass", detail = "Conforms (all values missing)."))
  }
  list(status = "pass", detail = "Conforms.")
}

# Build a one-row summary for a column with a matching metadata spec
.describe_known_column <- function(ds_name, var, col, specs, check_length) {
  expected_class <- .cdisc_expected_class(specs$Type)
  longest <- .cdisc_longest(col)
  label <- if ("Label" %in% names(specs)) as.character(specs$Label) else NA_character_
  max_len <- if (.cdisc_has_length(specs$Length)) {
    as.character(specs$Length)
  } else {
    NA_character_
  }
  st <- .describe_status(col, specs, expected_class, longest, check_length)

  data.frame(
    Dataset = ds_name, Variable = var, Label = label,
    Type = as.character(specs$Type), Expected_Class = expected_class,
    Observed_Class = .cdisc_observed_class(col), Max_Length = max_len,
    Longest_Value = longest, N_NonMissing = as.integer(sum(!is.na(col))),
    Status = st$status, Detail = st$detail,
    stringsAsFactors = FALSE
  )
}

# Describe a single dataset's columns against its metadata specs
.describe_one_cdisc_dataset <- function(df, ds_name, ds_meta, check_length) {
  rows <- lapply(names(df), function(var) {
    col <- df[[var]]
    specs <- .match_cdisc_spec(var, ds_meta)
    if (is.null(specs)) return(.describe_unknown_column(ds_name, var, col))
    .describe_known_column(ds_name, var, col, specs, check_length)
  })

  if (length(rows) == 0) return(.cdisc_empty_summary())
  do.call(rbind, rows)
}

#' Describe CDISC dataset columns against the declared metadata
#'
#' Produces a per-variable descriptive summary of the supplied CDISC datasets:
#' the declared label, type, expected and observed storage class, declared and
#' observed maximum length, and a conformance status for each column. This is
#' the descriptive counterpart to [validate_cdisc_types()], which only returns
#' problems.
#'
#' @param cdisc_data Named list of CDISC data frames keyed `pp`, `adpp`,
#'   and/or `adnca` (as returned by [export_cdisc()]).
#' @param metadata Metadata data frame with `Dataset`, `Variable`, `Label`,
#'   `Type`, and `Length` columns. Defaults to `metadata_nca_variables`.
#' @param check_length Logical. Consider the declared `Length` when assigning
#'   status? Defaults to `TRUE`.
#'
#' @returns A data frame with one row per column of each dataset and columns
#'   `Dataset`, `Variable`, `Label`, `Type`, `Expected_Class`,
#'   `Observed_Class`, `Max_Length`, `Longest_Value`, `N_NonMissing`,
#'   `Status`, and `Detail`. `Status` is one of `pass`, `error`, `unknown`,
#'   or `skipped`.
#' @export
#'
#' @examples
#' cdisc_data <- list(
#'   adnca = data.frame(STUDYID = "S1", AVAL = 1.2, stringsAsFactors = FALSE)
#' )
#' describe_cdisc_variables(cdisc_data)
describe_cdisc_variables <- function(cdisc_data,
                                     metadata = metadata_nca_variables,
                                     check_length = TRUE) {
  .iterate_cdisc_datasets(
    cdisc_data, metadata, check_length,
    per_dataset = .describe_one_cdisc_dataset,
    empty = .cdisc_empty_summary(),
    cols = CDISC_SUMMARY_COLS
  )
}
