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

# Validate a single dataset's columns against its metadata specs
.validate_one_cdisc_dataset <- function(df, ds_name, ds_meta, check_length) {
  findings <- list()

  for (var in names(df)) {
    col <- df[[var]]
    specs <- ds_meta[ds_meta$Variable == var, , drop = FALSE]

    # Unknown variable: present in data but absent from metadata
    if (nrow(specs) == 0) {
      findings[[length(findings) + 1]] <- .cdisc_finding_row(
        ds_name, var, "unknown_variable", "warning",
        "declared in metadata", "not in metadata", nrow(df),
        sprintf(
          "Column '%s' is not declared in the metadata for %s.",
          var, ds_name
        )
      )
      next
    }

    specs <- specs[1, , drop = FALSE]
    expected_class <- .cdisc_expected_class(specs$Type)

    # Skip variables whose type cannot be interpreted
    if (is.na(expected_class)) next

    # Class check
    if (!.cdisc_class_ok(col, expected_class)) {
      findings[[length(findings) + 1]] <- .cdisc_finding_row(
        ds_name, var, "class", "error",
        expected_class, .cdisc_observed_class(col), sum(!is.na(col)),
        sprintf(
          "Column '%s' should be %s but is %s.",
          var, expected_class, .cdisc_observed_class(col)
        )
      )
      # A wrong-class column cannot be meaningfully length-checked
      next
    }

    # Length check (character values only)
    if (check_length && expected_class == "character" &&
          !is.na(specs$Length) && is.numeric(specs$Length) &&
          specs$Length > 0) {
      char_col <- as.character(col)
      too_long <- !is.na(char_col) & nchar(char_col) > specs$Length
      if (any(too_long)) {
        max_obs <- max(nchar(char_col[too_long]))
        findings[[length(findings) + 1]] <- .cdisc_finding_row(
          ds_name, var, "length", "error",
          paste0("<= ", specs$Length), as.character(max_obs), sum(too_long),
          sprintf(
            "Column '%s' has %d value(s) longer than the declared length %d.",
            var, sum(too_long), specs$Length
          )
        )
      }
    }
  }

  if (length(findings) == 0) return(.cdisc_empty_findings())
  do.call(rbind, findings)
}

#' Validate CDISC datasets against metadata-declared data types
#'
#' Checks each column of the supplied CDISC datasets against the expected
#' storage class and maximum length declared in `metadata_nca_variables`.
#' This is intentionally not a full CDISC conformance engine: it covers only
#' what the metadata can assert (class and length), and never fails on
#' metadata `Type` values it cannot interpret.
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
  if (is.null(cdisc_data) || length(cdisc_data) == 0) {
    return(.cdisc_empty_findings())
  }

  keys <- intersect(names(cdisc_data), names(CDISC_KEY_DS_MAP))
  results <- list()

  for (key in keys) {
    df <- cdisc_data[[key]]
    if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) next

    ds_name <- CDISC_KEY_DS_MAP[[key]]
    ds_meta <- metadata[metadata$Dataset == ds_name, , drop = FALSE]

    results[[key]] <- .validate_one_cdisc_dataset(
      df, ds_name, ds_meta, check_length
    )
  }

  if (length(results) == 0) return(.cdisc_empty_findings())

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  out[CDISC_FINDING_COLS]
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
