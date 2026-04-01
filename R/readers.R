#' Reads datasets from various file formats.
#'
#' @details
#' Currently supported file formats include:
#' \itemize{`r paste0("\\item \\code{", names(readers), "}", collapse = "\n")`}
#'
#' @param path Character string with path to the dataset file.
#' @returns A data.frame object with loaded data.
#'
#' @examples
#' path <- system.file("shiny/tests/testthat/dummy_simplified.csv", package = "aNCA")
#' df <- read_pk(path)
#'
#' @importFrom tools file_ext
#' @importFrom utils read.csv
#' @export
read_pk <- function(path) {
  if (!file.exists(path)) stop("File does not exist: ", path)

  format <- tools::file_ext(path)
  if (!format %in% names(readers))
    stop("Invalid file type. Accepted formats are ", paste(names(readers), collapse = ", "))

  readers[[format]](path) %>%
    validate_pk()
}

#' Validates data table with raw pk data.
#'
#' @details
#' Performs the following checks:
#'   - If provided variable is of class `data.frame`.
#'   - If number of rows in provided data frame is greater than 0.
#'
#' Throws an error if any of the checks does not pass.
#'
#' @param pk_data Object to check.
#'
#' @returns Original, unchanged object. If any of the checks do not pass, throws an error.
validate_pk <- function(pk_data) {
  if (!inherits(pk_data, "data.frame"))
    stop("Invalid data format. Data frame was expected, but received ", class(pk_data), ".")

  if (NROW(pk_data) == 0)
    stop("Empty data frame received, please check the input file.")

  pk_data
}

#' List of readers responsible for loading files with specific file extensions.
#' @noRd
#' @keywords internal
readers <- list(
  csv = function(path) read.csv(path, na = c("", "NA")),
  rds = function(path) readRDS(path),
  xlsx = function(path) {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop(
        "Handling .xlsx files requires `readxl` package, please install it with ",
        "`install.packages('readxl')`"
      )
    as.data.frame(readxl::read_excel(path))
  },
  sas7bdat = function(path) {
    if (!requireNamespace("haven", quietly = TRUE))
      stop(
        "Handling .sas7bdat files requires `haven` package, please install it with ",
        "`install.packages('haven')`"
      )
    haven::read_sas(path)
  },
  xpt = function(path) {
    if (!requireNamespace("haven", quietly = TRUE))
      stop(
        "Handling .xpt files requires `haven` package, please install it with ",
        "`install.packages('haven')`"
      )
    haven::read_xpt(path)
  },
  parquet = function(path) {
    if (!requireNamespace("arrow", quietly = TRUE))
      stop(
        "Handling .parquet files requires `arrow` package, please install it with ",
        "`install.packages('arrow')`"
      )
    arrow::read_parquet(path)
  }
)

#' Helper Logic to parse and structure settings YAML
#'
#' Supports both the legacy flat format (top-level `settings` key) and
#' the versioned format (top-level `current` key). For versioned files
#' the most recent version is returned by default. Use `version` to
#' select a specific version by index or comment. The full versioned
#' object is attached as attribute `"versioned"` so callers can offer
#' version selection.
#'
#' @param path Character string with path to the settings YAML file.
#' @param version Optional version selector for versioned settings files.
#'   Either an integer index (1 = most recent, 2 = second, etc.) or a
#'   character string matched against the version `comment` field.
#'   Ignored for non-versioned (legacy) settings files.
#' @returns A list with parsed settings. For versioned files, the
#'   attribute `"versioned"` contains the full
#'   [read_versioned_settings()] result.
#'
#' @importFrom yaml read_yaml
#'
#' @export
read_settings <- function(path, version = NULL) {
  obj <- yaml::read_yaml(path)
  versioned_attr <- NULL

  if ("current" %in% names(obj) && is.list(obj$current) &&
        "datetime" %in% names(obj$current)) {
    versioned_attr <- read_versioned_settings(obj = obj)
    chosen <- .select_version(versioned_attr$versions, version)
    obj <- chosen[setdiff(names(chosen), VERSION_META_KEYS)]
  }

  # Shared validation — works for both legacy and versioned
  obj <- .process_settings_payload(obj)

  if (!is.null(versioned_attr)) {
    attr(obj, "versioned") <- versioned_attr
  }

  obj
}

#' Validate and post-process a settings payload.
#'
#' Checks that the `settings` key exists, then converts YAML lists
#' to data.frames. Used by both `read_settings()` (legacy and versioned)
#' and the version restore logic in `data_upload`.
#' @param obj A list with settings payload.
#' @returns The processed list with data.frame conversions applied.
#' @keywords internal
#' @noRd
.process_settings_payload <- function(obj) {
  if (!is.list(obj) || !"settings" %in% names(obj)) {
    stop(
      "The file does not appear to be a valid settings YAML file. ",
      "Please ensure that the file is a list with element 'settings'."
    )
  }

  obj$slope_rules <- .convert_list_to_df(obj$slope_rules)
  obj$settings$units <- .convert_list_to_df(obj$settings$units)
  obj$settings$int_parameters <- .convert_list_to_df(obj$settings$int_parameters)
  obj$filters <- .convert_filter_values(obj$filters)
  obj$settings$ratio_table <- .convert_list_to_df(obj$settings$ratio_table)

  obj
}

#' Select a version from a sorted list of version entries.
#'
#' @param versions List of version entries (sorted newest first, as
#'   returned by [read_versioned_settings()]).
#' @param version `NULL` (return first/most recent), an integer index,
#'   or a character string to match against the `comment` field.
#' @returns A single version entry list.
#' @noRd
.select_version <- function(versions, version = NULL) {
  if (is.null(version)) {
    return(versions[[1]])
  }

  if (is.numeric(version)) {
    idx <- as.integer(version)
    if (idx < 1 || idx > length(versions)) {
      stop(
        "settings_version index ", idx, " is out of range. ",
        "The file contains ", length(versions), " version(s)."
      )
    }
    return(versions[[idx]])
  }

  if (is.character(version)) {
    comments <- vapply(
      versions,
      function(v) if (is.null(v$comment)) "" else v$comment,
      character(1)
    )
    match_idx <- which(comments == version)
    if (length(match_idx) == 0) {
      stop(
        "No version with comment \"", version, "\" found. ",
        "Available comments: ",
        paste0("\"", comments[nzchar(comments)], "\"", collapse = ", ")
      )
    }
    if (length(match_idx) > 1) {
      dup_info <- vapply(match_idx, function(i) {
        dt <- versions[[i]]$datetime
        paste0("  index ", i, " (", if (is.null(dt)) "no date" else dt, ")")
      }, character(1))
      warning(
        "Multiple versions with comment \"", version, "\" found. ",
        "Using the most recent (index ", match_idx[1], "). ",
        "Use settings_version = <index> to pick a specific one:\n",
        paste(dup_info, collapse = "\n")
      )
    }
    return(versions[[match_idx[1]]])
  }

  stop("version must be NULL, an integer, or a character string.")
}

#' Convert a YAML list to a data.frame via bind_rows.
#' Returns NULL if input is NULL or not a list.
#' @param x A list or NULL.
#' @returns A data.frame or NULL.
#' @keywords internal
#' @noRd
.convert_list_to_df <- function(x) {
  if (!is.null(x) && is.list(x)) as.data.frame(bind_rows(x)) else x
}

#' Convert filter values from YAML lists to vectors.
#' @param filters A list of filter specifications or NULL.
#' @returns A list with unlist-ed values, or NULL.
#' @keywords internal
#' @noRd
.convert_filter_values <- function(filters) {
  if (is.null(filters) || !is.list(filters)) return(filters)
  lapply(filters, function(filt) {
    filt$value <- unlist(filt$value)
    filt
  })
}
