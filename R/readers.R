#' Reads PK datasets from various file formats.
#'
#' @details
#' Currently supported file formats include:
#' \itemize{`r paste0("\\item \\code{", names(readers), "}", collapse = "\n")`}
#'
#' @param path Character string with path to the dataset file.
#' @returns A data.frame object with loaded data.
#'
#' @examples
#' df <- read_pk(system.file("shiny/data/example-ADNCA.csv", package = "aNCA"))
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
    if (!requireNamespace("openxlsx2", silently = TRUE))
      stop(
        "Handling .xlsx files requires `openxlsx2` package, please install it with ",
        "`install.packages('openxlsx2')`"
      )
    openxlsx2::read_xlsx(path)
  },
  sas7bdat = function(path) {
    if (!requireNamespace("haven", silently = TRUE))
      stop(
        "Handling .sas7bdat files requires `haven` package, please install it with ",
        "`install.packages('haven')`"
      )
    haven::read_sas(path)
  },
  xpt = function(path) {
    if (!requireNamespace("haven", silently = TRUE))
      stop(
        "Handling .xpt files requires `haven` package, please install it with ",
        "`install.packages('haven')`"
      )
    haven::read_xpt(path)
  },
  parquet = function(path) {
    if (!requireNamespace("arrow", silently = TRUE))
      stop(
        "Handling .parquet files requires `arrow` package, please install it with ",
        "`install.packages('arrow')`"
      )
    arrow::read_parquet(path)
  }
)

#' Helper Logic to parse and structure settings YAML
#' @param path Character string with path to the settings YAML file.
#' @param name Character string with the name of the settings YAML file.
#' @returns A list with parsed settings or NULL if not a valid settings file.
#'
#' @importFrom tools file_ext
#' @importFrom yaml read_yaml
#'
#' @export
read_settings <- function(path, name) {

  obj <- yaml::read_yaml(path)

  if (!is.list(obj) || !"settings" %in% names(obj)) {
    stop("The file does not appear to be a valid settings YAML file.",
         "Please ensure that the file is a list with element 'settings'.")
  }

  if (!is.null(obj$slope_rules) && is.list(obj$slope_rules)) {
    obj$slope_rules$manual_slopes <- bind_rows(obj$slope_rules$manual_slopes)
    obj$slope_rules$profiles_per_subject <- bind_rows(obj$slope_rules$profiles_per_subject)
  }

  if (!is.null(obj$settings) && is.list(obj$settings)) {
    obj$settings$int_parameters <- bind_rows(obj$settings$int_parameters)
    obj$settings$units <- bind_rows(obj$settings$units)
    obj$settings$parameters$types_df <- bind_rows(obj$settings$parameters$types_df)
  }

  obj
}
