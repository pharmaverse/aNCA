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

#' Read uploaded file
#' This function reads the uploaded input and attempts to parse
#'  it as either PK data or settings YAML.
#'
#' @param path Character string with path to the uploaded file.
#' @param name Character string with the name of the uploaded file.
#' @returns A list with status, content, name, and type of the uploaded file.
#' 
#' @examples
#' # Example of reading a PK data file
#' data_file <- system.file("shiny/data/example-ADNCA.csv", package = "aNCA")
#' result_data <- read_uploaded_file(data_file, "example-ADNCA.csv")
#' 
#' @importFrom tools file_ext
#' @importFrom yaml read_yaml
#' @importFrom dplyr bind_rows
#' 
#' @export
read_uploaded_file <- function(path, name) {
  tryCatch({
    # Attempt 1: Read as Data
    data <- read_pk(path)
    list(status = "success", data = data, name = name, type = "data")
  }, error = function(e_pk) {
    # Attempt 2: Read as Settings
    tryCatch({
      settings <- .parse_settings_yaml(path, name)
      if (!is.null(settings)) {
        list(status = "success", content = settings, name = name, type = "settings")
      } else {
        stop(e_pk$message) # Throw original PK error if not settings
      }
    }, error = function(e_yaml) {
      list(status = "error", msg = e_pk$message, name = name)
    })
  })
}

#' Helper Logic to parse and structure settings YAML
#' @param path Character string with path to the settings YAML file.
#' @param name Character string with the name of the settings YAML file.
#' @returns A list with parsed settings or NULL if not a valid settings file.
#' @keywords internal
#' @noRd
.parse_settings_yaml <- function(path, name) {
  # Check extension
  is_yaml <- tolower(tools::file_ext(name)) %in% c("yaml", "yml")
  
  if (!is_yaml) return(NULL)
  
  obj <- yaml::read_yaml(path)
  
  if (!is.list(obj) || !"settings" %in% names(obj)) return(NULL)
  
  # if (!is.null(obj$slope_rules)) {
  #   obj$slope_rules$manual_slopes <- .bind_settings_list(obj$slope_rules$manual_slopes)
  #   obj$slope_rules$profiles_per_subject <- .bind_settings_list(obj$slope_rules$manual_slopes)
  # }
  # 
  # if (!is.null(obj$settings)) {
  #   obj$settings$partial_aucs <- .bind_settings_list(obj$settings$partial_aucs)
  #   obj$settings$units <- .bind_settings_list(obj$settings$units)
  # }
  
  obj
}

#' Helper to standardize binding lists within settings
#' @param obj_list A list of settings to bind.
#' @returns A data frame if binding was successful, otherwise returns the original object.
#' @keywords internal
#' @noRd
.bind_settings_list <- function(obj_list) {
  if (!is.null(obj_list) && is.list(obj_list)) dplyr::bind_rows(obj_list) else obj_list
}
