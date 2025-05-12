#' Reads PK datasets from various file formats.
#'
#' @details
#' Currently supported file formats include:
#' - `csv`
#' - `rds`
#'
#' @param path Character string with path to the dataset file.
#' @returns A data.frame object with loaded data.
#'
#' @examples
#' df <- read_pk(system.file("shiny/data/Dummy_complex_data.csv", package = "aNCA"))
#'
#' @importFrom tools file_ext
#' @importFrom utils read.csv
#' @export
read_pk <- function(path) {
  if (!file.exists(path)) stop("File does not exist: ", path)

  switch(
    file_ext(path),
    csv = read.csv(path, na = c("", "NA")),
    rds = readRDS(path),
    xlsx = {
      if (!requireNamespace("openxlsx2", silently = TRUE))
        stop(
          "Handling .xlsx files requires `openxlsx2` package, please install it with ",
          "`install.packages('openxlsx2')`"
        )
      openxlsx2::read_xlsx(path)
    },
    sas7bdat = {
      if (!requireNamespace("haven", silently = TRUE))
        stop(
          "Handling .sas7bdat files requires `haven` package, please install it with ",
          "`install.packages('haven')`"
        )
      haven::read_sas(path)
    },
    xpt = {
      if (!requireNamespace("haven", silently = TRUE))
        stop(
          "Handling .xpt files requires `haven` package, please install it with ",
          "`install.packages('haven')`"
        )
      haven::read_xpt(path)
    },
    stop("Invalid file type. Accepted formats are .csv, .rds, .xlsx.")
  ) |>
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