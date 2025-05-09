#' Reads PK datasets from various file formats.
#'
#' @details
#' Currently supported file formats include:
#' - csv
#' - rds
#'
#' @param path Character string with path to the dataset file.
#' @returns A data.frame object with loaded data.
#'
#' @examples
#' df <- read_pk(system.file("shiny/data/Dummy_complex_data.csv", package = "aNCA"))
#' @importFrom tools file_ext
read_pk <- function(path) {
  if (!file.exists(path)) stop("File does not exist: ", path)

  switch(
    file_ext(path),
    csv = read.csv(path, na = c("", "NA")),
    rds = readRDS(path),
    stop("Invalid file type. Only accepted are .csv and .rds")
  )
}