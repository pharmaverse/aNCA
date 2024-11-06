#' Evaluates range notation. If provided notation is invalid, returns NA. In example,
#' eval_range("1:5") returns c(1,2,3,4,5).
#' TODO: provide proper documentation.
#'
#' @param x character string with range notation, eg. 1:5.
#' @returns numeric vector with specified range of numbers, NA if notation is invalid
#' @export
.eval_range <- function(x) {
  val_range <- try(eval(parse(text = paste0("c(", x, ")"))), silent = TRUE)
  if (inherits(val_range, "try-error")) NA else val_range
}

#' Compresses a numeric vector into the simples possible character string that, when evaluated,
#' will create the same numeric vector.
#' TODO: providep proper documentation
#'
#' @param range_vector numeric vector with numbers to compress into string
#' @returns simplest possible character string representing provided vector
#' @export
.compress_range <- function(range_vector) {
  if (class(range_vector) != "numeric") range_vector <- as.numeric(range_vector)
  if (any(is.na(range_vector))) stop("Error: only numeric values allowed")

  range_vector <- sort(unique(range_vector))
  grouped_range <- c(TRUE, diff(range_vector) != 1) %>%
    cumsum() %>%
    split(range_vector, .)

  subranges <- sapply(grouped_range, \(group) {
    if (length(group) > 1) paste0(group[1], ":", group[length(group)]) else as.character(group)
  })

  paste0(subranges, collapse = ",")
}
