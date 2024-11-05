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