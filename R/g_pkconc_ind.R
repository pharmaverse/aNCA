#' TODO: Implement actual pkconc plot
#' @export
g_pkconc_ind <- function(data, scale = "lin", xlab = "test x lab", ylab = "test y lab") {
  p <- ggplot2::ggplot(
    data = data,
    mapping = aes(x = TIME, y = ADOSEDUR)
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = xlab,
      y = ylab
    )

  if (scale == "log") {
    p <- p +
      ggplot2::scale_y_log10()
  }

  p
}

#' @export
g_pkconc_ind_lin <- function(data, ...) {
  g_pkconc_ind(data = data, scale = "lin", ...)
}

#' @export
g_pkconc_ind_log <- function(data, ...) {
  g_pkconc_ind(data, scale = "log", ...)
}