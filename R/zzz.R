.onLoad <- function(libname, pkgname) {
  # setup logger #
  logger::log_layout(logger::layout_glue_colors)
  logger::log_formatter(logger::formatter_glue)
  logger::log_threshold(logger::TRACE)
  logger::log_appender(logger::appender_console, namespace = "global")
}