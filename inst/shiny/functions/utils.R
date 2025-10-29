#' Sets up the logger package for the application.
#'
#' @details
#' The application logs everything to a log file located in `/log` directory. If such folder
#' does not exist, it will be created. Logfile for each session will be separate. The application
#' will keep 5 log files at any given time - if this number is exceeded, the oldest log file
#' will be deleted.
#'
#' In addition, information of the level specified by the user will be logged to console.
#' As a default, this level is INFO - this is so that the user has good information on what is
#' happening inside the app, but is not overwhelmed with tracing and debugging information. This
#' level can be changed using `aNCA_LOG_LEVEL` environmental variable, set for example in
#' `.Renviron` file.
setup_logger <- function() {
  log_layout(layout_glue_colors)
  log_formatter(formatter_glue)
  log_threshold(TRACE)
  log_threshold(Sys.getenv("aNCA_LOG_LEVEL", "INFO"), index = 2)

  log_dir <- "./log"
  if (!dir.exists(log_dir)) dir.create(log_dir)
  existing_logs <- list.files(log_dir, full.names = TRUE)
  if (length(existing_logs) >= 5) file.remove(sort(existing_logs)[1]) # keep only five log files
  logfile_name <- paste0(log_dir, "/aNCA_app_", format(Sys.time(), "%y%m%d-%H%M%S-"), ".log")

  log_appender(appender_file(logfile_name))
  log_appender(appender_console, index = 2)
}

#' Logs a list and data frame objects.
#'
#' @details
#' Utilitary function for logging a list object (like mapping list or used settings) in
#' a nice format. Parses a list into nice string and logs at DEBUG level. Can also process
#' data frames, which will be converted into a list of rows.
#'
#' @param title Title for the logs.
#' @param l     List object to be parsed into log. Can also be a data.frame.
log_debug_list <- function(title, l) {
  log_debug(aNCA:::.concatenate_list(title, l))
}

#' Needed to properly reset reactable.extras widgets
#'
#' @details
#' When the data underlying the `reactable` instance is changed (e.g. rows are deleted), and
#' the reactable is re-rendered, the `reactable.extras` widgets still hold their values. This
#' leads to incorrect display, e.g. after removing a middle row, now widgets for removed row 2
#' will be displayed over what used to be row 3. After significanlty changing the data under
#' `reactable`, the memory of `reactable.extras` needs to be reset.
reset_reactable_memory <- function() {
  shinyjs::runjs("memory = {};")
}
