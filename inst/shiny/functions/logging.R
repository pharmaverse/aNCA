# Lightweight logging system for the aNCA Shiny app.
#
# Replaces the `logger` package with console-only output and in-memory
# log capture for ZIP export. Supports glue-style interpolation and
# paste-style multi-argument calls.
#
# Log levels: TRACE < DEBUG < INFO < SUCCESS < WARN < ERROR
# Default threshold: INFO (configurable via aNCA_LOG_LEVEL env var).
#
# The log captures application-level events only — not raw R console
# output. Warnings and errors from third-party packages appear only
# when explicitly caught by tryCatch blocks with log_warn/log_error.
#
# The in-memory buffer is exported as session_log.txt in the ZIP
# download. For a full reference of logged events, see:
# https://pharmaverse.github.io/aNCA/articles/session_log.html

.log_env <- new.env(parent = emptyenv())
.log_env$threshold <- "INFO"
.log_env$buffer <- character(0)

.LOG_LEVELS <- c(TRACE = 1L, DEBUG = 2L, INFO = 3L, SUCCESS = 4L, WARN = 5L, ERROR = 6L)

#' Initialise the logging system.
#'
#' Reads the threshold from the `aNCA_LOG_LEVEL` environment variable
#' (default `"INFO"`) and clears the in-memory log buffer.
setup_logger <- function() {
  level <- toupper(Sys.getenv("aNCA_LOG_LEVEL", "INFO"))
  if (!level %in% names(.LOG_LEVELS)) level <- "INFO"
  .log_env$threshold <- level
  .log_env$buffer <- character(0)
}

#' Core logging function.
#' @param level Character: one of the log level names.
#' @param ... Message parts. If the first argument contains `{`, it is
#'   evaluated as a glue string in the caller's environment. Otherwise
#'   all arguments are pasted together.
#' @noRd
.log_msg <- function(level, ...) {
  if (.LOG_LEVELS[[level]] < .LOG_LEVELS[[.log_env$threshold]]) return(invisible(NULL))

  args <- list(...)
  if (length(args) == 0L) {
    msg <- ""
  } else if (length(args) == 1L && grepl("\\{", args[[1]])) {
    msg <- tryCatch(
      glue::glue(args[[1]], .envir = parent.frame(2)),
      error = function(e) paste0(args, collapse = "")
    )
  } else {
    msg <- paste0(args, collapse = "")
  }

  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- paste0("[", timestamp, "] ", level, ": ", msg)

  .log_env$buffer <- c(.log_env$buffer, line)
  message(line)
  invisible(NULL)
}

# Public log functions matching the logger API
log_trace   <- function(...) .log_msg("TRACE", ...)
log_debug   <- function(...) .log_msg("DEBUG", ...)
log_info    <- function(...) .log_msg("INFO", ...)
log_success <- function(...) .log_msg("SUCCESS", ...)
log_warn    <- function(...) .log_msg("WARN", ...)
log_error   <- function(...) .log_msg("ERROR", ...)

#' Logs a list or data frame at DEBUG level.
#'
#' @param title Title for the log entry.
#' @param l     List or data.frame to log.
log_debug_list <- function(title, l) {
  log_debug(aNCA:::.concatenate_list(title, l))
}

#' Return the in-memory log buffer as a character vector.
#' @noRd
get_log_buffer <- function() {
  .log_env$buffer
}
