# CI-only policy: keep the approved R 4.4 dependency NOTE visible without
# making required app dependencies optional. Any change to the NOTE needs review.
check_r_cmd_results <- function(result, r_version = getRversion()) {
  stopifnot(
    is.list(result),
    all(c("errors", "warnings", "notes") %in% names(result)),
    is.character(result$errors), is.character(result$warnings), is.character(result$notes)
  )
  expected_note <- paste(
    "checking package dependencies ... NOTE",
    "Imports includes 24 non-default packages.",
    "Importing from so many packages makes the package vulnerable to any of",
    "them becoming unavailable. Move as many as possible to Suggests and",
    "use conditionally."
  )
  r_version <- numeric_version(r_version)
  allowed_note <- r_version >= "4.4" && r_version < "4.5" &&
    length(result$notes) == 1L &&
    identical(trimws(gsub("[[:space:]]+", " ", result$notes)), expected_note)

  unaccepted <- c(result$errors, result$warnings, if (!allowed_note) result$notes)
  if (length(unaccepted)) {
    stop("R CMD check found diagnostics outside the approved R 4.4 exception.", call. = FALSE)
  }
  if (allowed_note) {
    message("Approved R 4.4 exception: exactly 24 non-default Imports; NOTE retained in check log.")
  }
  invisible(result)
}
