# Manual test: verify run_app() works without Suggests packages
#
# This script hides all Suggests packages (except testthat/covr needed by
# devtools) and launches the app. Use it to verify that:
#
#   1. The app starts without errors.
#   2. Core workflow works (upload → map → NCA → export).
#   3. Optional features (SAS import, PowerPoint export, listings) show
#      graceful messages instead of crashing.
#
# Usage:
#   1. Open a fresh R session (Ctrl+Shift+F10 in RStudio).
#   2. source("data-raw/test_suggests_hidden.R")
#   3. Test the app manually.
#   4. Stop the app (Ctrl+C / Esc), then run restore_packages() to unhide.
#
# WARNING: Do NOT run this in CI. It renames package directories on disk.

# --- Hide Suggests packages ---
desc <- read.dcf("DESCRIPTION", fields = "Suggests")
hide <- trimws(unlist(strsplit(desc[1, "Suggests"], ",")))
hide <- gsub("\\s*\\(.*\\)", "", hide)
hide <- hide[nzchar(hide)]

# Keep packages that devtools::load_all() needs
hide <- setdiff(hide, c("testthat", "covr"))

lib <- .libPaths()[1]
hidden <- character(0)

for (pkg in hide) {
  from <- file.path(lib, pkg)
  if (dir.exists(from)) {
    file.rename(from, paste0(from, "_HIDDEN"))
    hidden <- c(hidden, pkg)
  }
}
cat("Hidden:", paste(hidden, collapse = ", "), "\n")

# --- Load and run ---
devtools::load_all()
aNCA::run_app()

# --- Restore function (call after stopping the app) ---
restore_packages <- function() {
  for (pkg in hidden) {
    to <- file.path(lib, pkg)
    from <- paste0(to, "_HIDDEN")
    if (dir.exists(from)) file.rename(from, to)
  }
  cat("Restored:", paste(hidden, collapse = ", "), "\n")
}
