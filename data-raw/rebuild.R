# Rebuild all data-raw outputs.
#
# Run this script before committing when you have changed any source file
# that feeds into data-raw/ (e.g. .scss partials, .csv metadata, example
# data). It sources every R script in data-raw/ except itself and
# test_suggests_hidden.R.
#
# After rebuilding, it compares the regenerated .rda datasets to the
# previously committed versions. Any mismatch is reported. When running
# in GitHub Actions (GITHUB_ACTIONS=true), mismatches cause the script
# to stop() so the CI step fails automatically.
#
# Usage:
#   Rscript data-raw/rebuild.R

# --- Load committed .rda files before rebuilding ---
load_rda <- function(path) {
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  as.list(env)
}

rda_files <- list.files("data", pattern = "\\.rda$", full.names = TRUE)
before <- list()
for (f in rda_files) {
  before[[f]] <- load_rda(f)
}

# --- Run all data-raw scripts ---
excluded <- c("rebuild.R", "test_suggests_hidden.R")

scripts <- list.files("data-raw", pattern = "\\.R$", full.names = TRUE)
scripts <- scripts[!basename(scripts) %in% excluded]

library(dplyr)
for (script in scripts) {
  message(">> Running ", script)
  source(script, local = new.env(parent = globalenv()))
}

# --- Compare regenerated .rda files to committed versions ---
mismatches <- character()
for (f in rda_files) {
  after <- load_rda(f)
  if (!identical(before[[f]], after)) {
    obj_name <- names(after)[1]
    mismatches <- c(mismatches, obj_name)
    message("MISMATCH: ", obj_name, " (", f, ") has changed")
  }
}

# Check for new .rda files that didn't exist before
new_rda <- setdiff(
  list.files("data", pattern = "\\.rda$", full.names = TRUE),
  rda_files
)
for (f in new_rda) {
  obj_name <- names(load_rda(f))[1]
  mismatches <- c(mismatches, obj_name)
  message("MISMATCH: ", obj_name, " (", f, ") is new")
}

if (length(mismatches) > 0) {
  msg <- paste0(
    length(mismatches), " dataset(s) changed: ",
    paste(mismatches, collapse = ", "),
    "\nRun 'Rscript data-raw/rebuild.R' and commit the updated .rda files."
  )
  message("\n>> ", msg)

  if (identical(Sys.getenv("GITHUB_ACTIONS"), "true")) {
    stop(msg, call. = FALSE)
  }
} else {
  message(">> All datasets are up to date.")
}

message(">> All data-raw scripts completed.")
