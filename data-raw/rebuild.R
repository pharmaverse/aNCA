# Rebuild all data-raw outputs.
#
# Run this script before committing when you have changed any source file
# that feeds into data-raw/ (e.g. .scss partials, .csv metadata, example
# data). It sources every R script in data-raw/ except itself and
# test_suggests_hidden.R.
#
# Usage:
#   Rscript data-raw/rebuild.R

excluded <- c("rebuild.R", "test_suggests_hidden.R")

scripts <- list.files("data-raw", pattern = "\\.R$", full.names = TRUE)
scripts <- scripts[!basename(scripts) %in% excluded]

library(dplyr)
for (script in scripts) {
  message(">> Running ", script)
  source(script, local = new.env(parent = globalenv()))
}

# Write CSV snapshots of every .rda dataset to data-raw/test/.
# These deterministic text files are used by CI to detect stale outputs
# (R's save() serialization is not reproducible across versions/platforms,
# so .rda files cannot be diffed reliably).
test_dir <- "data-raw/test"
dir.create(test_dir, showWarnings = FALSE)

rda_files <- list.files("data", pattern = "\\.rda$", full.names = TRUE)
for (rda in rda_files) {
  env <- new.env(parent = emptyenv())
  load(rda, envir = env)
  obj_name <- ls(env)[1]
  write.csv(env[[obj_name]],
            file.path(test_dir, paste0(obj_name, ".csv")),
            row.names = FALSE)
  message(">> Snapshot: ", file.path(test_dir, paste0(obj_name, ".csv")))
}

message(">> All data-raw scripts completed.")
