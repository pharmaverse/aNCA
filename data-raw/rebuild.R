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

for (script in scripts) {
  message(">> Running ", script)
  source(script, local = new.env(parent = globalenv()))
}

message(">> All data-raw scripts completed.")
