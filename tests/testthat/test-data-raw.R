# Verify that committed data/*.rda files match what data-raw/ scripts produce.
#
# Each data-raw script is sourced in an environment where usethis::use_data()
# is replaced with a capturing function. The captured objects are compared to
# the committed .rda data using identical().

library(dplyr)

# Helper: load all objects from an .rda file into a named list.
load_rda <- function(path) {
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  as.list(env)
}

# Helper: source a data-raw script, intercept usethis::use_data() calls,
# and return a named list of the objects that would have been saved.
capture_use_data <- function(script_path) {
  captured <- list()

  mock_use_data <- function(..., overwrite = FALSE, compress = "bzip2",
                            internal = FALSE) {
    objs <- eval(substitute(alist(...)))
    parent <- parent.frame()
    for (expr in objs) {
      nm <- deparse(expr)
      captured[[nm]] <<- eval(expr, envir = parent)
    }
  }

  # Build an environment that shadows usethis::use_data with the mock.
  # The script calls usethis::use_data(), so we need to intercept :: calls.
  script_env <- new.env(parent = globalenv())

  # Override :: so that usethis::use_data resolves to our mock
  script_env[["::"]] <- function(pkg, name) {
    pkg_str <- deparse(substitute(pkg))
    name_str <- deparse(substitute(name))
    if (pkg_str == "usethis" && name_str == "use_data") {
      return(mock_use_data)
    }
    getExportedValue(pkg_str, name_str)
  }

  source(script_path, local = script_env)
  captured
}

describe("data-raw outputs are up to date", {
  # Scripts that produce .rda files via usethis::use_data()
  rda_scripts <- c(
    "adnca_example.R",
    "metadata_nca_parameters.R",
    "metadata_nca_variables.R",
    "sdtm_example.R"
  )

  for (script_name in rda_scripts) {
    script_path <- file.path("data-raw", script_name)

    it(paste0(script_name, " produces data matching committed .rda"), {
      skip_if_not(file.exists(script_path), paste("Script not found:", script_path))

      regenerated <- capture_use_data(script_path)
      expect_gt(length(regenerated), 0,
                label = paste("No use_data() calls captured from", script_name))

      for (obj_name in names(regenerated)) {
        rda_path <- file.path("data", paste0(obj_name, ".rda"))
        skip_if_not(file.exists(rda_path), paste("Missing .rda:", rda_path))

        committed <- load_rda(rda_path)
        expect_true(
          identical(regenerated[[obj_name]], committed[[obj_name]]),
          label = paste0(
            obj_name, " from ", script_name,
            " differs from committed data/", obj_name, ".rda.",
            " Run 'Rscript data-raw/rebuild.R' and commit the results."
          )
        )
      }
    })
  }

  it("compile_css.R produces CSS matching committed main.css", {
    skip_if_not_installed("sass")
    scss_path <- "inst/shiny/www/styles/main.scss"
    css_path <- "inst/shiny/www/main.css"
    skip_if_not(file.exists(scss_path))
    skip_if_not(file.exists(css_path))

    committed <- readLines(css_path)
    regenerated <- strsplit(
      sass::sass(sass::sass_file(scss_path)),
      "\n"
    )[[1]]

    expect_identical(
      regenerated, committed,
      label = paste0(
        "main.css is stale.",
        " Run 'Rscript data-raw/compile_css.R' and commit the results."
      )
    )
  })

  it("test-multianalyte-ADNCA.R produces CSV matching committed file", {
    script_path <- "data-raw/test-multianalyte-ADNCA.R"
    csv_path <- "tests/testthat/data/test-multianalyte-ADNCA.csv"
    skip_if_not(file.exists(script_path))
    skip_if_not(file.exists(csv_path))

    committed <- read.csv(csv_path)

    # Re-run the transformation without writing the file
    regenerated <- read.csv(
      "data-raw/adnca_example.csv", na.strings = c("", "NA")
    ) %>%
      mutate(DOSFRM = ifelse(ROUTE == "ORAL", "TABLET", "INJECTION, SOLUTION")) %>%
      mutate(
        AVAL = ifelse(PARAM == "Metab-DrugA" & AVALU == "ug/mL", AVAL / 1000, AVAL),
        AVALU = ifelse(PARAM == "Metab-DrugA" & AVALU == "ug/mL", "mg/mL", AVALU)
      )

    expect_identical(
      regenerated, committed,
      label = paste0(
        "test-multianalyte-ADNCA.csv is stale.",
        " Run 'Rscript data-raw/rebuild.R' and commit the results."
      )
    )
  })
})
