# Source the ratio calculations table module to test its pure helpers
local({
  library(shiny)
  shiny_dir <- system.file("shiny", package = "aNCA")
  source(
    file.path(shiny_dir, "modules", "tab_nca", "setup", "ratio_calculations_table.R"),
    local = TRUE
  )
},
envir = parent.env(environment()))

describe(".build_interval_param_options", {
  it("builds standard parameter_start-end labels", {
    int_params <- data.frame(
      parameter = c("AUCINT", "CAVGINT"),
      start_auc = c(0, 0),
      end_auc = c(24, 12),
      stringsAsFactors = FALSE
    )
    expect_equal(
      .build_interval_param_options(int_params),
      c("AUCINT_0-24", "CAVGINT_0-12")
    )
  })

  it("uses the custom name when set, standard otherwise (#1463)", {
    int_params <- data.frame(
      parameter = c("AUCINT", "CAVGINT"),
      start_auc = c(0, 0),
      end_auc = c(24, 12),
      custom_name = c("Early exposure", NA),
      stringsAsFactors = FALSE
    )
    expect_equal(
      .build_interval_param_options(int_params),
      c("Early exposure", "CAVGINT_0-12")
    )
  })

  it("ignores incomplete rows (missing start/end)", {
    int_params <- data.frame(
      parameter = c("AUCINT", "CAVGINT"),
      start_auc = c(0, NA),
      end_auc = c(24, 12),
      custom_name = c("Early exposure", "ignored"),
      stringsAsFactors = FALSE
    )
    expect_equal(
      .build_interval_param_options(int_params),
      "Early exposure"
    )
  })

  it("returns an empty vector for NULL or empty input", {
    expect_equal(.build_interval_param_options(NULL), character(0))
    expect_equal(
      .build_interval_param_options(
        data.frame(parameter = character(), start_auc = numeric(), end_auc = numeric())
      ),
      character(0)
    )
  })
})
