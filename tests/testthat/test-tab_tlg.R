# Boundary behaviour of tab_tlg_server.  The TLG modules receive an unfiltered,
# label-restored plain data frame; summary-exclusion filtering happens inside the
# summary/mean TLG functions (filter_summary_excluded), not at this boundary.

# Source the tab_tlg module and its server-side dependencies.
local({
  library(shiny)
  library(logger)
  library(reactable)
  library(dplyr)
  shiny_dir <- system.file("shiny", package = "aNCA")
  for (f in list(
    c("modules", "tab_tlg", "tlg_module.R"),
    c("modules", "tab_tlg", "tlg_option_select.R"),
    c("modules", "common", "reactable.R"),
    c("modules", "tab_tlg.R")
  )) {
    source(do.call(file.path, c(list(shiny_dir), as.list(f))), local = TRUE)
  }
},
envir = parent.env(environment()))

describe("tab_tlg_server: data boundary", {
  adnca_df <- data.frame(
    USUBJID = c("S1", "S2"), AVAL = c(1, 2),
    PKSUMXF = c("Y", NA_character_), stringsAsFactors = FALSE
  )
  adpp_df <- data.frame(
    USUBJID = c("S1", "S2"), AVAL = c(3, 4),
    PPSUMXF = c("Y", NA_character_), stringsAsFactors = FALSE
  )

  it("restores column labels on every data source (issue 1336)", {
    # PKNCA/dplyr processing strips label attributes; the boundary re-applies
    # them so parse_annotation() can resolve `!COLUMN` references in
    # title/subtitle/footnote/axis inputs downstream.
    expect_null(attr(adnca_df$AVAL, "label"))
    shiny::testServer(
      tab_tlg_server,
      args = list(
        data = shiny::reactive(list(conc = list(data = adnca_df))),
        adpp = shiny::reactive(adpp_df)
      ),
      {
        expect_equal(attr(conc_data()$AVAL, "label"), "Analysis Value")
        expect_equal(attr(adpp_data()$AVAL, "label"), "Analysis Value")
      }
    )
  })

  it("passes unfiltered data to the modules (exclusion happens in the TLG funcs)", {
    # Individual plots and listings must see summary-excluded rows; the boundary
    # therefore keeps every record and leaves filtering to the summary/mean
    # functions (filter_summary_excluded) (#1438).
    shiny::testServer(
      tab_tlg_server,
      args = list(
        data = shiny::reactive(list(conc = list(data = adnca_df))),
        adpp = shiny::reactive(adpp_df)
      ),
      {
        expect_equal(nrow(conc_data()), 2)
        expect_equal(nrow(adpp_data()), 2)
      }
    )
  })
})
