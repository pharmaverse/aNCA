# Boundary behaviour of tab_tlg_server.  The TLG modules receive an
# already-exclusion-filtered, label-restored plain data frame; the filtering
# and label restoration happen here (not inside tlg_module_server).

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
    # PKNCA/dplyr processing (and row-subsetting) strips label attributes; the
    # boundary re-applies them so parse_annotation() can resolve `!COLUMN`
    # references in title/subtitle/footnote/axis inputs downstream.
    expect_null(attr(adnca_df$AVAL, "label"))
    shiny::testServer(
      tab_tlg_server,
      args = list(
        data = shiny::reactive(list(conc = list(data = adnca_df))),
        adpp = shiny::reactive(adpp_df)
      ),
      {
        expect_equal(attr(conc_data_all()$AVAL, "label"), "Analysis Value")
        expect_equal(attr(conc_data()$AVAL, "label"), "Analysis Value")
        expect_equal(attr(adpp_data_all()$AVAL, "label"), "Analysis Value")
        expect_equal(attr(adpp_data()$AVAL, "label"), "Analysis Value")
      }
    )
  })

  it("scopes each exclusion flag to its own dataset", {
    # A record flagged PPSUMXF (drop from PK-param summary) must still survive in
    # the concentration source, and vice-versa (issue 1356 / Gero review).
    shiny::testServer(
      tab_tlg_server,
      args = list(
        data = shiny::reactive(list(conc = list(data = adnca_df))),
        adpp = shiny::reactive(adpp_df)
      ),
      {
        # Filtered sources drop the flagged row from their OWN dataset only.
        expect_equal(nrow(conc_data()), 1)
        expect_equal(nrow(adpp_data()), 1)
        # Unfiltered "all" sources keep every row (used by individual listings).
        expect_equal(nrow(conc_data_all()), 2)
        expect_equal(nrow(adpp_data_all()), 2)
      }
    )
  })
})
