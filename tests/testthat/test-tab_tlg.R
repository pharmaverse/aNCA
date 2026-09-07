# Server-side tests for tab_tlg_server:
#  - add-picker selection, removal, and the Order Details edit write-back (issue #1335)
#  - data boundary: the TLG modules receive an already-exclusion-filtered,
#    label-restored plain data frame; the filtering and label restoration happen
#    here, not inside tlg_module_server (issue #1336 / #1356).

# Source the tab_tlg module and its server-side dependencies.
local({
  library(shiny)
  library(dplyr)
  library(purrr)
  library(logger)
  library(reactable)
  library(reactable.extras)
  shiny_dir <- system.file("shiny", package = "aNCA")
  for (f in list(
    c("functions", "tlg_add_picker.R"),
    c("modules", "tab_tlg", "tlg_module.R"),
    c("modules", "tab_tlg", "tlg_option_select.R"),
    c("modules", "common", "reactable.R"),
    c("modules", "tab_tlg.R")
  )) {
    source(do.call(file.path, c(list(shiny_dir), as.list(f))), local = TRUE)
  }
},
envir = parent.env(environment()))

test_data <- reactive(list(conc = list(data = data.frame(
  USUBJID = c("S1", "S2"),
  PCSPEC  = c("PLASMA", "PLASMA"),
  AVAL    = c(1, 2),
  stringsAsFactors = FALSE
))))

describe("tab_tlg_server: add-picker selection", {
  it("sets Selection = TRUE for exactly the checked ids on confirm", {
    testServer(tab_tlg_server, args = list(data = test_data), {
      session$flushReact()
      target <- head(tlg_order()$id[!tlg_order()$Selection], 2)
      expect_length(target, 2)

      # The confirm handler reads modal_group_ids() and input[[gid]]; drive it
      # directly (values not belonging to a real group are simply ignored by the
      # id %in% checked_ids mapping).
      modal_group_ids("grp")
      session$setInputs(grp = as.character(target))
      session$setInputs(confirm_add_tlg = 1)
      session$flushReact()

      expect_true(all(tlg_order()$Selection[tlg_order()$id %in% target]))
    })
  })

  it("leaves Selection unchanged when nothing is checked", {
    testServer(tab_tlg_server, args = list(data = test_data), {
      session$flushReact()
      before <- tlg_order()$Selection
      modal_group_ids("grp")
      session$setInputs(grp = character(0))
      session$setInputs(confirm_add_tlg = 1)
      session$flushReact()
      expect_identical(tlg_order()$Selection, before)
    })
  })
})

describe("tab_tlg_server: Order Details edit write-back", {
  it("writes an edited Footnote into the matching full-frame row", {
    testServer(tab_tlg_server, args = list(data = test_data), {
      session$flushReact()
      # First selected (displayed) row maps to the first Selection == TRUE row.
      first_id <- tlg_order()$id[tlg_order()$Selection][1]

      # selected_tlg_state()$edit() is fed by the nested reactable module's
      # edit_<col> input; set it through the namespaced id and clear the debounce.
      session$setInputs(
        `selected_tlg_table-edit_Footnote` = list(row = 1, column = "Footnote", value = "My note")
      )
      session$elapse(800)
      session$flushReact()

      row <- tlg_order()[tlg_order()$id == first_id, ]
      expect_equal(row$Footnote, "My note")
    })
  })

  it("ignores an edit targeting a non-editable column", {
    testServer(tab_tlg_server, args = list(data = test_data), {
      session$flushReact()
      before <- tlg_order()
      session$setInputs(
        `selected_tlg_table-edit_Footnote` = list(row = 1, column = "Type", value = "HACKED")
      )
      session$elapse(800)
      session$flushReact()
      expect_identical(tlg_order()$Type, before$Type)
    })
  })
})

describe("tab_tlg_server: data boundary", {
  adnca_df <- data.frame(
    USUBJID = c("S1", "S2"), AVAL = c(1, 2),
    PKSUMXF = c("Y", NA_character_), stringsAsFactors = FALSE
  )
  adpp_df <- data.frame(
    USUBJID = c("S1", "S2"), AVAL = c(3, 4),
    ANL01FL = c("", "Y"), stringsAsFactors = FALSE
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
    # A record without ANL01FL (drop from PK-param summary) must still survive in
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
