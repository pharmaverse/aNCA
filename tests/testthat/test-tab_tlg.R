# Server-side tests for tab_tlg_server: add-picker selection, removal, and the
# Order Details edit write-back (issue #1335).
local({
  library(shiny)
  library(dplyr)
  library(purrr)
  library(logger)
  library(reactable)
  library(reactable.extras)
  shiny_dir <- system.file("shiny", package = "aNCA")
  source(file.path(shiny_dir, "functions", "tlg_add_picker.R"), local = TRUE)
  source(file.path(shiny_dir, "modules", "common", "reactable.R"), local = TRUE)
  source(file.path(shiny_dir, "modules", "tab_tlg", "tlg_module.R"), local = TRUE)
  source(file.path(shiny_dir, "modules", "tab_tlg.R"), local = TRUE)
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
