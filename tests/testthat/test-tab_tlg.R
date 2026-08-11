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
  # tlg_module_ui() builds a bslib layout_sidebar with a shinyWidgets dropdown; without
  # these the panel builder cannot run and no modules get registered.
  library(bslib)
  library(shinyWidgets)
  shiny_dir <- system.file("shiny", package = "aNCA")
  for (f in list(
    c("functions", "tlg_add_picker.R"),
    c("functions", "zip-utils.R"),
    c("functions", "tlg_export.R"),
    c("modules", "tab_tlg", "tlg_module.R"),
    # All four option types: tlg_module_server() resolves the per-option server by name
    # (`tlg_option_<type>_server`), so a missing one aborts module init partway and leaves
    # its `tlg_list` unusable.
    c("modules", "tab_tlg", "tlg_option_select.R"),
    c("modules", "tab_tlg", "tlg_option_text.R"),
    c("modules", "tab_tlg", "tlg_option_numeric.R"),
    c("modules", "tab_tlg", "tlg_option_table.R"),
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
    PKSUM1F = c("Y", NA_character_), stringsAsFactors = FALSE
  )
  adpp_df <- data.frame(
    USUBJID = c("S1", "S2"), AVAL = c(3, 4),
    PPSUMFL = c("Y", NA_character_), stringsAsFactors = FALSE
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
    # A record flagged PPSUMFL (drop from PK-param summary) must still survive in
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

# Bulk export of the rendered TLGs (issue #1344).  Each module hands its `tlg_list`
# reactive back to tab_tlg_server, which keeps them in `.tlg_registry`; the download
# handler resolves that registry and zips the result.

#' Touch the three panel outputs so their renderUI runs and registers the modules.
#'
#' The ADPP-backed panels `validate()` out when NCA has not run (this fixture passes no
#' `adpp`), which `testServer` re-raises on output access.  In the app that is a gated
#' panel, not a failure, so it is swallowed here -- the point is only to trigger
#' registration.
render_tlg_panels <- function(output) {
  try(output$tables, silent = TRUE)
  try(output$listings, silent = TRUE)
  try(output$graphs, silent = TRUE)
  invisible(NULL)
}

describe("tab_tlg_server: TLG export registry", {
  it("registers an entry for every rendered TLG", {
    testServer(tab_tlg_server, args = list(data = test_data), {
      # tlg_order_filtered() is bindEvent(submit_tlg_order), so nothing renders until the
      # order is submitted -- the same sequence a user goes through.
      session$setInputs(submit_tlg_order = 1)
      session$flushReact()
      render_tlg_panels(output)
      session$flushReact()

      ids <- ls(envir = .tlg_registry)
      expect_gt(length(ids), 0)
      # Ids are the catalog keys, and every entry carries its definition and type.
      entry <- get(ids[1], envir = .tlg_registry)
      expect_setequal(names(entry), c("def", "type", "items"))
      expect_true(entry$type %in% c("table", "listing", "graph"))
      expect_true(is.function(entry$items))
    })
  })

  it("exports only the currently selected TLGs, not everything ever rendered", {
    testServer(tab_tlg_server, args = list(data = test_data), {
      session$setInputs(submit_tlg_order = 1)
      session$flushReact()
      render_tlg_panels(output)
      session$flushReact()
      expect_gt(length(.collect_tlg_outputs()), 1)

      # Narrow the order to a single TLG and re-submit.  Modules stay registered on
      # purpose (removing them would re-create their observers on re-add), so the registry
      # keeps growing -- but the download must follow the order as it stands now.
      keep <- tlg_order()$id[tlg_order()$Selection][1]
      o <- tlg_order()
      o$Selection <- o$id == keep
      tlg_order(o)
      session$setInputs(submit_tlg_order = 2)
      session$flushReact()
      render_tlg_panels(output)
      session$flushReact()

      expect_gt(length(ls(envir = .tlg_registry)), 1)  # registry is still append-only
      collected <- .collect_tlg_outputs()
      expect_length(collected, 1)
      expect_equal(names(collected), names(.TLG_DEFINITIONS)[keep])
    })
  })

  it("collects outputs without raising when a TLG is still gated or failing", {
    testServer(tab_tlg_server, args = list(data = test_data), {
      session$setInputs(submit_tlg_order = 1)
      session$flushReact()
      render_tlg_panels(output)
      session$flushReact()

      # The fixture is deliberately minimal, so most TLGs cannot render.  Collection must
      # still succeed -- a req()-gated module is "not ready", not a failure.
      entries <- expect_no_error(.collect_tlg_outputs())
      expect_gt(length(entries), 0)
      expect_true(all(vapply(entries, function(e) "items" %in% names(e), logical(1))))
    })
  })
})

describe("tab_tlg_server: download handler", {
  it("produces a zip containing a manifest", {
    testServer(tab_tlg_server, args = list(data = test_data), {
      session$setInputs(submit_tlg_order = 1)
      session$flushReact()
      render_tlg_panels(output)
      session$flushReact()

      zipfile <- withr::local_tempfile(fileext = ".zip")
      .run_tlg_export(.collect_tlg_outputs(), zipfile, session)

      expect_true(file.exists(zipfile))
      expect_true("manifest.csv" %in% zip::zip_list(zipfile)$filename)
    })
  })

  it("notifies instead of writing an archive when nothing has rendered", {
    testServer(tab_tlg_server, args = list(data = test_data), {
      zipfile <- withr::local_tempfile(fileext = ".zip")
      expect_null(.run_tlg_export(list(), zipfile, session))
      expect_false(file.exists(zipfile))
    })
  })
})
