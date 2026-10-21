# Server-side tests for tab_tlg_server:
#  - add-picker selection, removal, and the Order Details edit write-back (issue #1335)
#  - data boundary: the TLG modules receive an unfiltered, label-restored plain
#    data frame; summary-exclusion filtering happens inside the summary/mean TLG
#    functions (filter_summary_excluded), not at this boundary (issue #1438).

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
    c("functions", "utils.R"),
    c("functions", "tlg_add_picker.R"),
    c("functions", "zip-utils.R"),
    c("functions", "utils-tlg.R"),
    c("functions", "utils-tlg-settings.R"),
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
    c("modules", "tab_data", "data_upload.R"),
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
  it("preserves edited labels in rebuilt panels and the export registry after resubmission", {
    conc <- FIXTURE_CONC_DATA[FIXTURE_CONC_DATA$USUBJID == 1, ]
    conc$TRT01A <- "Treatment A"
    testServer(
      tab_tlg_server,
      args = list(data = reactive(list(conc = list(data = conc)))),
      {
        session$flushReact()
        table_id <- match("t_pkct01", names(.TLG_DEFINITIONS))
        order <- tlg_order()
        order$Selection <- order$id == table_id
        tlg_order(order)
        session$setInputs(submit_tlg_order = 1)
        session$flushReact()
        render_tlg_panels(output)

        prefix <- paste0(table_id, "_tbl-")
        labels <- c("title", "subtitle", "footnote")
        initial <- c(.TLG_DEFINITIONS$t_pkct01$options$title$default, "", "Initial footnote")
        do.call(session$setInputs, setNames(as.list(initial), paste0(prefix, labels, "-text")))
        sidebar <- output[[paste0(prefix, "options")]]$html
        expect_match(sidebar, initial[[1]], fixed = TRUE)

        edited <- c("Study $STUDYID", "Assay: !AVAL", "Reviewed concentration summary")
        do.call(session$setInputs, setNames(as.list(edited), paste0(prefix, labels, "-text")))
        do.call(session$setInputs,
                setNames(list(c("1", "3")), paste0(prefix, "time_filter-select")))
        session$elapse(800)
        session$flushReact()
        # Typing should leave the active widgets intact, until the parent rebuilds them.
        expect_identical(output[[paste0(prefix, "options")]]$html, sidebar)

        session$setInputs(submit_tlg_order = 2)
        session$flushReact()
        render_tlg_panels(output)
        for (text in edited) {
          expect_match(output[[paste0(prefix, "options")]]$html, text, fixed = TRUE)
        }
        expect_length(ls(envir = .registered_modules), 1)

        # Exercise the collector used by the app-wide ZIP, including resolved annotation
        # values and real concentration summaries rather than only registry structure.
        exported <- session$userData$tlg_outputs("table")
        expect_named(exported, "t_pkct01")
        expect_length(exported$t_pkct01$items, 1)
        table <- exported$t_pkct01$items[[1]]
        expect_s3_class(table, "data.frame")
        expect_equal(attr(table, "tlg_title"), "Study S1")
        expect_equal(attr(table, "tlg_subtitle"), "Assay: Analysis Value")
        expect_equal(attr(table, "tlg_footnote"), edited[[3]])
        expect_equal(as.numeric(table$NFRLT), c(1, 3))
        expect_equal(as.numeric(table$n), c(1, 1))
        expect_equal(as.numeric(table$n_blq), c(0, 0))
        expect_equal(as.numeric(table$Mean), c(1, 3))
      }
    )
  })

  it("reuses the same modules when the order is submitted repeatedly", {
    testServer(tab_tlg_server, args = list(data = test_data), {
      session$setInputs(submit_tlg_order = 1)
      session$flushReact()
      render_tlg_panels(output)
      registered <- ls(envir = .registered_modules)
      items <- lapply(as.list(.tlg_registry), function(entry) entry$items)
      expect_gt(length(registered), 0)

      for (submission in 2:4) {
        session$setInputs(submit_tlg_order = submission)
        session$flushReact()
        render_tlg_panels(output)
        expect_identical(ls(envir = .registered_modules), registered)
        expect_identical(lapply(as.list(.tlg_registry), function(entry) entry$items), items)
      }
    })
  })

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
      expect_setequal(names(entry), c("def", "type", "items", "labels", "options", "comment"))
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


describe("tab_tlg_server: publishes outputs for the app-wide export", {
  it("exposes a collector on session$userData that zip.R can call", {
    # The download lives in the global "Export as ZIP" button, so this module only has to
    # publish; zip.R reads it through session$userData (#1344).
    testServer(tab_tlg_server, args = list(data = test_data), {
      session$setInputs(submit_tlg_order = 1)
      session$flushReact()
      render_tlg_panels(output)
      session$flushReact()

      collect <- session$userData$tlg_outputs
      expect_true(is.function(collect))
      expect_gt(length(collect()), 0)
      # Callable with a type filter, which is how the tree selection is applied.
      tables_only <- collect("table")
      expect_true(all(vapply(tables_only, function(e) e$type == "table", logical(1))))
      expect_lt(length(tables_only), length(collect()))
    })
  })
})

describe("tab_tlg_server: sidebar label settings", {
  it("round-trips raw labels and restores/reset them without changing the values", {
    conc <- FIXTURE_CONC_DATA[FIXTURE_CONC_DATA$USUBJID == 1, ]
    conc$TRT01A <- "Treatment A"
    restored <- reactiveVal(NULL)
    testServer(tab_tlg_server, args = list(
      data = reactive(list(conc = list(data = conc))), settings_override = restored
    ), {
      # Reading before registration must not cache an empty export forever.
      expect_identical(session$userData$tlg_labels(), list())
      order <- tlg_order()
      order$Selection <- order$id == match("t_pkct01", names(.TLG_DEFINITIONS))
      tlg_order(order)
      session$setInputs(submit_tlg_order = 1)
      render_tlg_panels(output)
      prefix <- paste0(order$id[order$Selection], "_tbl-")
      edited <- list(title = "Study $STUDYID", subtitle = "Assay: !AVAL", footnote = "Reviewed\nPK")
      do.call(session$setInputs, setNames(edited, paste0(prefix, names(edited), "-text")))
      session$flushReact()
      expect_identical(session$userData$tlg_labels()$t_pkct01, edited)

      session$userData$settings <- reactive(list(method = "linear"))
      session$userData$units_table <- reactive(NULL)
      session$userData$ratio_table <- reactive(NULL)
      session$userData$slope_rules <- reactive(NULL)
      session$userData$settings_versions <- reactiveVal(list())
      target <- withr::local_tempdir()
      .export_settings(target, session)
      imported <- read_settings(file.path(target, "settings.yaml"))
      expect_identical(imported$tlg_labels$t_pkct01, edited)
      expect_identical(.settings_export_payload(session)$tlg_labels, imported$tlg_labels)

      # An unopened graph's labels must survive a subsequent settings save.
      imported$tlg_labels$p_pkpg02_doseprop <- list(title = "Saved graph", subtitle = "")
      attr(imported, "versioned")$versions[[1]]$tlg_labels <- imported$tlg_labels
      upload <- function() {
        .apply_uploaded_settings(
          list(list(type = "settings", content = imported, name = "settings.yaml")),
          list(), restored, reactiveVal(NULL), session, session$ns
        )
      }
      expect_identical(upload(), list())
      session$flushReact()
      expect_identical(session$userData$tlg_labels()$t_pkct01, edited)
      expect_identical(session$userData$tlg_labels()$p_pkpg02_doseprop,
                       list(title = "Saved graph", subtitle = ""))
      session$setInputs(submit_tlg_order = 2)
      render_tlg_panels(output)
      expect_match(output[[paste0(prefix, "options")]]$html, "Assay: !AVAL", fixed = TRUE)
      do.call(session$setInputs, setNames(list(c("1", "3")), paste0(prefix, "time_filter-select")))
      session$elapse(800)
      result <- session$userData$tlg_outputs("table")$t_pkct01$items[[1]]
      expect_equal(attr(result, "tlg_title"), "Study S1")
      expect_equal(attr(result, "tlg_subtitle"), "Assay: Analysis Value")
      expect_equal(as.numeric(result$Mean), c(1, 3))

      do.call(session$setInputs, setNames(list("Unsaved edit"), paste0(prefix, "title-text")))
      expect_equal(session$userData$tlg_labels()$t_pkct01$title, "Unsaved edit")
      expect_identical(upload(), list())
      session$flushReact()
      expect_identical(session$userData$tlg_labels()$t_pkct01, edited)

      do.call(session$setInputs, setNames(list(1), paste0(prefix, "reset_widgets")))
      expect_equal(session$userData$tlg_labels()$t_pkct01$title,
                   .TLG_DEFINITIONS$t_pkct01$options$title$default)
      expect_equal(session$userData$tlg_labels()$t_pkct01$subtitle, "")

      # Loading old settings clears edits rather than inheriting another project's text.
      do.call(session$setInputs, setNames(list("Later edit"), paste0(prefix, "title-text")))
      restored(list(settings = list(method = "linear")))
      session$flushReact()
      expect_equal(session$userData$tlg_labels()$t_pkct01$title,
                   .TLG_DEFINITIONS$t_pkct01$options$title$default)
      expect_null(session$userData$tlg_labels()$p_pkpg02_doseprop)
    })
  })

  it("initializes unopened modules from uploaded labels using stable catalog keys", {
    restored <- reactiveVal(list(tlg_labels = list(t_pkct01 = list(
      title = "Uploaded title", subtitle = "", footnote = "!AVAL"
    ))))
    testServer(tab_tlg_server, args = list(data = test_data, settings_override = restored), {
      session$flushReact()
      expect_identical(session$userData$tlg_labels(), restored()$tlg_labels)
      session$setInputs(submit_tlg_order = 1)
      render_tlg_panels(output)
      expect_identical(session$userData$tlg_labels()$t_pkct01, restored()$tlg_labels$t_pkct01)
      expect_match(output$`1_tbl-options`$html, ">Uploaded title</textarea>", fixed = TRUE)
    })
  })
})


describe("tab_tlg_server: companion settings compatibility", {
  it("retains imported annotations and order metadata independently", {
    saved <- reactiveVal(list(
      tlg_labels = list(t_pkct01 = list(title = "Saved title", footnote = "Rendered note")),
      tlg_order = list(t_pkct01 = list(Selection = FALSE, Footnote = "Metadata note",
                                       Stratification = "SEX", Comment = "Saved comment"))
    ))
    testServer(tab_tlg_server, args = list(data = test_data, settings_override = saved), {
      session$flushReact()
      expect_identical(session$userData$tlg_labels()$t_pkct01, saved()$tlg_labels$t_pkct01)
      expect_identical(session$userData$tlg_order()$t_pkct01, saved()$tlg_order$t_pkct01)
      saved(list(tlg_labels = list(t_pkct01 = list(title = "Earlier version")),
                 tlg_order = list(t_pkct01 = list(Selection = TRUE, Footnote = "Earlier note"))))
      session$flushReact()
      expect_identical(session$userData$tlg_labels()$t_pkct01$title, "Earlier version")
      expect_identical(session$userData$tlg_order()$t_pkct01$Footnote, "Earlier note")
    })
  })
})

describe("TLG order settings validation", {
  it("matches catalog names and skips removed entries with a warning", {
    saved <- list(
      removed_output = list(Selection = TRUE),
      t_pkct01 = list(Selection = FALSE, Footnote = "Saved note", id = 99, Label = "Old label")
    )
    expect_warning(
      result <- .normalize_tlg_order(saved, rev(names(.TLG_DEFINITIONS))),
      "Skipped TLGs no longer in the catalog: removed_output"
    )
    expect_identical(result, list(t_pkct01 = list(Selection = FALSE, Footnote = "Saved note")))
  })

  it("accepts blank and missing text while rejecting malformed fields", {
    saved <- list(t_pkct01 = list(
      Selection = NA, Footnote = NULL, Stratification = "", Comment = c("one", "two")
    ))
    messages <- character()
    result <- withCallingHandlers(
      .normalize_tlg_order(saved, names(.TLG_DEFINITIONS)),
      warning = function(w) {
        messages <<- c(messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    expect_length(messages, 2)
    expect_identical(result$t_pkct01, list(Footnote = NA_character_, Stratification = ""))
    expect_identical(.normalize_tlg_order(NULL, names(.TLG_DEFINITIONS)), list())
    expect_warning(.normalize_tlg_order(list(list(Selection = TRUE)), "t_pkct01"), "unique catalog")
    expect_warning(
      .normalize_tlg_order(list(t_pkct01 = "bad row"), "t_pkct01"), "invalid TLG order row"
    )
  })

  it("restores reordered catalog entries and leaves new entries at their defaults", {
    catalog_ids <- c("new_output", "t_pkct01_dose", "t_pkct01")
    defaults <- data.frame(
      id = 1:3,
      Selection = c(FALSE, TRUE, FALSE),
      Footnote = NA_character_,
      Label = c("New output", "Current dose label", "Current label")
    )
    saved <- .normalize_tlg_order(list(
      t_pkct01 = list(Selection = TRUE, Footnote = "Saved note"),
      t_pkct01_dose = list(Selection = FALSE)
    ), catalog_ids)
    restored <- .restore_tlg_order(defaults, saved, catalog_ids)
    expect_identical(restored$Selection, c(FALSE, FALSE, TRUE))
    expect_identical(restored$Footnote, c(NA_character_, NA_character_, "Saved note"))
    expect_identical(restored[c("id", "Label")], defaults[c("id", "Label")])
  })
})

describe("TLG order restore compatibility", {
  it("refreshes an existing table and resets absent fields and legacy settings to defaults", {
    restored <- reactiveVal(NULL)
    testServer(tab_tlg_server, args = list(data = test_data, settings_override = restored), {
      session$flushReact()
      initial <- displayed_order()
      restored(list(tlg_order = list(t_pkct01_dose = list(
        Selection = TRUE, Footnote = "First version", Comment = "Keep only in first version"
      ))))
      session$flushReact()
      expect_equal(nrow(displayed_order()), nrow(initial) + 1L)
      expect_equal(session$userData$tlg_order()$t_pkct01_dose$Footnote, "First version")
      expect_named(displayed_order(), c("Type", "Dataset", "Output"))
      expect_true(session$userData$tlg_order()$t_pkct01$Selection) # New catalog row keeps default.

      restored(list(tlg_order = list(t_pkct01_dose = list(Selection = FALSE))))
      session$flushReact()
      expect_identical(displayed_order(), initial)
      expect_true(is.na(session$userData$tlg_order()$t_pkct01_dose$Comment))
      expect_true(is.na(session$userData$tlg_order()$t_pkct01_dose$Footnote))

      restored(list(tlg_order = list(t_pkct01 = list(Selection = FALSE))))
      session$flushReact()
      expect_false(session$userData$tlg_order()$t_pkct01$Selection)
      restored(list(settings = list(method = "linear")))
      session$flushReact()
      expect_identical(displayed_order(), initial)
    })
  })

  it("preserves urine deselections when data arrives later and retains defaults for new outputs", {
    data <- reactiveVal(NULL)
    restored <- reactiveVal(list(tlg_order = list(t_pkpt08_uri = list(Selection = FALSE))))
    testServer(tab_tlg_server, args = list(data = data, settings_override = restored), {
      session$flushReact()
      data(list(conc = list(data = data.frame(PCSPEC = "URINE", AVAL = 1))))
      session$flushReact()
      expect_false(session$userData$tlg_order()$t_pkpt08_uri$Selection)
      expect_true(session$userData$tlg_order()$l_pkcl02_uri$Selection)
      expect_true(session$userData$tlg_order()$t_pkct01$Selection)
      data(list(conc = list(data = data.frame(PCSPEC = "URINE", AVAL = 2))))
      session$flushReact()
      expect_false(session$userData$tlg_order()$t_pkpt08_uri$Selection)
    })
  })
})
