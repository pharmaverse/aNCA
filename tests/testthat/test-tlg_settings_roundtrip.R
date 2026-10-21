# Exercise submitted TLG settings across fresh Shiny sessions and real YAML.
local({
  library(shiny)
  library(dplyr)
  library(purrr)
  library(logger)
  library(reactable)
  library(reactable.extras)
  library(bslib)
  library(shinyWidgets)
  shiny_dir <- system.file("shiny", package = "aNCA")
  for (path in c(
    "functions/utils.R", "functions/utils-tlg-settings.R", "functions/utils-tlg.R",
    "functions/tlg_add_picker.R", "functions/zip-utils.R", "functions/tlg_export.R",
    "modules/common/reactable.R", "modules/tab_tlg/tlg_module.R",
    "modules/tab_tlg/tlg_option_text.R", "modules/tab_tlg/tlg_option_select.R",
    "modules/tab_tlg/tlg_option_numeric.R", "modules/tab_tlg/tlg_option_table.R",
    "modules/tab_tlg.R"
  )) source(file.path(shiny_dir, path), local = TRUE)
}, envir = parent.env(environment()))

.contract_render_panels <- function(output) {
  invisible(lapply(c("tables", "graphs", "listings"), function(id) output[[id]]))
}

.contract_export_session <- function(session) {
  session$userData$settings <- reactive(list(method = "linear"))
  session$userData$units_table <- reactive(NULL)
  session$userData$ratio_table <- reactive(NULL)
  session$userData$slope_rules <- reactive(NULL)
  session$userData$settings_versions <- reactiveVal(list())
}

describe("Submitted TLG settings: fresh-session round trip", {
  it("reproduces actual table values and labels, preserving explicit empty controls", {
    saved <- new.env(parent = emptyenv())
    saved$dir <- withr::local_tempdir()
    conc <- FIXTURE_CONC_DATA[FIXTURE_CONC_DATA$USUBJID == 1, ]
    conc$TRT01A <- "Treatment A"
    data <- reactive(list(conc = list(data = conc)))
    testServer(tab_tlg_server, args = list(data = data), {
      session$flushReact()
      order <- tlg_order()
      order$Selection <- order$id == match("t_pkct01", names(.TLG_DEFINITIONS))
      tlg_order(order)
      session$setInputs(submit_tlg_order = 1)
      .contract_render_panels(output)
      session$setInputs(`1_tbl-time_filter-select` = c("1", "3"),
                        `1_tbl-strat_var-select` = "NFRLT",
                        `1_tbl-list_vars-select` = character(),
                        `1_tbl-stats-select` = "Mean",
                        `1_tbl-title-text` = "Study $STUDYID",
                        `1_tbl-subtitle-text` = "Assay: !AVAL",
                        `1_tbl-footnote-text` = "Reviewed\nPK",
                        `1_tbl-order_comment-text` = "Check source data")
      session$elapse(800)
      session$flushReact()
      saved$items <- session$userData$tlg_outputs("table")$t_pkct01$items
      expect_length(saved$items, 1L)
      expect_named(saved$items, "all")
      expect_equal(as.numeric(saved$items[[1]]$Mean), c(1, 3))
      expect_equal(attr(saved$items[[1]], "tlg_title"), "Study S1")
      expect_equal(attr(saved$items[[1]], "tlg_subtitle"), "Assay: Analysis Value")
      expect_identical(session$userData$tlg_options()$t_pkct01$list_vars, character())
      expect_identical(session$userData$tlg_order()$t_pkct01$Comment, "Check source data")
      expect_null(session$userData$tlg_labels()$t_pkct01$order_comment)
      .contract_export_session(session)
      .export_settings(saved$dir, session)
      saved$payload <- read_settings(file.path(saved$dir, "settings.yaml"))

      # A draft removal has not changed the actual outputs; export their submitted order.
      draft <- tlg_order()
      draft$Selection[] <- FALSE
      tlg_order(draft)
      expect_true(session$userData$tlg_order()$t_pkct01$Selection)
      expect_length(session$userData$tlg_outputs("table"), 1)
      session$setInputs(submit_tlg_order = 2)
      .contract_render_panels(output)
      expect_false(session$userData$tlg_order()$t_pkct01$Selection)
      expect_length(session$userData$tlg_outputs("table"), 0)
    })

    restored <- reactiveVal(saved$payload)
    testServer(tab_tlg_server, args = list(data = data, settings_override = restored), {
      session$flushReact()
      expect_length(session$userData$tlg_outputs(), 0)
      session$setInputs(submit_tlg_order = 1)
      .contract_render_panels(output)
      session$elapse(800)
      session$flushReact()
      expect_equal(session$userData$tlg_outputs("table")$t_pkct01$items, saved$items)
      expect_identical(session$userData$tlg_options()$t_pkct01$list_vars, character())
      expect_identical(session$userData$tlg_order()$t_pkct01$Comment, "Check source data")
      expect_match(output$`1_tbl-options`$html, "Check source data", fixed = TRUE)
      session$setInputs(submit_tlg_order = 2)
      .contract_render_panels(output)
      expect_equal(session$userData$tlg_outputs("table")$t_pkct01$items, saved$items)

      session$setInputs(`1_tbl-time_filter-select` = "2", `1_tbl-title-text` = "Unsaved")
      restored(NULL)
      restored(saved$payload)
      session$flushReact()
      expect_length(session$userData$tlg_outputs(), 0)
      session$setInputs(submit_tlg_order = 3)
      .contract_render_panels(output)
      session$elapse(800)
      expect_equal(session$userData$tlg_outputs("table")$t_pkct01$items, saved$items)
      session$setInputs(`1_tbl-reset_widgets` = 1)
      expect_identical(session$userData$tlg_options()$t_pkct01$list_vars, c("PARAM", "PCSPEC"))
      expect_identical(session$userData$tlg_order()$t_pkct01$Comment, "")
      expect_identical(session$userData$tlg_labels()$t_pkct01$title,
                       .TLG_DEFINITIONS$t_pkct01$options$title$default)
    })
  })
})

describe("Dependent selector settings restoration", {
  it("restores stable option IDs regardless of catalog option order", {
    defs <- list(
      param_filter = list(type = "select", choices = "$PARAM",
                          choices_from = "param_var", multiple = TRUE),
      param_var = list(type = "select", choices = ".colnames", default = "PARAM")
    )
    restored <- reactiveVal(list(param_var = "PARAMCD", param_filter = "CMAX"))
    testServer(tlg_module_server, args = list(
      data = reactive(data.frame(PARAM = c("Cmax", "AUC"),
                                 PARAMCD = c("CMAX", "AUCLST"), AVAL = c(5, 20))),
      type = "table", render_list = function(data, param_var, param_filter = NULL) {
        list(data[is.null(param_filter) | data[[param_var]] %in% param_filter, ])
      }, options = defs, option_settings = restored
    ), {
      session$flushReact()
      expect_identical(options_values$param_var(), "PARAMCD")
      expect_identical(options_values$param_filter(), "CMAX")
      session$elapse(800)
      expect_equal(tlg_list()[[1]]$AVAL, 5)
      restored(list(param_var = "PARAM", param_filter = "AUC"))
      session$flushReact()
      expect_identical(options_values$param_var(), "PARAM")
      expect_identical(options_values$param_filter(), "AUC")
      session$elapse(800)
      expect_equal(tlg_list()[[1]]$AVAL, 20)
    })
  })
})

describe("Settings before ADPP becomes available", {
  it("retains registered imported options while the ADPP data is gated", {
    payload <- list(
      tlg_order = setNames(lapply(names(.TLG_DEFINITIONS), function(key) {
        list(Selection = key == "t_pkpt03_col")
      }), names(.TLG_DEFINITIONS)),
      tlg_options = list(t_pkpt03_col = list(
        param_filter = "Cmax", strat_var = "TRT01A", list_vars = "PPCAT"
      )),
      tlg_labels = list(t_pkpt03_col = list(title = "Imported title"))
    )
    adpp <- reactiveVal(NULL)
    testServer(tab_tlg_server, args = list(
      data = reactive(list(conc = list(data = data.frame(PCSPEC = "SERUM", AVAL = 1)))),
      adpp = adpp, settings_override = reactive(payload)
    ), {
      session$flushReact()
      session$setInputs(submit_tlg_order = 1)
      invisible(output$tables)
      session$flushReact()
      snapshot <- tryCatch(.tlg_settings_payload(session), error = identity)
      expect_false(inherits(snapshot, "error"))
      if (!inherits(snapshot, "error")) {
        expect_identical(snapshot$tlg_options$t_pkpt03_col$param_filter, "Cmax")
      }
      adpp(data.frame(USUBJID = "S1", PPCAT = "Drug", PPSPEC = "SERUM",
                      PARAM = c("Cmax", "AUC"), AVAL = c(5, 20), TRT01A = "A"))
      session$flushReact()
      session$elapse(800)
      expect_identical(session$userData$tlg_options()$t_pkpt03_col$param_filter, "Cmax")
    })
  })
})

describe("Immediate export settings consistency", {
  it("exports settings consistent with rendered output after a sidebar change", {
    testServer(tlg_module_server, args = list(
      data = reactive(data.frame(PARAM = c("A", "B"), AVAL = c(5, 20))),
      type = "table", render_list = function(data, param_filter = NULL) {
        list(if (!length(param_filter)) data else data[data$PARAM %in% param_filter, ])
      }, options = list(param_filter = list(type = "select", choices = "$PARAM", multiple = TRUE))
    ), {
      session$flushReact()
      session$elapse(800)
      expect_equal(session$getReturned()$items()[[1]]$AVAL, c(5, 20))
      session$setInputs(`param_filter-select` = "A")
      session$flushReact()
      values <- session$getReturned()$options()
      output_rows <- session$getReturned()$items()[[1]]
      expect_identical(values$param_filter, "A")
      expect_equal(output_rows$AVAL, 5)
    })
  })
})


describe("Listing displayed-column defaults", {
  it("retains the documented defaults when a saved empty selection is restored", {
    skip_if_not_installed("rlistings")
    conc <- FIXTURE_CONC_DATA[FIXTURE_CONC_DATA$USUBJID == 1, ]
    conc$TRT01A <- "Treatment A"
    conc <- apply_labels(conc, type = "ADNCA")
    definition <- .TLG_DEFINITIONS$l_pkcl01
    saved <- new.env(parent = emptyenv())
    for (run in 1:2) {
      restored <- if (run == 1L) {
        list()
      } else {
        .normalize_tlg_options(yaml::yaml.load(saved$yaml), .TLG_DEFINITIONS)$l_pkcl01
      }
      testServer(tlg_module_server, args = list(
        data = reactive(conc), type = "listing", render_list = l_pkcl01,
        options = definition$options, option_settings = reactive(restored)
      ), {
        session$flushReact()
        if (run == 1L) {
          session$setInputs(`displaying_vars-select` = character(),
                            `listgroup_vars-select` = character())
        }
        applied <- session$getReturned()$options()
        expect_identical(applied$displaying_vars, character())
        expect_identical(applied$listgroup_vars, character())
        labels <- session$getReturned()$labels()
        default_args <- applied[setdiff(names(applied), "displaying_vars")]
        default_args <- c(list(data = conc), default_args, labels[nzchar(unlist(labels))])
        expected <- do.call(l_pkcl01, default_args)
        expect_equal(session$getReturned()$items(), expected)
        expect_named(expected, "all")
        expect_true("AVAL" %in% names(expected[[1]]))
        saved$yaml <- yaml::as.yaml(list(l_pkcl01 = applied))
      })
    }
  })
})
