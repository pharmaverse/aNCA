local({
  library(shiny)
  library(shinyWidgets)
  library(logger)
  shiny_dir <- system.file("shiny", package = "aNCA")
  source(file.path(shiny_dir, "functions", "zip-utils.R"), local = TRUE)
  source(file.path(shiny_dir, "functions", "tlg_export.R"), local = TRUE)
  source(file.path(shiny_dir, "modules", "tab_tlg.R"), local = TRUE)
  for (file in c("tlg_module", "tlg_option_select", "tlg_option_text")) {
    source(file.path(shiny_dir, "modules", "tab_tlg", paste0(file, ".R")), local = TRUE)
  }
}, envir = parent.env(environment()))

describe("NEST catalog and Shiny integration", {
  it("keeps pilots opt-in and resolves dose-safe defaults and inherited controls", {
    defs <- .TLG_DEFINITIONS
    for (id in c("t_pkpt03_nest", "l_pkpl01_nest", "g_pkcg03_lin_nest")) {
      expect_false(defs[[id]]$is_default)
      expect_equal(defs[[id]]$fun, id)
      expect_true(all(c("title", "subtitle", "footnote") %in% names(defs[[id]]$options)))
    }
    expect_equal(defs$t_pkpt03_nest$options$list_vars$default, c("PPCAT", "PPSPEC", "ATPTREF"))
    expect_equal(defs$t_pkpt03_nest$options$strat_var$default, c("TRT01A", "PARAM"))
    expect_equal(defs$t_pkpt03_nest$options$list_vars$type, "select")
    expect_true(defs$t_pkpt03_col$is_default)
    expect_true(defs$l_pkpl01$is_default)
  })

  it("updates listing selection and annotations and renders HTML in the module", {
    skip_if_not_installed("crane", "0.3.2")
    opts <- .TLG_DEFINITIONS$l_pkpl01_nest$options[c("title", "param_var", "param_filter")]
    shiny::testServer(tlg_module_server,
      args = list(data = shiny::reactive(nest_adpp_fixture()), type = "listing",
                  render_list = l_pkpl01_nest, options = opts), {
        session$setInputs(`param_var-select` = "PARAM", `param_filter-select` = "Cmax",
                          `title-text` = "Study $PPCAT", entries_per_page = "All")
        session$elapse(800)
        table <- tlg_list()[[1]]
        expect_named(attr(table, "tlg_data"), c("TRT01A", "USUBJID", "Cmax"))
        expect_equal(attr(table, "tlg_annotations")$title, "Study Drug")
        expect_match(output$tlg_output$html, "Cmax (mg/L)", fixed = TRUE)
        expect_match(output$tlg_output$html, "Study Drug", fixed = TRUE)
        # The default subtitle already names the split; no second group header.
        expect_false(grepl("tlg-table-group-header", output$tlg_output$html, fixed = TRUE))
      }
    )
  })

  it("renders every offered PKPT03 statistic through the sidebar", {
    skip_if_not_installed("gtsummary", "2.6.1")
    opts <- .TLG_DEFINITIONS$t_pkpt03_nest$options["stats"]
    choices <- unlist(opts$stats$choices)
    expected <- c(n = 3, Mean = 2.333, SD = 1.528, CV_pct = 65.5, Median = 2,
                  GeoMean = 2, GeoCV_pct = 78.5, Min = 1, Max = 4)
    expect_setequal(unname(choices), names(expected))
    widget <- shiny::isolate(tlg_option_select_ui("stats", opts$stats,
                                                  shiny::reactive(nest_adpp_fixture())))
    expect_match(as.character(widget), 'value="n"', fixed = TRUE)
    expect_false(grepl("n_blq", as.character(widget), fixed = TRUE))
    shiny::testServer(tlg_module_server,
      args = list(data = shiny::reactive(nest_adpp_fixture()), type = "table",
                  render_list = t_pkpt03_nest, options = opts), {
        for (stat in unname(choices)) {
          session$setInputs(`stats-select` = stat, entries_per_page = "All")
          session$elapse(800)
          table <- tlg_list()[[1]]
          frame <- attr(table, "tlg_data")
          expect_equal(as.numeric(frame[[stat]][frame$PARAM == "Cmax"]), unname(expected[stat]))
          expect_match(output$tlg_output$html, aNCA:::.STAT_LABELS[[stat]], fixed = TRUE)
        }
      }
    )
  })

  it("renders a no-data explanation when a selected listing split is entirely missing", {
    skip_if_not_installed("crane", "0.3.2")
    d <- nest_adpp_fixture()
    d$PPCAT <- NA_character_
    opts <- .TLG_DEFINITIONS$l_pkpl01_nest$options["listgroup_vars"]
    shiny::testServer(tlg_module_server,
      args = list(data = shiny::reactive(d), type = "listing",
                  render_list = l_pkpl01_nest, options = opts), {
        session$setInputs(`listgroup_vars-select` = c("PPCAT", "PPSPEC"),
                          entries_per_page = "All")
        session$elapse(800)
        expect_identical(tlg_list(), list(all = "No data available for this NEST listing."))
        expect_match(output$tlg_output$html,
                     "No data available for this NEST listing.", fixed = TRUE)
        expect_false(grepl("Error:", output$tlg_output$html, fixed = TRUE))
      }
    )
  })
})

describe("NEST exports", {
  it("writes the annotation sidecar only beside flat files and skips failed pilots once", {
    skip_if_not_installed("gtsummary", "2.6.1")
    entry <- function(items) {
      list(type = "table", items = items,
           def = list(label = "Pilot", type = "Table", dataset = "ADPP"))
    }
    dest <- tempfile("nest-sidecar-")
    on.exit(unlink(dest, recursive = TRUE))
    manifest <- write_tlg_exports(
      list(t_pkpt03_nest = entry(t_pkpt03_nest(nest_adpp_fixture())),
           t_pkpt03_failed = entry(list(all = "Error: NEST pilot requires: gt"))),
      dest, table_formats = c("pdf", "html")
    )
    expect_false(any(grepl("annotations", manifest$file)))
    failed <- manifest[manifest$id == "t_pkpt03_failed", ]
    expect_equal(nrow(failed), 1)
    expect_match(failed$note, "requires: gt", fixed = TRUE)
  })

  it("paginates a full summary with wrapped labels without overflowing the PDF", {
    skip_if_not_installed("gtsummary", "2.6.1")
    params <- c("AUC to Last Nonzero Conc", "AUC to Last Nonzero Conc Norm by Dose",
                "Half-Life Lambda z", "Lambda z", "Lambda z Lower Limit", "Lambda z Span",
                "Last Nonzero Conc", "Last Nonzero Conc Norm by Dose", "Max Conc",
                "Max Conc Norm by Dose", "Number of Points for Lambda z", "R Squared",
                "R Squared Adjusted", "Time of CMAX Observation", "Time of Last Nonzero Conc",
                "Vol Dist Steady State Obs")
    data <- expand.grid(
      PARAM = params,
      TRT01A = c("DrugA 5 mg, bolus", "DrugA 5 mg, Infusion", "DrugA 10 mg, bolus"),
      USUBJID = c("1", "2", "3"), stringsAsFactors = FALSE
    )
    data$PPCAT <- "DrugA"
    data$PPSPEC <- "SERUM"
    data$ATPTREF <- "DOSE 1"
    data$AVAL <- rep(c(1, 2, 4), length.out = nrow(data))
    data$AVALU <- "Hours*ug/mL"
    # The mixture of one- and two-line rows fills the vertical page boundary.
    tables <- t_pkpt03_nest(data, strat_var = c("PARAM", "TRT01A"))
    entry <- list(type = "table", items = tables,
                  def = list(label = "Pilot", type = "Table", dataset = "ADPP"))
    dest <- tempfile("nest-pagination-")
    on.exit(unlink(dest, recursive = TRUE))
    manifest <- write_tlg_exports(list(t_pkpt03_nest = entry), dest, table_formats = "pdf")
    expect_true(all(manifest$status == "ok"))
    expect_false(any(grepl("exceeds", manifest$note)))
  })

  it("writes complete numeric keys, units, annotations and rendered documents into the ZIP tree", {
    skip_if_not_installed("crane", "0.3.2")
    skip_if_not_installed("officer")
    skip_if_not_installed("flextable")
    skip_if_not_installed("readxl")
    summary_data <- nest_adpp_fixture()
    summary_data$AVAL[summary_data$PARAM == "AUC"] <- c(123456.789, 123456.790, 123456.791)
    tables <- t_pkpt03_nest(summary_data, stats = c("n", "Mean", "SD", "Min", "Max"),
                            title = "Study $PPCAT", footnote = "Pilot methods")
    listing_data <- nest_adpp_fixture()
    listing_data$AVAL[1] <- NA_real_
    listing_data$AVALU[1] <- ""
    listings <- l_pkpl01_nest(listing_data, title = "Individual $PPCAT")
    entry <- function(type, items) {
      list(type = type, items = items,
           def = list(label = "Pilot", type = type, dataset = "ADPP"))
    }
    entries <- list(t_pkpt03_nest = entry("table", tables),
                    l_pkpl01_nest = entry("listing", listings))
    dest <- tempfile("nest-export-")
    on.exit(unlink(dest, recursive = TRUE))
    manifest <- write_tlg_exports(entries, dest, table_formats = c("csv", "xlsx", "pdf",
                                                                   "html", "docx"))
    expect_true(all(manifest$status == "ok"), info = paste(manifest$note, collapse = "; "))
    listing_csv <- list.files(file.path(dest, "Listings/csv"), recursive = TRUE, full.names = TRUE)
    flat <- read.csv(listing_csv, check.names = FALSE)
    expect_equal(flat[[get_label("TRT01A", type = "ADPP")]], c("A", "A", "A"))
    expect_equal(flat[["Cmax (mg/L)"]], c(NA, 2, 4))
    workbook <- readxl::read_xlsx(file.path(dest, "Listings/xlsx/pkpl01_nest.xlsx"))
    expect_equal(workbook[["Cmax (mg/L)"]], c(NA, 2, 4))
    expect_equal(workbook[[get_label("USUBJID", type = "ADPP")]], c("1", "2", "3"))
    summary_csv <- list.files(file.path(dest, "Tables/csv"), recursive = TRUE, full.names = TRUE)
    summary_flat <- read.csv(summary_csv, check.names = FALSE)
    expect_equal(summary_flat$Mean[summary_flat$Parameter == "AUC"], 123456.790)
    summary_workbook <- readxl::read_xlsx(file.path(dest, "Tables/xlsx/pkpt03_nest.xlsx"))
    expect_equal(summary_workbook$Mean[summary_workbook$Parameter == "AUC"], 123456.790)
    html <- paste(readLines(file.path(dest, "Tables/html/pkpt03_nest.html")), collapse = "\n")
    expect_match(html, "Study Drug", fixed = TRUE)
    expect_match(html, "Pilot methods", fixed = TRUE)
    expected_values <- c("123456.79", "123456.789", "123456.791")
    for (value in expected_values) expect_match(html, value, fixed = TRUE)
    pdf_frame <- .tlg_pdf_frame(tables[[1]])
    expect_identical(
      unname(unlist(pdf_frame[pdf_frame$Parameter == "AUC", c("Mean", "Min", "Max")])),
      expected_values
    )
    listing_html <- paste(readLines(file.path(dest, "Listings/html/pkpl01_nest.html")),
                          collapse = "\n")
    expect_match(listing_html, "Cmax (mg/L)", fixed = TRUE)
    expect_true("Cmax (mg/L)" %in% names(.tlg_pdf_frame(listings[[1]])))
    listing_doc <- officer::read_docx(file.path(dest, "Listings/docx/pkpl01_nest.docx"))
    expect_true("Cmax (mg/L)" %in% officer::docx_summary(listing_doc)$text)
    doc <- officer::read_docx(file.path(dest, "Tables/docx/pkpt03_nest.docx"))
    word <- officer::docx_summary(doc)
    expect_true(all(c("Study Drug", "Pilot methods", "2.333") %in% word$text))
    expect_true(all(expected_values %in% word$text))
    notes <- read.csv(file.path(dest, "Tables/annotations/pkpt03_nest.annotations.csv"))
    expect_equal(notes$footnote, "Pilot methods")
    pdf <- file.path(dest, "Tables/pdf/pkpt03_nest.pdf")
    expect_gt(file.info(pdf)$size, 1000)
    archive <- tempfile(fileext = ".zip")
    on.exit(unlink(archive), add = TRUE)
    zip::zipr(archive, list.files(dest, full.names = TRUE), root = dest)
    expect_true("Tables/docx/pkpt03_nest.docx" %in% zip::zip_list(archive)$filename)
  })
})
