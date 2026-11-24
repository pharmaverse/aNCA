interval_metadata_fixture <- function(specimen, with_volume = TRUE) {
  data <- subset(adnca_example, USUBJID == "S1-01" & PARAM == "DrugA" & PCSPEC == specimen)
  if (!with_volume) data <- data[, setdiff(names(data), c("VOLUME", "VOLUMEU"))]
  pk <- PKNCA_create_data_object(data)
  types <- unique(aNCA:::.derive_study_types(pk)$type)
  params <- if (specimen == "URINE") {
    c("ae", "volpk", "fe", "ertlst", "ertmax")
  } else {
    c("cmax", "half.life")
  }
  PKNCA_update_data_object(
    pk, method = "lin up/log down", selected_analytes = "DrugA",
    selected_profile = unique(data$ATPTREF), selected_pcspec = specimen,
    parameter_selections = setNames(rep(list(params), length(types)), types)
  )
}

describe("collection volume interval metadata", {
  for (specimen in c("SERUM", "URINE")) {
    it(paste("preserves", specimen, "values while omitting unused interval volume"), {
      pk <- interval_metadata_fixture(specimen)
      expect_false("VOLUME" %in% names(pk$intervals))
      expect_true("VOLUME" %in% names(pk$conc$data))
      expect_equal(pk$conc$data$VOLUME,
                   subset(adnca_example, USUBJID == "S1-01" & PARAM == "DrugA" &
                            PCSPEC == specimen)$VOLUME)
      result <- suppressWarnings(PKNCA_calculate_nca(pk))
      expect_gt(nrow(result$result), 0)

      # Retaining the same interval metadata must not change any shared result values.
      retained <- pk
      retained$intervals$VOLUME <- pk$conc$data$VOLUME[1]
      retained$options$keep_interval_cols <- c(pk$options$keep_interval_cols, "VOLUME")
      reference <- suppressWarnings(PKNCA_calculate_nca(retained))
      shared <- intersect(names(result$result), names(reference$result))
      expect_equal(result$result[, shared], reference$result[, shared])
      expect_false("VOLUME" %in% names(result$result))
    })
  }

  it("calculates half-life plots with and without an input volume column", {
    with_volume <- interval_metadata_fixture("SERUM")
    without_volume <- interval_metadata_fixture("SERUM", with_volume = FALSE)
    a <- suppressWarnings(get_halflife_plots(with_volume))
    b <- suppressWarnings(get_halflife_plots(without_volume))
    expect_gt(length(a$plots), 0)
    expect_identical(names(a$plots), names(b$plots))
    expect_equal(suppressWarnings(PKNCA_calculate_nca(with_volume))$result,
                 suppressWarnings(PKNCA_calculate_nca(without_volume))$result)
  })

  it("honors explicit interval volume retention", {
    pk <- PKNCA_create_data_object(subset(adnca_example, USUBJID == "S1-01" &
                                            PARAM == "DrugA" & PCSPEC == "URINE"))
    types <- unique(aNCA:::.derive_study_types(pk)$type)
    updated <- PKNCA_update_data_object(
      pk, method = "lin up/log down", selected_analytes = "DrugA",
      selected_profile = unique(pk$conc$data$ATPTREF), selected_pcspec = "URINE",
      keep_interval_cols = "VOLUME",
      parameter_selections = setNames(rep(list("ae"), length(types)), types)
    )
    expect_true("VOLUME" %in% names(updated$intervals))
    expect_gt(nrow(suppressWarnings(PKNCA_calculate_nca(updated))$result), 0)
  })
})

local({
  library(shiny)
  library(logger)
  library(plotly)
  library(shinyWidgets)
  library(shinyjqui)
  env <- new.env(parent = environment())
  shiny_dir <- system.file("shiny", package = "aNCA")
  source(file.path(shiny_dir, "functions/utils-slope_selector.R"), local = env)
  source(file.path(shiny_dir, "modules/tab_nca/setup/slope_selector.R"), local = env)
  env$page_and_searcher_server <- function(...) list()
  env$event_data <- function(...) NULL
  env$manual_slopes_table_server <- function(...) {
    list(manual_slopes = reactiveVal(NULL), refresh_reactable = reactiveVal(0))
  }

  describe("slope generation error recovery", {
    it("clears failed plots without disconnecting and regenerates after correction", {
      run <- get_halflife_plots
      state <- new.env()
      state$fail <- TRUE
      env$get_halflife_plots <- function(...) {
        if (state$fail) stop("simulated slope failure")
        run(...)
      }
      pk <- interval_metadata_fixture("SERUM")
      data <- reactiveVal(pk)
      shiny::testServer(
        env$slope_selector_server,
        args = list(processed_pknca_data = data, manual_slopes_override = reactive(NULL)),
        {
          session$userData$results <- list(slope_selector = list(old = "stale plot"))
          session$flushReact()
          expect_false(session$isClosed())
          expect_length(plot_outputs(), 0)
          expect_length(session$userData$results$slope_selector, 0)
          expect_null(pknca_data())
          state$fail <- FALSE
          pk$options$min.hl.points <- 4
          data(pk)
          session$flushReact()
          expect_gt(length(plot_outputs()), 0)
          expect_identical(session$userData$results$slope_selector, plot_outputs())
          expect_false(session$isClosed())
        }
      )
    })
  })
})
