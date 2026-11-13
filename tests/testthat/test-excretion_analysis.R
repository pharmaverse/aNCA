# Test the production calculation reactive; UI helpers are isolated from the mock session.
local({
  library(shiny)
  library(purrr)
  library(units)
  source(system.file("shiny/modules/tab_nca/excretion_analysis.R", package = "aNCA"),
         local = TRUE)
  selector_label <- function(...) NULL
  reactable_server <- function(...) NULL
}, envir = parent.env(environment()))

excretion_fixture <- function() {
  data <- subset(adnca_example, USUBJID == "S1-01" & PARAM == "DrugA")
  # Give each dose a known weight and express its original dose per kg.
  data$AVALU[data$PCSPEC == "URINE"] <- "ug/mL"
  data$DOSEA <- 5
  data$DOSEU <- "mg/kg"
  data$WTBL <- 2
  data$WTBLU <- "kg"
  PKNCA_create_data_object(data)
}

describe("excretion analysis selections", {
  it("selects uppercase URINE as the initial matrix", {
    selected <- NULL
    testthat::local_mocked_bindings(updateSelectInput = function(session, input_id, ...) {
      if (input_id == "matrix_select") selected <<- list(...)$selected
    }, .package = "shiny")
    shiny::testServer(
      excretion_server,
      args = list(input_pknca_data = reactive(excretion_fixture())),
      {
        session$flushReact()
        expect_identical(selected, "URINE")
      }
    )
  })

  for (empty_input in c("matrix_select", "param_select", "interval_types")) {
    it(paste("does not run PKNCA with an empty", empty_input), {
      notified <- character()
      testthat::local_mocked_bindings(showNotification = function(ui, ...) {
        notified <<- c(notified, as.character(ui))
      }, .package = "shiny")
      testthat::local_mocked_bindings(pk.nca = function(...) stop("must not run NCA"),
                                      .package = "PKNCA")
      shiny::testServer(
        excretion_server,
        args = list(input_pknca_data = reactive(excretion_fixture())),
        {
          values <- list(adjust_bw = FALSE, matrix_select = "URINE", end_time_col = "AEFRLT",
                         param_select = "RCAMINT", interval_types = c("sample", "profile"))
          values[[empty_input]] <- character(0)
          do.call(session$setInputs, values)
          session$setInputs(submit_btn = 1)
          expect_null(analysis_result())
          expect_true(any(grepl("Please select at least one", notified)))
        }
      )
    })
  }
})

describe("excretion calculations", {
  it("calculates amount recovered with weight adjustment unchecked", {
    original <- excretion_fixture()
    shiny::testServer(
      excretion_server,
      args = list(input_pknca_data = reactive(original)),
      {
        session$setInputs(adjust_bw = FALSE, matrix_select = "URINE", end_time_col = "AEFRLT",
                          param_select = "RCAMINT", interval_types = "sample", submit_btn = 1)
        result <- analysis_result()$result
        expect_true(nrow(result) > 0)
        expect_true(is.numeric(result$PPORRES))
        expect_true(all(result$PPTESTCD == "RCAMINT"))
        samples <- original$conc$data %>% filter(PCSPEC == "URINE")
        index <- match(result$start, samples$AFRLT)
        expect_false(anyNA(index))
        expect_equal(result$PPORRES, samples$AVAL[index] * samples$VOLUME[index])
        expect_identical(input_pknca_data(), original)
      }
    )
  })

  it("converts the weight-adjusted recovered fraction to percent", {
    original <- excretion_fixture()
    shiny::testServer(
      excretion_server,
      args = list(input_pknca_data = reactive(original)),
      {
        session$setInputs(adjust_bw = TRUE, matrix_select = "URINE", end_time_col = "AEFRLT",
                          param_select = c("RCAMINT", "FREXINT"),
                          interval_types = "profile", submit_btn = 1)
        result <- analysis_result()$result
        amount <- subset(result, PPTESTCD == "RCAMINT")
        fraction <- subset(result, PPTESTCD == "FREXINT")
        expect_true(nrow(fraction) > 0)
        # conc ug/mL * volume mL -> ug; 5 mg/kg * 2 kg -> 10 mg.
        expect_equal(fraction$PPORRES, amount$PPORRES / 10)
        expect_equal(fraction$PPSTRES, amount$PPORRES / 100)
        expect_true(all(fraction$PPSTRESU == "%"))
        expect_identical(input_pknca_data(), original)
      }
    )
  })
})
