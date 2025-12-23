#' This module provides a user interface and server function for ratio calculations
#' @param id A character string used to uniquely identify the module.
#' @param title A character string used to title the module.
#' @param select_label1 A character string used to label the first selectInput.
#' @param select_label2 A character string used to label the second selectInput.
#' @param multiple A logical value indicating if multiple selections are allowed.
non_nca_ratio_ui <- function(id, title, select_label1, select_label2) {
  ns <- NS(id)

  tagList(
    card(
      style = "height: 33vh;",
      card_header(paste0(title, " Setup")),
      card_body(
        fluidRow(
          column(
            6,
            actionButton(ns("add_pair"), "Add Ratio Pair",
                         icon = icon("plus"), class = "add-pair-btn")
          ),
          column(
            6,
            selectInput(
              ns("summary_groups"),
              "Summarise By:",
              choices = NULL,
              multiple = TRUE
            )
          )
        ),
        # Placeholder for dynamically added ratio pair selectors
        div(id = ns("ratio_pairs_placeholder"))
      )
    ),
    card(
      height = "60vh",
      card_header(paste0(title, " Results")),
      card_body(
        reactable_ui(ns("results"))
      )
    )
  )
}

#' Server function for ratio calculations
#' @param id A character string used to uniquely identify the module.
#' @param data A PKNCAdata object
#' @param grouping_vars A reactiveVal containing a character vector of grouping variables
#' @param func The function used to perform the ratio calculation

non_nca_ratio_server <- function(id, data, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pair_counter <- reactiveVal(0)
    selections <- reactiveValues()

    spec_options <- reactive({
      req(data())
      unique(data()$conc$data$PCSPEC)
    })

    id_groups <- reactive({
      req(data())
      data()$conc$columns$groups %>%
        purrr::list_c() %>%
        append("ATPTREF") %>%
        purrr::keep(\(col) {
          !is.null(col) && col != "PCSPEC" && length(unique(data()$conc$data[[col]])) > 1
        })
    })

    ratio_groups <- reactive({
      req(data())
      c(grouping_vars(), id_groups(),
        data()$dose$columns$dose, data()$dose$columns$time.nominal,
        data()$dose$columns$route)
    })

    observeEvent(ratio_groups(), {
      updateSelectInput(session, "summary_groups", choices = ratio_groups())
    })

    # Add the first pair automatically when data is loaded
    observeEvent(data(), {
      req(pair_counter() == 0)
      # Manually trigger the event for adding a pair
      input$add_pair
    }, once = TRUE, ignoreInit = TRUE, priority = 1)

    # Add a new pair of selectInputs when the button is clicked
    observeEvent(input$add_pair, {
      count <- pair_counter() + 1
      pair_counter(count)

      pair_id <- paste0("pair_", count)
      spec1_id <- paste0("spec1_", count)
      spec2_id <- paste0("spec2_", count)
      remove_id <- paste0("remove_", count)

      ui <- div(
        class = "ratio-pair-row",
        id = ns(pair_id),
        selectInput(
          ns(paste0("spec1_", count)),
          "Numerator(s)",
          choices = spec_options(),
          multiple = TRUE
        ),

        selectInput(
          ns(paste0("spec2_", count)),
          "Denominator(s)",
          choices = spec_options(),
          multiple = TRUE
        ),
        div(
          class = "ratio-remove-btn-wrapper",
          actionButton(ns(paste0("remove_", count)), "", icon = icon("trash-alt"))
        )
      )

      insertUI(
        selector = paste0("#", ns("ratio_pairs_placeholder")),
        where = "beforeEnd",
        ui = ui
      )

      observeEvent(input[[spec1_id]], {
        selections[[spec1_id]] <- input[[spec1_id]]
      }, ignoreNULL = FALSE)
      observeEvent(input[[spec2_id]], {
        selections[[spec2_id]] <- input[[spec2_id]]
      }, ignoreNULL = FALSE)

      # Observer to remove the UI for the specific pair
      observeEvent(input[[remove_id]], {
        removeUI(selector = paste0("#", ns(pair_id)))
        selections[[spec1_id]] <- NULL
        selections[[spec2_id]] <- NULL
      }, once = TRUE, ignoreInit = TRUE, autoDestroy = TRUE)

    })

    results <- reactive({
      req(pair_counter() > 0)
      # Trigger this reactive when any selection changes
      reactiveValuesToList(selections)

      all_ratios <- purrr::map_dfr(seq_len(pair_counter()), function(i) {
        spec1 <- selections[[paste0("spec1_", i)]]
        spec2 <- selections[[paste0("spec2_", i)]]

        if (length(spec1) > 0 && length(spec2) > 0) {
          filtered_samples <- data()$conc$data %>% filter(PCSPEC %in% c(spec1, spec2))
          multiple_matrix_ratios(
            data = filtered_samples,
            matrix_col = "PCSPEC",
            conc_col = data()$conc$columns$concentration,
            units_col = "AVALU",
            groups = ratio_groups(),
            spec1 = spec1,
            spec2 = spec2
          )
        } else {
          NULL
        }
      })

      if (nrow(all_ratios) == 0) return(NULL)
      return(all_ratios)
    }) %>%
      debounce(500) # Wait 500ms after user stops changing inputs

    full_output <- reactive({
      req(results())

      summary <- results() %>%
        group_by(across(all_of(input$summary_groups)), Ratio_Type) %>%
        summarise(
          Geomean_Ratio = round(exp(mean(log(Ratio), na.rm = TRUE)), 3),
          N = n(),
          .groups = "drop"
        )
      results() %>%
        bind_rows(summary) %>%
        arrange(across(all_of(input$summary_groups)), Ratio_Type)
    })

    reactable_server(
      "results",
      full_output,
      download_buttons = c("csv", "xlsx"),
      file_name = function() paste0("Ratios_result_", Sys.Date()),
      defaultPageSize = 10,
      showPageSizeOptions = TRUE,
      pageSizeOptions = reactive(c(10, 50, nrow(full_output()))),
      style = list(fontSize = "0.75em")
    )

    # Save the results in the output folder
    observeEvent(results(), {
      session$userData$results$additional_analysis$matrix_ratios <- full_output()
    })
  })
}
