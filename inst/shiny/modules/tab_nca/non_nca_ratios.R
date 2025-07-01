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
          selectInput(
            ns("selected_spec1"),
            select_label1,
            choices = NULL,
            multiple = TRUE
          ),
          selectInput(
            ns("selected_spec2"),
            select_label2,
            choices = NULL,
            multiple = TRUE
          ),
          selectInput(
            ns("summary_groups"),
            "Summarise By:",
            choices = NULL,
            multiple = TRUE
          )
        ),
      )
    ),
    card(
      height = "60vh",
      card_header(paste0(title, " Results")),
      card_body(
        DTOutput(ns("results"))
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

    id_groups <- reactive({
      req(data())
      data()$conc$columns$groups %>%
        purrr::list_c() %>%
        append("NCA_PROFILE") %>%
        purrr::keep(\(col) {
          !is.null(col) && col != "PCSPEC" && length(unique(data()$conc$data[[col]])) > 1
        })
    })
    # Update select inputs dynamically
    observeEvent(data(), {
      spec_options <- unique(data()$conc$data$PCSPEC)

      ratio_groups <- c(grouping_vars(), id_groups(),
                        data()$dose$columns$dose, data()$dose$columns$time.nominal,
                        data()$dose$columns$route)

      updateSelectInput(session, "selected_spec1", choices = spec_options)
      updateSelectInput(session, "selected_spec2", choices = spec_options)
      updateSelectInput(session, "summary_groups", choices = ratio_groups)
    })

    # Filter & prepare data for calculation
    filtered_samples <- reactive({
      req(input$selected_spec1, input$selected_spec2)
      data()$conc$data %>%
        filter(PCSPEC %in% c(input$selected_spec1, input$selected_spec2))
    })

    # Perform Calculation on Submit
    results <- reactive({
      req(filtered_samples())

      spec1 <- input$selected_spec1
      spec2 <- input$selected_spec2

      ratio_groups <- c(grouping_vars(), id_groups(),
                        data()$dose$columns$dose, data()$dose$columns$time.nominal,
                        data()$dose$columns$route)

      # Calculate ratios
      multiple_matrix_ratios(
        data = filtered_samples(),
        matrix_col = "PCSPEC",
        conc_col = data()$conc$columns$concentration,
        units_col = "AVALU",
        groups = ratio_groups,
        spec1 = spec1,
        spec2 = spec2
      )
    })

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

    # Display results
    output$results <- renderDT({
      req(full_output())
      datatable(
        full_output(),
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          fixedHeader = TRUE,
          dom = "Bfrtip",
          buttons = list(
            list(
              extend = "copy",
              title = paste0("Ratios_result", Sys.Date())
            ),
            list(
              extend = "csv",
              filename = paste0("Ratios_result_", Sys.Date())
            )
          )
        ),
      )
    })

  })
}
