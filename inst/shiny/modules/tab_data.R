#' Module handling pre-processing of data.
#'
#' @details
#' Handles user upload or dummy data, filtering, mapping and reviewing. The general pipeline for
#' the module is:
#' 1. Upload raw adnca data (or return a dummy dataset)
#' 2. Filter the raw data based on user input
#' 3. Process the data based on column mapping specifications
#' The module also allows the user to review the data after performing filtering and mapping -
#' the processed data will go further into the analysis pipeline.
#'
#' @returns list containing two reactives:
#'            - data -> processed data for further analysis
#'            - grouping_variables -> grouping variables specified by the user.

tab_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    div(
      class = "data-tab-container",
      div(
        class = "data-tab-content-container",
        div(
          class = "data-tab-content",
          navset_pill(
            id = ns("data_navset"),
            nav_panel(
              "Upload",
              data_upload_ui(ns("raw_data"))
            ),
            nav_panel(
              "Filtering",
              data_filtering_ui(ns("data_filtering"))
            ),
            nav_panel(
              "Mapping",
              data_mapping_ui(ns("column_mapping"))
            ),
            nav_panel("Preview",
              id = ns("data_navset-review"),
              div(
                stepper_ui("Preview"),
                div(
                  uiOutput(ns("processed_data_message")),
                  reactable_ui(ns("data_processed"))
                )
              )
            )
          )
        )
      ),
      div(
        class = "data-tab-btns-container",
        # Left side: Restart button
        actionButton(ns("restart"), "Restart"),

        # Right side: Previous and Next buttons
        div(
          class = "nav-btns",
          actionButton(ns("prev_step"), "Previous", disabled = TRUE),
          actionButton(ns("next_step"), "Next", , class = "btn-primary")
        )
      )
    )
  )
}

tab_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    steps <- c("upload", "filtering", "mapping", "preview")
    step_labels <- c("Upload", "Filtering", "Mapping", "Preview")
    data_step <- reactiveVal("upload")

    observe({
      current <- data_step()
      if (current == steps[1]) {
        shinyjs::disable("prev_step")
      } else {
        shinyjs::enable("prev_step")
      }

      if (current == steps[length(steps)]) {
        shinyjs::disable("next_step")
      } else {
        shinyjs::enable("next_step")
      }
    })

    observeEvent(input$restart, {
      log_info("Application restarting...")
      session$reload()
    })

    observeEvent(input$next_step, {
      current <- data_step()
      idx <- match(current, steps)
      if (!is.na(idx) && idx < length(steps)) {
        data_step(steps[idx + 1])
      }
      updateTabsetPanel(session, "data_navset", selected = step_labels[idx + 1])
    })

    observeEvent(input$prev_step, {
      current <- data_step()
      idx <- match(current, steps)
      if (!is.na(idx) && idx > 1) {
        data_step(steps[idx - 1])
      }
      updateTabsetPanel(session, "data_navset", selected = step_labels[idx - 1])
    })

    #' Load raw ADNCA data
    adnca_raw <- data_upload_server("raw_data")

    #' Filter data
    adnca_filtered <- data_filtering_server("data_filtering", adnca_raw)

    # Call the column mapping module
    column_mapping <- data_mapping_server(
      id = "column_mapping",
      adnca_data = adnca_filtered
    )

    #' Reactive value for the processed dataset
    processed_data <- column_mapping$processed_data
    observeEvent(processed_data(), {
      req(processed_data())
      data_step("preview")
      updateTabsetPanel(session, "data_navset", selected = "Preview")
    })

    #' Global variable to store grouping variables
    grouping_variables <- column_mapping$grouping_variables

    output$processed_data_message <- renderUI({
      tryCatch(
        {
          req(processed_data())
          p(
            "This is the data set that will be used for the analysis.
          If you would like to make any changes please return to the previous tabs."
          )
        },
        error = function(e) {
          p("Please map your data in the 'Column Mapping' section before reviewing it.")
        }
      )
    })

    # Update the data table object with the filtered data
    reactable_server(
      "data_processed",
      processed_data,
      columns = generate_col_defs,
      compact = TRUE,
      style = list(fontSize = "0.75em"),
      height = "50vh",
      showPageSizeOptions = TRUE,
      pageSizeOptions = reactive(c(10, 25, 50, 100, nrow(processed_data()))),
    )

    list(
      data = processed_data,
      grouping_variables = grouping_variables
    )
  })
}
