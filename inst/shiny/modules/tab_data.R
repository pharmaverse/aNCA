#' The data navbar tab loads a dummy data set by default.
#' The user can upload a new data set by clicking the "Browse" button.
#' The data set must be in CSV format and contain the following columns:
#' STUDYID, USUBJID, ANALYTE, PCSPEC, DOSEFRQ, DOSNO, AFRLT, ARRLT, NRRLT, NFRLT,
#' AVAL, AVALU, ROUTE, DOSEA, AGE


tab_data_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      actionButton(ns("login"), "Login and Select File Path"),
      br(), br(), br(),
      # Local upload option
      fileInput(
        ns("local_upload"),
        width = "60%",
        label = NULL,
        placeholder = "CSV rds",
        buttonLabel = list(icon("folder"), "Upload File..."),
        accept = c(".csv", ".rds")
      ),
      br(),
      # Add filter UI elements
      actionButton(ns("add_filter"), "Add Filter"),
      tags$div(id = ns("filters")),
      br(), br(),
    ),
    mainPanel(
      DTOutput(ns("filecontents"))
    )
  )

}

tab_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # DATA LOADING -----------------------------------------------------------------
    # Load the dummy ADNCA example for the user as default
    ADNCA <- reactiveVal(
      read.csv(
        system.file("shiny/data/DummyRO_ADNCA.csv", package = "aNCA"),
        na.strings = c("", "NA")
      ) %>%
        # Make sure PCSTRESC is numeric and TIME is derived from the first dose
        mutate(TIME = ifelse(DOSNO == 1, AFRLT, ARRLT))
    )

    # Load data provided by user
    observeEvent(input$local_upload, {
      new_adnca <- switch(
        file_ext(input$local_upload$name),
        csv = read.csv(input$local_upload$datapath, na = c("", "NA")),
        rds = readRDS(input$local_upload$datapath),
        validate("Invalid file type. Only accepted are .csv and .rds")
      )

      # TODO: Make sure the dataset includes all the neded column names

      # Disconsider events that do not contain drug information (i.e, Follow-up visits)
      new_adnca <- new_adnca %>%
        filter(
          if ("AVISIT" %in% names(new_adnca) && !all(is.na(AVISIT))) {
            tolower(gsub("[^a-zA-Z]", "", AVISIT)) != "followup"
          } else {
            TRUE
          }
        )

      # Make sure AVAL is numeric and TIME is derived from the first dose
      new_adnca <- new_adnca  %>%
        mutate(
          AVAL = {
            if ("AVAL" %in% names(new_adnca)) {
              as.numeric(AVAL)
            } else {
              ifelse(
                PCSTRESC %in% c("BLQ", "Negative", "negative", "NEGATIVE"),
                0,
                as.numeric(PCSTRESC)
              )
            }
          }
        ) %>%
        mutate(
          TIME = ifelse(DOSNO == 1, AFRLT, ARRLT),
          NDOSEDUR = as.numeric(NDOSEDUR),
          ADOSEDUR = as.numeric(ADOSEDUR)
        )

      ADNCA(new_adnca)
    })

    # Handle user-provided filters
    filters <- reactiveValues()

    observeEvent(input$add_filter, {
      # Create a unique ID for each filter
      filter_id <- paste0("filter_", input$add_filter)

      # Insert a new filter UI
      insertUI(
        selector = paste0("#", session$ns("filters")),
        ui = create_filter(filter_id, ADNCA())
      )

      # Observe the "Remove Filter" button click for the newly created filter
      observeEvent(input[[paste0(filter_id, "_remove")]], {
        filters[[filter_id]] <- NULL
        # Remove the filter UI
        removeUI(selector = paste0("#", filter_id))
        # Remove the filter from the reactiveValues object
        filters[[filter_id]] <- NULL
      }, ignoreInit = TRUE, once = TRUE)

      observe({
        column <- input[[paste0(filter_id, "_column")]]
        condition <- input[[paste0(filter_id, "_condition")]]
        value <- input[[paste0(filter_id, "_value")]]

        if ((input[[paste0(filter_id, "_confirm")]] == TRUE) &&
              !is.null(column) &&
              !is.null(condition) &&
              !is.null(value)) {
          filters[[filter_id]] <- list(column = column, condition = condition, value = value)
        } else {
          filters[[filter_id]] <- NULL
        }
      })
    })

    # Include keyboard limits for the settings GUI display
    ## Define a function that simplifies the action
    # TODO: ???
    input_limit <- function(input_id, max = Inf, min = -Inf, update_fun = updateNumericInput) {
      observeEvent(input[[input_id]], {
        if (input[[input_id]] < min & !is.na(input[[input_id]])) {
          update_fun(session, input_id, "", value = min)
        }
        if (input[[input_id]] > max & !is.na(input[[input_id]])) {
          update_fun(session, input_id, "", value = max)
        }
      })
    }

    ## Keyboard limits for the setting thresholds
    input_limit("adj.r.squared_threshold", max = 1, min = 0)
    input_limit("aucpext.obs_threshold", max = 100, min = 0)
    input_limit("aucpext.pred_threshold", max = 100, min = 0)
    input_limit("span.ratio_threshold", min = 0)

    ## Keyboard limits for the dynamically created partial AUC ranges
    # TODO: ????
    observe({
      inputs_list <- reactiveValuesToList(input)
      for (input_id in names(inputs_list)) {
        if (startsWith(input_id, "timeInput")) {
          local({
            my_input_id <- input_id
            observeEvent(input[[my_input_id]], {
              if (is.numeric(input[[my_input_id]]) && input[[my_input_id]] < 0) {
                input_limit(my_input_id, min = 0)
              }
            })
          })
        }
      }
    })


    # create reactive value with applied filters
    data <- reactive({
      apply_filters(
        ADNCA(),
        reactiveValuesToList(filters)
      )
    })


    # update the data table object with the filtered data #
    output$filecontents <- DT::renderDataTable({
      req(data())
      DT::datatable(
        data(),
        options = list(
          scrollX = TRUE,
          scrollY = TRUE,
          lengthMenu = list(c(10, 25, -1), c("10", "25", "All"))
        )
      )
    })

    return(data)
  })
}