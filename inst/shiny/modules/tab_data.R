#' The data navbar tab loads a dummy data set by default.
#' The user can upload a new data set by clicking the "Browse" button.
#' The data set must be in CSV format and contain the following columns:
#' STUDYID, USUBJID, ANALYTE, PCSPEC, DOSEFRQ, DOSNO, AFRLT, ARRLT, NRRLT, NFRLT,
#' AVAL, AVALU, ROUTE, DOSEA, AGE

source(system.file("/shiny/modules/input_filter.R", package = "aNCA"))

tab_data_ui <- function(id) {
  ns <- NS(id)

  
  navset_pill( 
    nav_panel("Raw Data Upload",
              card(
                "Upload your PK dataset in .csv format",
                # Local upload option
                fileInput(
                  ns("local_upload"),
                  width = "60%",
                  label = NULL,
                  placeholder = "CSV rds",
                  buttonLabel = list(icon("folder"), "Upload File..."),
                  accept = c(".csv", ".rds"))
              ),
              reactableOutput(ns("filecontents"))
              ), 
    nav_panel("Mapping and Filters",
              card(
                "Data Mapping",
                "The following columns are required for data analysis.
                Please ensure each of these columns has been assigned a corresponding column from your dataset",
              ),
              card(
              # Add filter UI elements
              actionButton(ns("add_filter"), "Add filter"),
              tags$div(id = ns("filters")),
              actionButton(ns("submit_filters"), "Submit filters"),
              )
    ),
    nav_panel("Review Data",
              "This is the data set that will be used for the analysis.
              If you want to make any changes, please do so in the Mapping and Filters tab.",
              reactableOutput(ns("data_processed"))
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
        ui = input_filter_ui(session$ns(filter_id), colnames(ADNCA()))
      )

      filters[[filter_id]] <- input_filter_server(filter_id)
    })

    # create reactive value with applied filters
    data <- reactiveVal(NULL)
    observeEvent(list(input$submit_filters, ADNCA()), {
      # extract filters from reactive #
      applied_filters <- lapply(reactiveValuesToList(filters), \(x) x())

      # filter and overwrite data #
      filtered_data <- apply_filters(
        ADNCA(), applied_filters
      )
      data(filtered_data)
    }, ignoreInit = FALSE)

    # update the data table object with the filtered data #
    output$filecontents <- renderReactable({
      req(data())
      reactable(
        data(),
        searchable = TRUE,
        sortable = TRUE,
        highlight = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        bordered = TRUE
        # extensions = "FixedHeader",
        # options = list(
        #   scrollX = TRUE,
        #   scrollY = TRUE,
        #   lengthMenu = list(c(10, 25, -1), c("10", "25", "All")),
        #   fixedHeader = TRUE
        # )
      )
    })

    return(data)
  })
}
