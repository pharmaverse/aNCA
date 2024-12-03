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
                  placeholder = ".csv",
                  buttonLabel = list(icon("folder"), "Upload File..."),
                  accept = c(".csv", ".rds"))
              ),
              reactableOutput(ns("filecontents"))
    ), 
    nav_panel("Mapping and Filters",
              card(
                h3("Data Mapping"),
                br(),
                "The following columns are required for data analysis.
                Please ensure each of these columns has been assigned a corresponding column from your dataset",
                br(),
                uiOutput(ns("column_selectors")),
                actionButton(ns("submit_columns"), "Submit Mapping")
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
    ns <- session$ns
    
    # DATA LOADING -----------------------------------------------------------------
    # Load the dummy ADNCA example for the user as default
    ADNCA <- reactiveVal(
      read.csv(
        system.file("shiny/data/DummyRO_ADNCA.csv", package = "aNCA"),
        na.strings = c("", "NA")
      ) %>%
        mutate(TIME = ifelse(DOSNO == 1, AFRLT, ARRLT))#TODO: Remove this after auc0 merged
    )
    
    # Load data provided by user
    observeEvent(input$local_upload, {
      new_adnca <- switch(
        file_ext(input$local_upload$name),
        csv = read.csv(input$local_upload$datapath, na = c("", "NA")),
        rds = readRDS(input$local_upload$datapath),
        validate("Invalid file type. Only accepted are .csv and .rds")
      )
      
      ADNCA(new_adnca)
    })
    
    output$filecontents <- renderReactable({
      req(ADNCA())
      reactable(
        ADNCA(),
        searchable = TRUE,
        sortable = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        resizable = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        bordered = TRUE,
        height = "98vh"
      )
    })
    
    # Define the required columns and group them into categories
    column_groups <- list(
      "Group Identifiers" = c("STUDYID", "USUBJID", "Grouping Variables"),
      "Sample Variables" = c("ANALYTE", "PCSPEC", "ROUTE", "AVAL"),
      "Dose Variables" = c("DOSNO", "DOSEA", "ADOSEDUR"),
      "Time Variables" = c("AFRLT", "ARRLT", "NFRLT", "NRRLT"),
      "Unit Variables" = c("AVALU", "DOSEU", "RRLTU")
    )
    
    # Define the desired column order
    desired_order <- c("STUDYID", "USUBJID", "ANALYTE",
                       "PCSPEC", "AVAL", "AVALU", "AFRLT",
                       "ARRLT", "NRRLT", "NFRLT", "RRLTU",
                       "ROUTE", "DOSEA", "DOSEU", "DOSNO")
    
    output$column_selectors <- renderUI({
      data <- ADNCA()
      column_names <- names(data)
      
      # Exclude columns specified in desired_order from the choices for "Grouping Variables"
      grouping_variable_choices <- setdiff(column_names, desired_order)
      
      ui_elements <- lapply(names(column_groups), function(group) {
        group_columns <- column_groups[[group]]
        
        group_ui <- lapply(group_columns, function(column) {
          choices <- c("Select Column" = "", column_names)
          if (column == "ADOSEDUR") {
            choices <- c(choices, "NA")
          }
          # Handle choices for Grouping Variables separately
          if (column == "Grouping Variables") {
            choices <- grouping_variable_choices
          }
          
          selectizeInput(
            inputId = ns(paste0("select_", column)),
            label = column,
            choices = choices,
            selected = if (column %in% column_names) column else "",
            multiple = column == "Grouping Variables",
            options = list(
              placeholder = "Select Column"
            )
          )
        })
        
        fluidRow(
          column(12, h4(group)),
          do.call(tagList, group_ui)
        )
      })
      do.call(tagList, ui_elements)
      
    })
    
    # # Add tooltips for each column selector
    # observe({
    #   lapply(names(tooltips), function(column) {
    #     bsTooltip(id = ns(paste0("select_", column)),
    #               title = tooltips[[column]],
    #               placement = "right",
    #               trigger = "hover")
    #   })
    # })
    
    # Global variable to store grouping variables
    grouping_variables <- NULL
    
    # Reactive value for the processed dataset
    processed_data <- reactiveVal(NULL)
    
    # Observe submit button click and update processed_data
    observeEvent(input$submit_columns, {
      req(ADNCA())
      data <- ADNCA()
      
      # Get the selected columns
      selected_cols <- sapply(names(column_groups), function(group) {
        sapply(column_groups[[group]], function(column) {
          input[[(paste0("select_", column))]]
        })
      }, simplify = FALSE)
      
      # Extract and store the "Grouping Variables" column
      grouping_variables <<- selected_cols[["Group Identifiers"]][["Grouping Variables"]]
      
      # Remove "Grouping Variables" from selected columns to prevent renaming
      selected_cols[["Group Identifiers"]] <- selected_cols[["Group Identifiers"]][
        names(selected_cols[["Group Identifiers"]]) != "Grouping Variables"
      ]
      
      # Rename columns
      colnames(data) <- sapply(colnames(data), function(col) {
        for (group in names(selected_cols)) {
          if (col %in% selected_cols[[group]]) {
            return(names(selected_cols[[group]])[which(selected_cols[[group]] == col)])
          }
        }
        return(col)
      })
      
      # Reorder columns based on the desired order
      ordered_data <- data[, c(desired_order, setdiff(names(data), desired_order))]
      
      processed_data(ordered_data)
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
    observeEvent(list(input$submit_filters, processed_data()), {
      # extract filters from reactive #
      applied_filters <- lapply(reactiveValuesToList(filters), \(x) x())
      
      # filter and overwrite data #
      filtered_data <- apply_filters(
        processed_data(), applied_filters
      )
      data(filtered_data)
    }, ignoreInit = FALSE)
    
    # update the data table object with the filtered data #
    output$data_processed <- renderReactable({
      req(data())
      reactable(
        data(),
        searchable = TRUE,
        sortable = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        resizable = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        bordered = TRUE,
        height = "98vh"
      )
    })
    
    return(data)
  })
}