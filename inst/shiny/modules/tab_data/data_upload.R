#' Module responsible for loading and validating raw ADNCA data.
#'
#' @details
#' Upon startup, when no data is provided by the user, the module will return dummy data
#' available with the application. Upon upload, user data will be loaded from .csv or .rds files.
#'
#' @param id ID of the module.
#'
#' @returns A reactive with raw adnca data as provided by the user (or dummy dataset).

data_upload_ui <- function(id) {
  ns <- NS(id)

  div(
    stepper_ui("Upload"),
    div(
      class = "upload-container",
      id = ns("upload_container"),
      p("Upload your PK dataset."),
      fileInput(
        ns("data_upload"),
        width = "50%",
        label = NULL,
        placeholder = paste(names(aNCA:::readers), collapse = ", "),
        buttonLabel = list(icon("folder"), "Upload File...")
      ),
      uiOutput(ns("file_loading_message"))
    ),
    withSpinner(
      reactableOutput(ns("data_display"))
    )
  )
}

data_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    #' Dummy data is automatically loaded on startup if no data path is provided
    DUMMY_DATA <- read.csv(
      system.file("shiny/data/Dummy_complex_data.csv", package = "aNCA"),
      na.strings = c("", "NA")
    )

    #' Display file loading error if any issues arise
    file_loading_error <- reactiveVal(NULL)
    output$file_loading_message <- renderUI({
      if (is.null(file_loading_error())) {
        p("")
      } else {
        p(file_loading_error(), class = "error-string")
      }
    })

    datapath <- getOption("aNCA.datapath", NULL)

    raw_data <- (
      reactive({
        #' If no data is provided by the user, load dummy data
        if (is.null(input$data_upload$datapath) & is.null(datapath)) {
          DUMMY_DATA
        } else {
          if (is.null(input$data_upload$datapath)) {
            log_info("Data upload module initialized with datapath: ", datapath)
            final_datapath <- datapath
          } else {
            final_datapath <- input$data_upload$datapath
          }

          df <- tryCatch({
            file_loading_error(NULL)
            read_pk(final_datapath)
          }, error = function(e) {
            file_loading_error(e$message)
          })

          if (is.null(file_loading_error())) {
            log_success("User data loaded successfully.")
            df
          } else {
            log_error("Error loading user data: ", file_loading_error())
            DUMMY_DATA
          }
        }
      }) |>
        bindEvent(input$data_upload, ignoreNULL = FALSE)
    )

    output$data_display <- renderReactable({
      req(raw_data())
      reactable(
        raw_data(),
        searchable = TRUE,
        sortable = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        resizable = TRUE,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        height = "70vh",
        class = "reactable-table"
      )
    })

    raw_data
  })
}
