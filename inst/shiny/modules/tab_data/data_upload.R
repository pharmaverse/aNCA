#' Module responsible for loading and validating raw ADNCA data.
#' Upon startup, when no data is provided by the user, the module will return dummy data
#' available with the application.
#'
#' Upon upload, user data will be loaded from .csv or .rds files.

data_upload_ui <- function(id) {
  ns <- NS(id)

  fileInput(
    ns("data_upload"),
    width = "60%",
    label = NULL,
    placeholder = ".csv",
    buttonLabel = list(icon("folder"), "Upload File..."),
    accept = c(".csv", ".rds")
  )
}

data_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      #' if no data is provided by the user, load dummy data
      if (is.null(input$data_upload$datapath)) {
        read.csv(
          system.file("shiny/data/Dummy_complex_data.csv", package = "aNCA"),
          na.strings = c("", "NA")
        )
      } else {
        df <- switch(
          file_ext(input$data_upload$name),
          csv = read.csv(input$data_upload$datapath, na = c("", "NA")),
          rds = readRDS(input$data_upload$datapath),
          validate("Invalid file type. Only accepted are .csv and .rds")
        )

        log_success("User data loaded successfully.")
        df
      }
    }) |>
      bindEvent(input$data_upload, ignoreNULL = FALSE)
  })
}
