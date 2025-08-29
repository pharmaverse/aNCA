download_code_ui <- function(id) {
  ns <- NS(id)
  actionButton(ns("show_modal"), NULL, icon = icon("code"))
}

download_code_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$show_modal, {
      showModal(modalDialog(
        title = "Download Code",
        div(
          class = "download-code-modal",
          navset_card_pill(
            nav_panel(
              "Data pre-processing",
              downloadButton(session$ns("download_preprocessing_code"), "Download"),
              verbatimTextOutput(session$ns("preprocessing_code"), placeholder = TRUE)
            )
          )
        ),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "xl"
      ))
    })

    preprocessing_code_parsed <- reactive({
      template_path <- system.file(
        "shiny",
        "resources",
        "data_preprocessing_script_template.R",
        package = "aNCA"
      )
      .parse_code_template(template_path, session)
    }) |>
      bindEvent(
        session$userData$mapping(),
        session$userData$applied_filters()
      )
    output$preprocessing_code <- renderText({
      preprocessing_code_parsed()
    })

    output$download_preprocessing_code <- downloadHandler(
      filename = function() {
        "data_preprocessing.R"
      },
      content = function(file) {
        writeLines(preprocessing_code_parsed(), con = file)
      }
    )
  })
}

.parse_code_template <- function(template_path, session) {
  deparse_pattern <- "(deparse1\\((?:[^()]|\\([^()]*\\))*\\))"

  template_path %>%
    readLines() %>%
    purrr::keep(\(line) !grepl("#'", line, fixed = TRUE)) %>%
    paste(collapse = "\n") %>%
    gsub(deparse_pattern, "{\\1}", ., perl = TRUE) %>%
    glue::glue()
}