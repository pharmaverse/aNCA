##' NCA ZIP Export Module
##'
##' This module provides UI and server logic to export all relevant App results a ZIP file.
##' Users can select which results to export and the output formats for graphics, tables and slides.
##' This module indirectly uses the stored session$userData$results.
##'
##' @param id A character string used to uniquely identify the module.
##' @param res_nca NCA results object (for server).
##' @param settings Settings object (for server).
##' @param grouping_vars Grouping variables (for server).

zip_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      inputId = ns("open_zip_modal"),
      label = "Save",
      icon = icon("download"),
      class = "btn btn-primary",
      style = paste(
        "margin-left: 10px;",
        "padding: 8px 18px;",
        "border-radius: 8px;",
        "box-shadow: 0 2px 6px rgba(0,0,0,0.08);",
        "font-weight: 500;",
        "font-size: 1rem;"
      ),
      title = "Export all selected results as a ZIP archive",
      disabled = TRUE
    )
  )
}

zip_server <- function(id, res_nca, settings, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Enable/disable ZIP export button based on res_nca availability
    observe({
      req(res_nca())
      shinyjs::enable("open_zip_modal")
    })

    # Show ZIP export modal when button is clicked
    observeEvent(input$open_zip_modal, {
      TREE_UI <- create_tree_from_list_names(TREE_LIST)
      showModal(
        modalDialog(
          title = NULL,
          tagList(
            fluidRow(
              column(
                width = 6,
                div(
                  h4("Results to Export"),
                  shinyWidgets::treeInput(
                    inputId = ns("res_tree"),
                    label = NULL,
                    selected = get_tree_leaf_ids(TREE_UI),
                    choices = TREE_UI
                  ),
                  style = "text-align: left;"
                )
              ),
              column(
                width = 6,
                h4("Export Formats"),
                div(
                  selectizeInput(
                    ns("plot_formats"),
                    "Graphics and plots:",
                    choices = c("png", "html"),
                    selected = c("png", "html"),
                    multiple = TRUE
                  ),
                  style = "margin-bottom: 1em;"
                ),
                div(
                  selectizeInput(
                    ns("slide_formats"),
                    "Slide decks:",
                    choices = c("pptx", "qmd"),
                    selected = c("pptx", "qmd"),
                    multiple = TRUE
                  ),
                  style = "margin-bottom: 1em;"
                ),
                div(
                  selectizeInput(
                    ns("table_formats"),
                    "Data tables:",
                    choices = c("rds", "xpt", "csv"),
                    selected = c("rds", "xpt", "csv"),
                    multiple = TRUE
                  ),
                  style = "margin-bottom: 2em;"
                )
              )
            ),
            div(
              downloadButton(ns("download_zip"), "Export ZIP with Results"),
              style = "text-align: center; margin-top: 0.5em;"
            )
          ),
          easyClose = TRUE,
          footer = NULL,
          size = "l"
        )
      )
    })

    output$download_zip <- downloadHandler(
      filename = function() {
        project <- session$userData$project_name()
        if (project == "") {
          study_ids <- unique(res_nca()$data$conc$data[["STUDYID"]])
          study_ids <- study_ids[!is.na(study_ids)]
          if (length(study_ids) > 3) study_ids <- study_ids[1:3]
          project <- if (length(study_ids) > 0) {
            paste0("NCA_", paste(study_ids, collapse = "_"))
          } else {
            "NCA"
          }
        }
        paste0(project, ".zip")
      },
      content = function(fname) {
        tryCatch(
          {
            progress <- shiny::Progress$new(min = 0, max = 1)
            progress$set(message = "Creating exports...")
            progress$inc(0.1)

            output_tmpdir <- file.path(tempdir(), "output")

            prepare_export_files(
              target_dir = output_tmpdir,
              res_nca = res_nca(),
              settings = settings,
              grouping_vars = grouping_vars(),
              input = input,
              session = session,
              progress = progress
            )

            files <- list.files(output_tmpdir, recursive = TRUE)

            wd <- getwd()
            on.exit(setwd(wd), add = TRUE)
            setwd(output_tmpdir)

            progress$inc(0.9)
            progress$set(message = "Creating exports...",
                         detail = "Final touches...")
            zip::zipr(zipfile = fname, files = files, mode = "mirror")

            progress$set(message = "Complete!",
                         detail = "")
            progress$inc(1)
          },
          error = function(e) {
            message("Download Error:")
            message(e$message)
            stop(e)
          }
        )
      }
    )
  })
}

# Define a list with the possible outputs to export as end objects.
# Consider all the zip_server options to create and align accordingly.
TREE_LIST <- list(
  exploration = list(
    individualplot = "",
    meanplot = ""
  ),
  nca_results = list(
    nca_results = "",
    nca_statistics = ""
  ),
  CDISC = list(
    pp = "",
    adpp = "",
    adnca = ""
  ),
  additional_analysis = list(
    matrix_ratios = "",
    excretion_results = ""
  ),
  extras = list(
    results_slides = "",
    r_script = "",
    settings_file = ""
  )
)
