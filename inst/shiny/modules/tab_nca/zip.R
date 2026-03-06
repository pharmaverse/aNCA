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
    ),
    shinyjs::hidden(
      downloadButton(ns("download_zip"), label = "")
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
              actionButton(ns("confirm_export"), "Export ZIP with Results",
                           class = "btn btn-primary"),
              style = "text-align: center; margin-top: 0.5em;"
            )
          ),
          easyClose = TRUE,
          footer = NULL,
          size = "l"
        )
      )
    })

    observeEvent(input$confirm_export, {
      slides_selected <- "results_slides" %in% input$res_tree &&
        length(input$slide_formats) > 0

      if (!slides_selected) {
        removeModal()
        shinyjs::click(ns("download_zip"))
        return()
      }

      # Compute available sections dynamically
      fixed_sections <- list(
        list(id = "meanplot",   label = "Mean Plots"),
        list(id = "statistics", label = "Summary Statistics"),
        list(id = "ind_params", label = "PK Parameters"),
        list(id = "ind_plots",  label = "Individual Plots")
      )

      additional_analysis <- session$userData$results$additional_analysis
      if (is.null(additional_analysis)) additional_analysis <- list()
      additional_available <- Filter(
        function(x) is.data.frame(x) && nrow(x) > 0,
        additional_analysis
      )
      additional_sections <- lapply(names(additional_available), function(name) {
        list(id = name, label = gsub("_", " ", tools::toTitleCase(name)))
      })

      all_sections <- c(fixed_sections, additional_sections)
      all_ids    <- vapply(all_sections, `[[`, character(1), "id")
      all_labels <- setNames(vapply(all_sections, `[[`, character(1), "label"), all_ids)

      removeModal()
      showModal(modalDialog(
        title = "Customise Slide Contents",
        checkboxGroupInput(
          ns("slide_section_choices"),
          label = "Select sections to include in the slide deck:",
          choices = all_labels,
          selected = all_ids
        ),
        footer = tagList(
          downloadButton(ns("download_zip_configured"), "Export"),
          modalButton("Cancel")
        ),
        easyClose = TRUE,
        size = "m"
      ))
    })

    .zip_filename <- function() {
      project <- session$userData$project_name()
      if (project == "") {
        label <- session$userData$study_ids_label()
        project <- if (label != "") paste0("NCA_", label) else "NCA"
      }
      project <- gsub("[^A-Za-z0-9_-]", "_", project)
      paste0(project, ".zip")
    }

    .run_export <- function(fname, slide_sections = NULL) {
      tryCatch(
        {
          progress <- shiny::Progress$new(min = 0, max = 1)
          progress$set(message = "Creating exports...")
          progress$inc(0.1)

          output_tmpdir <- file.path(tempdir(), "output")
          unlink(output_tmpdir, recursive = TRUE)

          prepare_export_files(
            target_dir = output_tmpdir,
            res_nca = res_nca(),
            settings = settings,
            grouping_vars = grouping_vars(),
            input = input,
            session = session,
            progress = progress,
            slide_sections = slide_sections
          )

          files <- list.files(output_tmpdir, recursive = TRUE)
          wd <- getwd()
          on.exit(setwd(wd), add = TRUE)
          setwd(output_tmpdir)

          progress$inc(0.9)
          progress$set(message = "Creating exports...", detail = "Final touches...")
          zip::zipr(zipfile = fname, files = files, mode = "mirror")

          progress$set(message = "Complete!", detail = "")
          progress$inc(1)
        },
        error = function(e) {
          message("Download Error: ", e$message)
          stop(e)
        }
      )
    }

    output$download_zip <- downloadHandler(
      filename = .zip_filename,
      content = function(fname) {
        .run_export(fname, slide_sections = NULL)
      }
    )

    output$download_zip_configured <- downloadHandler(
      filename = .zip_filename,
      content = function(fname) {
        removeModal()
        .run_export(fname, slide_sections = input$slide_section_choices)
      }
    )
  })
}

# Define a list with the possible outputs to export as end objects.
# Consider all the zip_server options to create and align accordingly.
TREE_LIST <- list(
  exploration = list(
    individualplot = "",
    meanplot = "",
    qcplot = ""
  ),
  nca_results = list(
    nca_pkparam = "",
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
