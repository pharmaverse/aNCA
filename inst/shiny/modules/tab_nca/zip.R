#' NCA ZIP Export Module
#'
#' This module provides UI and server logic to export all relevant App results a ZIP file.
#' Users can select which results to export and the output formats for graphics, tables and slides.
#' This module indirectly uses the stored session$userData$results.
#'
#' @param id A character string used to uniquely identify the module.
#' @param res_nca NCA results object (for server).
#' @param adnca_data Reactive with mapped PKNCAdata. Used to enable the Save
#'   button after data mapping, before NCA has been run.
#' @param settings Settings object (for server).
#' @param grouping_vars Grouping variables (for server).

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

zip_server <- function(id, res_nca, adnca_data, settings, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Enable Save button after mapping; full content after NCA
    observe({
      req(adnca_data())
      shinyjs::enable("open_zip_modal")
    })

    nca_available <- reactive({
      tryCatch({
        result <- res_nca()
        !is.null(result)
      }, error = function(e) {
        if (inherits(e, "shiny.silent.error")) FALSE else stop(e)
      })
    })

    # Show ZIP export modal when button is clicked
    observeEvent(input$open_zip_modal, {
      # Build tree based on what's actually available
      tree_items <- .available_tree_items(
        nca_available = isTRUE(nca_available()),
        exploration_names = names(session$userData$results$exploration)
      )
      TREE_UI <- create_tree_from_list_names(tree_items)
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
              style = "width: 100%; text-align: center; margin-top: 0.5em;"
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
          label <- session$userData$study_ids_label()
          project <- if (label != "") paste0("NCA_", label) else "NCA"
        }
        project <- gsub("[^A-Za-z0-9_-]", "_", project)
        paste0(project, ".zip")
      },
      content = function(fname) {
        tryCatch(
          {
            progress <- shiny::Progress$new(min = 0, max = 1)
            progress$set(message = "Creating exports...")
            progress$inc(0.1)

            output_tmpdir <- file.path(tempdir(), "output")
            unlink(output_tmpdir, recursive = TRUE)

            nca_result <- tryCatch(res_nca(), error = function(e) NULL)

            prepare_export_files(
              target_dir = output_tmpdir,
              res_nca = nca_result,
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

# Build the tree of available export items based on current app state.
.available_tree_items <- function(nca_available, exploration_names) {
  items <- list()

  # Only show exploration plots that have been rendered
  avail_plots <- intersect(
    names(TREE_LIST$exploration),
    exploration_names
  )
  if (length(avail_plots) > 0) {
    items$exploration <- TREE_LIST$exploration[avail_plots]
  }

  if (nca_available) {
    items$nca_results <- TREE_LIST$nca_results
    items$CDISC <- TREE_LIST$CDISC
    items$additional_analysis <- TREE_LIST$additional_analysis
    items$extras <- TREE_LIST$extras
  } else {
    items$extras <- TREE_LIST$extras[c("settings_file", "session_info")]
  }

  items
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
    settings_file = "",
    session_info = ""
  )
)
