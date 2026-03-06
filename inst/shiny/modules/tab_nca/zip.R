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
            )
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("confirm_export"),
              label = "Export ZIP with Results",
              icon  = icon("download"),
              class = "btn btn-primary"
            )
          ),
          easyClose = FALSE,
          size = "l"
        )
      )
    })

    slide_types_rv <- reactiveVal(list())

    observeEvent(input$confirm_export, {
      slides_selected <- "results_slides" %in% input$res_tree &&
        length(input$slide_formats) > 0

      if (!slides_selected) {
        removeModal()
        showModal(modalDialog(
          title = NULL,
          p("Ready to export. No slide customisation needed."),
          footer = tagList(
            downloadButton(ns("download_zip"), "Export ZIP"),
            modalButton("Cancel")
          ),
          easyClose = TRUE,
          size = "s"
        ))
        return()
      }

      additional_analysis <- session$userData$results$additional_analysis
      if (is.null(additional_analysis)) additional_analysis <- list()
      additional_available <- Filter(
        function(x) is.data.frame(x) && nrow(x) > 0,
        additional_analysis
      )
      additional_sections <- lapply(names(additional_available), function(name) {
        list(id = name, label = gsub("_", " ", tools::toTitleCase(name)))
      })

      slide_types <- list(
        list(
          id       = "individual",
          label    = "Individual Slides",
          sections = list(
            list(id = "ind_plots",  label = "Individual Plots"),
            list(id = "ind_params", label = "PK Parameters")
          )
        ),
        list(
          id       = "summary",
          label    = "Summary Slides",
          sections = list(
            list(id = "meanplot",   label = "Mean Plots"),
            list(id = "statistics", label = "Summary Statistics")
          )
        )
      )
      if (length(additional_sections) > 0) {
        slide_types <- c(slide_types, list(
          list(
            id       = "additional",
            label    = "Additional Analysis",
            sections = additional_sections
          )
        ))
      }
      slide_types_rv(slide_types)

      slide_tree <- lapply(seq_along(slide_types), function(i) {
        type <- slide_types[[i]]
        parent_id <- paste0("slides_", i)
        children <- lapply(seq_along(type$sections), function(j) {
          sec <- type$sections[[j]]
          list(text = sec$label, id = paste0(parent_id, "_", j))
        })
        list(text = type$label, id = parent_id, children = children)
      })

      all_leaf_ids <- unlist(lapply(slide_tree, function(node) {
        vapply(node$children, `[[`, character(1), "id")
      }))

      available_params <- sort(unique(res_nca()$result$PPTESTCD))

      param_meta <- metadata_nca_parameters[metadata_nca_parameters$PPTESTCD %in% available_params, ]
      params_by_type <- split(param_meta, param_meta$TYPE)
      virtual_choices <- lapply(names(params_by_type), function(type_name) {
        df <- params_by_type[[type_name]]
        options_list <- lapply(seq_len(nrow(df)), function(i) {
          list(
            label       = as.character(df$PPTESTCD[i]),
            value       = as.character(df$PPTESTCD[i]),
            description = as.character(df$PPTEST[i])
          )
        })
        list(label = type_name, options = options_list)
      })
      default_selected <- intersect(DEFAULT_STATS_PARAMETERS, available_params)

      removeModal()
      showModal(modalDialog(
        title = "Customise Slide Contents",
        p(
          style = "color: #555; font-size: 0.92em; margin-bottom: 1em;",
          "Select which slide sections to include and which PK parameters to show in the individual and summary slides. ",
          "All selections below apply only to the exported slide deck."
        ),
        fluidRow(
          column(
            width = 6,
            div(
              h4("Slide Sections"),
              shinyWidgets::treeInput(
                inputId  = ns("slide_sections_tree"),
                label    = NULL,
                choices  = slide_tree,
                selected = all_leaf_ids
              ),
              style = "text-align: left;"
            )
          ),
          column(
            width = 6,
            div(
              h4("PK Parameters", style = "margin-bottom: 0.25em;"),
              p(style = "color: #777; font-size: 0.85em; margin-top: 0; margin-bottom: 0.75em;",
                "Parameters included in the slide tables")
            ),
            shinyWidgets::virtualSelectInput(
              inputId                  = ns("slide_ind_params"),
              label                    = "Individual slide:",
              choices                  = virtual_choices,
              selected                 = default_selected,
              multiple                 = TRUE,
              search                   = TRUE,
              showSelectedOptionsFirst = TRUE,
              hasOptionDescription     = TRUE,
              showValueAsTags          = TRUE,
              width                    = "100%"
            ),
            shinyWidgets::virtualSelectInput(
              inputId                  = ns("slide_summary_params"),
              label                    = "Summary slide:",
              choices                  = virtual_choices,
              selected                 = default_selected,
              multiple                 = TRUE,
              search                   = TRUE,
              showSelectedOptionsFirst = TRUE,
              hasOptionDescription     = TRUE,
              showValueAsTags          = TRUE,
              width                    = "100%"
            ),
            helpText(
              icon("circle-info"),
              "Only parameters calculated in this NCA run are available for selection.",
              "If a parameter you need is missing, return to the NCA tab, include it in the parameter",
              "selection, and re-run the analysis before exporting."
            )
          )
        ),
        footer = tagList(
          div(
            style = "display: flex; justify-content: space-between; width: 100%;",
            actionButton(ns("back_to_export"), "Back", icon = icon("arrow-left")),
            div(
              modalButton("Cancel"),
              downloadButton(ns("download_zip_configured"), "Export")
            )
          )
        ),
        easyClose = FALSE,
        size = "l"
      ))
    })

    observeEvent(input$back_to_export, {
      TREE_UI <- create_tree_from_list_names(TREE_LIST)
      removeModal()
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
            )
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("confirm_export"),
              label = "Export ZIP with Results",
              icon  = icon("download"),
              class = "btn btn-primary"
            )
          ),
          easyClose = FALSE,
          size = "l"
        )
      )
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

    .run_export <- function(fname, slide_config = NULL) {
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
            slide_config = slide_config
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
        .run_export(fname, slide_config = NULL)
      }
    )

    output$download_zip_configured <- downloadHandler(
      filename = .zip_filename,
      content = function(fname) {
        removeModal()
        # Map selected tree leaf IDs back to section IDs.
        # Leaf IDs are "slides_<i>_<j>"; corresponding section id from slide_types_rv().
        selected_tree <- input$slide_sections_tree
        types <- slide_types_rv()
        selected_sections <- unlist(lapply(seq_along(types), function(i) {
          parent_id <- paste0("slides_", i)
          sections <- types[[i]]$sections
          lapply(seq_along(sections), function(j) {
            leaf_id <- paste0(parent_id, "_", j)
            if (leaf_id %in% selected_tree) sections[[j]]$id else NULL
          })
        }))
        selected_sections <- unlist(selected_sections)
        slide_config <- list(
          slide_sections           = selected_sections,
          ind_stats_parameters     = input$slide_ind_params,
          summary_stats_parameters = input$slide_summary_params
        )
        .run_export(fname, slide_config = slide_config)
      }
    )
  })
}

DEFAULT_STATS_PARAMETERS <- c("CMAX", "TMAX", "VSSO", "CLSTP", "LAMZHL", "AUCIFO", "AUCLST", "FABS")

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
