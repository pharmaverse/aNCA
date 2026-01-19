zip_ui <- function(id) {
	ns <- NS(id)
	TREE_UI <- create_tree_from_list_names(TREE_LIST)
	tagList(
	  fluidRow(
      column(
        width = 6,
        h4("Select Results to Export"),
        shinyWidgets::treeInput(
          inputId = ns("res_tree"),
          label = NULL,
          selected = get_tree_leaf_ids(TREE_UI),
          choices = TREE_UI
        )
      ),
      column(
        width = 6,
        h4("Choose Export Formats"),
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
        ),
        div(
          downloadButton(ns("download_zip"), "Export ZIP"),
          style = "text-align: left;"
        )
      )
	)
  )
}

zip_server <- function(id, res_nca, settings, ratio_table, grouping_vars, pknca_data) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns
        session_results <- reactive(session$userData$results)

		output$download_zip <- downloadHandler(
			filename = function() {
				project <- session$userData$project_name()
				datetime <- attr(res_nca(), "provenance")$datetime
				paste0(project, "_", format(datetime, "%d-%m-%Y"), ".zip")
			},
			content = function(fname) {
				tryCatch({
					shiny::withProgress(message = "Preparing ZIP file...", value = 0, {
						output_tmpdir <- file.path(tempdir(), "output")

						# Use selected formats from UI
						plot_formats <- input$plot_formats
						slide_formats <- input$slide_formats
						table_formats <- input$table_formats
						browser()

						save_output(
              output = session$userData$results,
              output_path = output_tmpdir,
              ggplot_formats = plot_formats,
              table_formats = table_formats,
              obj_names = input$res_tree
            )
						incProgress(0.1)
                        
            if ("results_slides" %in% input$res_tree) {
                # Create presentation slides
                res_nca_val <- res_nca()
                res_dose_slides <- get_dose_esc_results(
                  o_nca = res_nca_val,
                  group_by_vars = setdiff(grouping_vars(), res_nca_val$data$conc$columns$subject),
                  facet_vars = "DOSEA",
                  statistics = c("Mean"),
                  stats_parameters = c(
                    "CMAX", "TMAX", "VSSO", "CLSTP", "LAMZHL", "AUCIFO", "AUCLST", "FABS"
                  ),
                  info_vars = grouping_vars()
                )
                presentations_path <- paste0(output_tmpdir, "/presentations")
                dir.create(presentations_path)

                if ("qmd" %in% slide_formats) {
                    create_qmd_dose_slides(
                        res_dose_slides = res_dose_slides,
                        quarto_path = paste0(presentations_path, "/results_slides.qmd"),
                        title = paste0("NCA Results", "\n", session$userData$project_name()),
                        use_plotly = TRUE
                    )
                }
                if ("pptx" %in% slide_formats) {
                    create_pptx_dose_slides(
                        res_dose_slides = res_dose_slides,
                        path = paste0(presentations_path, "/results_slides.pptx"),
                        title = paste0("NCA Results", "\n", session$userData$project_name()),
                        template = "www/templates/template.pptx"
                    )
                }
            }
            incProgress(0.6)

						# Create a settings folder
            if ("settings_file" %in% input$res_tree) {
                setts_tmpdir <- file.path(output_tmpdir, "settings")
                dir.create(setts_tmpdir, recursive = TRUE)
                settings_list <- session$userData$settings
                setings_to_save <- list(
                    settings = settings_list$settings(),
                    slope_rules = settings_list$slope_rules()
                )
                saveRDS(setings_to_save, paste0(setts_tmpdir, "/settings.rds"))
            }

						# Filter files by selected formats (for demonstration, not full implementation)
						files <- list.files(
							output_tmpdir,
							pattern = paste0(
								"(",
								paste0(c(
									if (length(table_formats) > 0) paste0("\\.", table_formats),
									if (length(plot_formats) > 0) paste0("\\.", plot_formats),
									if (length(slide_formats) > 0) paste0("results_slides\\.", slide_formats),
                  if ("r_script" %in% input$res_tree) "r_script\\.R",
                  if ("settings_file" %in% input$res_tree) "settings\\.rds"
								), collapse = "|"),
								")$"
							),
							recursive = TRUE
						)
						wd <- getwd()
						on.exit(setwd(wd), add = TRUE)
						setwd(output_tmpdir)
						incProgress(0.9)
						zip::zipr(zipfile = fname, files = files, mode = "mirror")
						incProgress(1)
					})
				}, error = function(e) {
					message("Download Error:")
					message(e$message)
					stop(e)
				})
			}
		)
	})
}

# Define a global EXTRAS_TREE_LIST to be used in the zip_ui tree
TREE_LIST <- list(
  exploration = list(
      individualplot = "",
      meanplot = ""
  ),
  nca_results = list(
      pivoted_results = ""
  ),
  CDISC = list(
      pp = "",
      adpp = "",
      adnca = ""
  ),
  extras = list(
      presentation_slides = list(
          results_slides = ""
      ),
      r_script = "",
      settings_file = ""
  )
)

