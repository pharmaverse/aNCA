zip_ui <- function(id) {
	ns <- NS(id)
	tagList(
	  fluidRow(
	    shinyWidgets::treeInput(
	      inputId = ns("res_tree"),
	      label = "Exportable ZIP Contents:",
	      selected = get_tree_leaf_ids(TREE_LIST),
	      choices = TREE_LIST
	    )
	  ),
		fluidRow(
			column(4,
				selectizeInput(
					ns("plot_formats"),
					"Plot formats:",
					choices = c("png", "html"),
					selected = c("png", "html"),
					multiple = TRUE
				)
			),
			column(4,
				selectizeInput(
					ns("slide_formats"),
					"Slide formats:",
					choices = c("pptx", "qmd"),
					selected = c("pptx", "qmd"),
					multiple = TRUE
				)
			),
			column(4,
				selectizeInput(
					ns("table_formats"),
					"Table formats:",
					choices = c("rds", "xpt", "csv"),
					selected = c("rds", "xpt", "csv"),
					multiple = TRUE
				)
			)
		),
		downloadButton(ns("download_zip"), "Download All Results as ZIP")
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

						res_tree_list <- session$userData$results
						extras_tree_list <- list(
							presentation_slides = list(
								results_slides = ""
							),
							r_script = "",
							settings_file = ""
						)

						res_tree_ui <- create_tree_from_list_names(session$userData$results)
						extras_tree_ui <- create_tree_from_list_names(extras_tree_list)

						session$userData$results
						save_output(output = session$userData$results, output_path = output_tmpdir)
						incProgress(0.1)

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
						incProgress(0.6)

						# Create a settings folder
						setts_tmpdir <- file.path(output_tmpdir, "settings")
						dir.create(setts_tmpdir, recursive = TRUE)
						settings_list <- session$userData$settings
						setings_to_save <- list(
							settings = settings_list$settings(),
							slope_rules = settings_list$slope_rules()
						)

						saveRDS(setings_to_save, paste0(setts_tmpdir, "/settings.rds"))

						# Filter files by selected formats (for demonstration, not full implementation)
						files <- list.files(
							output_tmpdir,
							pattern = paste0(
								"(",
								paste0(c(
									if (length(table_formats) > 0) paste0("\\.", table_formats),
									if (length(plot_formats) > 0) paste0("\\.", plot_formats),
									if (length(slide_formats) > 0) paste0("results_slides\\.", slide_formats)
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
TREE_LIST <- create_tree_from_list_names(
    list(
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
)
