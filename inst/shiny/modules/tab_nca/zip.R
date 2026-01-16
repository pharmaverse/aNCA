zip_ui <- function(id) {
	ns <- NS(id)
	tagList(
		downloadButton(ns("download_zip"), "Download All Results as ZIP")
	)
}

zip_server <- function(id, res_nca, settings, ratio_table, grouping_vars, pknca_data, session) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns

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

						create_qmd_dose_slides(
							res_dose_slides = res_dose_slides,
							quarto_path = paste0(presentations_path, "/results_slides.qmd"),
							title = paste0("NCA Results", "\n", session$userData$project_name()),
							use_plotly = TRUE
						)
						incProgress(0.3)
						create_pptx_dose_slides(
							res_dose_slides = res_dose_slides,
							path = paste0(presentations_path, "/results_slides.pptx"),
							title = paste0("NCA Results", "\n", session$userData$project_name()),
							template = "www/templates/template.pptx"
						)
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

						files <- list.files(
							output_tmpdir,
							pattern = paste0(
								"(\\.csv)|(\\.rds)|(\\.xpt)|(\\.html)|(\\.rda)|(\\.png)",
								"|(results_slides\\.pptx)|(results_slides\\.qmd)$"
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
