# nca_results UI Module
nca_results_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    "NCA Results",
    pickerInput(
      ns("params"),
      "Select Parameters :",
      choices = list("Run NCA first" = ""),
      selected = list("Run NCA first" = ""),
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    units_table_ui(ns("units_table")),
    reactable_ui(ns("myresults")),
    downloadButton(ns("local_download_NCAres"), "Download locally the NCA Data"),
    downloadButton(ns("download_zip"), "Download All Results as ZIP")
  )
}

# nca_results Server Module
nca_results_server <- function(id, pknca_data, res_nca, settings, ratio_table, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    units_table_server(
      "units_table",
      reactive({          #' Pass `pknca_data` to the units table only when the results
        req(res_nca())    #' are available.
        pknca_data()
      })
    )

    final_results <- reactive({
      req(res_nca())
      res <- res_nca()

      #' Apply units
      if (!is.null(session$userData$units_table())) {
        res$data$units <- session$userData$units_table()
        res$result <- res$result %>%
          select(-PPSTRESU, -PPSTRES) %>%
          left_join(
            session$userData$units_table() %>%
              mutate(PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTESTCD")),
            by = intersect(names(.), names(session$userData$units_table()))
          ) %>%
          mutate(PPSTRES = PPORRES * conversion_factor) %>%
          select(-conversion_factor)
      }

      #' Transform results
      # Calculate bioavailability if available
      results <- res_nca()

      # Transform results
      final_results <- pivot_wider_pknca_results(results)

      # Join subject data to allow the user to group by it
      conc_data_to_join <- res_nca()$data$conc$data %>%
        select(any_of(c(
          grouping_vars(),
          unname(unlist(res_nca()$data$conc$columns$groups)),
          "DOSEA",
          "ATPTREF",
          "ROUTE"
        )))

      final_results <- final_results %>%
        inner_join(conc_data_to_join, by = intersect(names(.), names(conc_data_to_join))) %>%
        distinct() %>%
        mutate(
          flagged = "NOT DONE"
        )

      # Add flaging column in the pivoted results
      applied_flags <- purrr::keep(settings()$flags, function(x) x$is.checked)
      flag_params <- names(settings()$flags)
      flag_thr <- sapply(settings()$flags, FUN =  function(x) x$threshold)
      flag_rule_msgs <- paste0(flag_params, c(" < ", " > ", " > ", " < "), flag_thr)
      flag_cols <- names(final_results)[formatters::var_labels(final_results)
                                        %in% translate_terms(flag_params, "PPTESTCD", "PPTEST")]

      if (length(flag_params) > 0) {
        final_results <- final_results %>%
          mutate(
            flagged = case_when(
              rowSums(is.na(select(., any_of(flag_cols)))) > 0 ~ "MISSING",
              is.na(Exclude) ~ "ACCEPTED",
              any(sapply(
                flag_rule_msgs, function(msg) str_detect(Exclude, fixed(msg))
              )) ~ "FLAGGED",
              TRUE ~ "ACCEPTED"
            )
          )
      }
      final_results
    })

    # Provide the zip file for download
    output$download_zip <- downloadHandler(
      filename = function() {
        project <- session$userData$project_name()
        datetime <- attr(res_nca(), "provenance")$datetime
        paste0(project, "_", format(datetime, "%d-%m-%Y"), ".zip")
      },
      content = function(fname) {
        shiny::withProgress(message = "Preparing ZIP file...", value = 0, {

          # Create an output folder with all plots, tables and listings
          output_tmpdir <- file.path(tempdir(), "output")
          save_output(output = session$userData$results, output_path = output_tmpdir)
          incProgress(0.1)

          # Create presentation slides
          res_nca <- res_nca()
          res_dose_slides <- get_dose_esc_results(
            o_nca = res_nca,
            group_by_vars = setdiff(group_vars(res_nca), res_nca$data$conc$columns$subject),
            facet_vars = "DOSEA",
            statistics = c("Mean"),
            stats_parameters = c(
              "CMAX", "TMAX", "VSSO", "CLSTP", "LAMZHL", "AUCIFO", "AUCLST", "FABS"
            )
          )
          presentations_path <- paste0(output_tmpdir, "/presentations")
          dir.create(presentations_path)

          create_qmd_dose_slides(
            res_dose_slides = res_dose_slides,
            quarto_path = paste0(presentations_path, "/dose_escalation.qmd"),
            title = paste0("NCA Results Slides", " (", session$userData$project_name(), ")"),
            use_plotly = TRUE
          )
          incProgress(0.3)
          create_pptx_dose_slides(
            res_dose_slides = res_dose_slides,
            path = paste0(presentations_path, "/dose_escalation.pptx"),
            title = paste0("NCA Results Slides", " (", session$userData$project_name(), ")"),
            template = "www/templates/template.pptx"
          )
          incProgress(0.6)

          # Create a settings folder
          setts_tmpdir <- file.path(output_tmpdir, "settings")
          dir.create(setts_tmpdir, recursive = TRUE)
          saveRDS(session$userData$settings(), paste0(setts_tmpdir, "/settings.rds"))

          files <- list.files(
            output_tmpdir,
            pattern = paste0(
              ".(csv)|(rds)|(xpt)|(html)|(rda)",
              "|(dose_escalation.pptx)|(dose_escalation.qmd)$"
            ),
            recursive = TRUE
          )

          wd <- getwd()
          on.exit(setwd(wd), add = TRUE) # this will reset the wd after the download handler
          setwd(output_tmpdir)
          incProgress(0.9)
          utils::zip(zipfile = fname, files = files)
          incProgress(1)
        })
      }
    )

    observeEvent(final_results(), {
      req(final_results())

      # Save the latest version of the object
      session$userData$results$nca_results$pivoted_results <- final_results()

      # Represent the available parameters in the input
      param_pptest_cols <- intersect(
        unname(formatters::var_labels(final_results())),
        unique(c(metadata_nca_parameters$PPTEST, ratio_table()$PPTESTCD))
      )
      param_inputnames <- translate_terms(param_pptest_cols, "PPTEST", "input_names")

      updatePickerInput(
        session = session,
        inputId = "params",
        label = "Select Parameters :",
        choices = sort(param_inputnames),
        selected =  param_inputnames
      )
    })

    output_results <- reactive({
      req(final_results(), input$params)

      # Select columns of parameters selected, considering each can have multiple diff units
      param_cols <- unique(res_nca()$result$PPTESTCD)
      input_params <- sub(":.*", "", input$params)
      #identify parameters to be removed from final results
      params_rem_cols <- setdiff(param_cols, input_params)

      col_names <- names(final_results())
      # Extract base names before the "[", or leave as-is if no "["
      col_base_names <- ifelse(str_detect(col_names, "\\["),
                               str_remove(col_names, "\\[.*"),
                               col_names)

      final_results() %>%
        select(c(all_of(col_names[!(col_base_names %in% params_rem_cols)]))) %>%
        # Add group variable labels (others were added in pivot_wider_pknca_result)
        apply_labels()
    })

    reactable_server(
      "myresults",
      output_results,
      compact = TRUE,
      style = list(fontSize = "0.75em"),
      height = "68vh",
      rowStyle = function(x) {
        function(index) {
          flagged_value <- x$flagged[index]
          if (flagged_value == "FLAGGED") {
            list(backgroundColor = "#f5b4b4")
          } else if (flagged_value == "MISSING") {
            list(backgroundColor = "#cbaddd")
          } else {
            NULL
          }
        }
      }
    )

    output$local_download_NCAres <- downloadHandler(
      filename = function() {
        paste0(session$userData$project_name(), "-pivoted_NCA_results.csv")
      },
      content = function(file) {
        write.csv(output_results(), file, row.names = FALSE)
      }
    )
  })
}
