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

    # Color legend for the results table
    div(
      class = "results-legend",
      style = "display:flex; gap:12px; align-items:center; margin:8px 0;",
      div(style = "font-weight:600; font-size:0.95em; margin-right:8px;", "Flag Rules:"),
      div(style = "display:flex; align-items:center; gap:6px;",
        div(style = paste0(
          "width:14px; height:14px; background:", FLAG_COLOR_FLAGGED, "; border:1px solid #ddd;"
        )),
        span("FLAGGED", style = "font-size:0.9em;")
      ),
      div(style = "display:flex; align-items:center; gap:6px;",
        div(style = paste0(
          "width:14px; height:14px; background:", FLAG_COLOR_MISSING, "; border:1px solid #ddd;"
        )),
        span("MISSING", style = "font-size:0.9em;")
      ),
      div(style = "display:flex; align-items:center; gap:6px;",
        div(style = "width:14px; height:14px; background:#ffffff; border:1px solid #ddd;"),
        span("ACCEPTED", style = "font-size:0.9em;")
      )
    ),

    # Download button
    downloadButton(ns("local_download_NCAres"), "Download locally the NCA Data")
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
      
      # Add flagging column in the pivoted results
      applied_flags <- purrr::keep(settings()$flags, function(x) x$is.checked)
      flag_params <- names(applied_flags)
      flag_params_pknca <- translate_terms(flag_params, "PPTESTCD", "PKNCA")

      requested_flags <- res_nca()$data$intervals %>%
        select(any_of(c(
          unname(unlist(res_nca()$data$conc$columns$groups)),
          flag_params_pknca
        ))) %>%
        # Group by all columns EXCEPT the flag parameters
        group_by(across(-any_of(flag_params_pknca))) %>%
        # Collapse duplicates: if any row is TRUE, the result is TRUE
        summarise(across(any_of(flag_params_pknca), any), .groups = "drop")
      
      flag_thr <- sapply(settings()$flags, FUN =  function(x) x$threshold)
      flag_rule_msgs <- c(paste0(names(settings()$flags), c(" < ", " < ", " > ", " > ", " < "), flag_thr))
      
      valid_indices <- map_lgl(flag_params, function(p) {
        any(grepl(paste0("^", p, "(\\[|$)"), names(final_results)))
      })
      
      flag_params_pknca <- flag_params_pknca[valid_indices]
      
      flag_cols <- names(final_results)[formatters::var_labels(final_results)
                                        %in% translate_terms(flag_params, "PPTESTCD", "PPTEST")]
      # join flag columns to final results
      final_results <- final_results %>%
        left_join(requested_flags, by = intersect(names(.), names(requested_flags)))
      
      if (length(flag_cols) > 0) {
        final_results <- final_results %>%
          rowwise() %>%
          mutate(
            na_msg_vec = list(purrr::map2_chr(flag_cols, flag_params_pknca, function(d_col, f_col) {
              if (!is.na(d_col) && is.na(get(d_col)) && isTRUE(get(f_col))) {
                # Return "Param is NA" (stripping unit for cleaner message)
                paste(str_remove(d_col, "\\[.*\\]"), "is NA")
              } else {
                NA_character_
              }
            }) %>% na.omit()),
            na_msg = paste(na_msg_vec, collapse = "; "),
            # Update Exclude using case_when
            Exclude = case_when(
              # Combine na message and existing Exclude text
              na_msg != "" & !is.na(Exclude) ~ paste(Exclude, na_msg, sep = "; "),
              na_msg != "" ~ na_msg,
              TRUE ~ Exclude
            ),
            flagged = case_when(
              is.na(Exclude) ~ "ACCEPTED",
              any(sapply(
                flag_rule_msgs, function(msg) str_detect(Exclude, fixed(msg))
              )) ~ "FLAGGED",
              TRUE ~ "MISSING"
            )
          ) %>%
          ungroup() %>%
          select(-all_of(c(flag_params_pknca, "na_msg_vec", "na_msg")))
      }
      final_results
    })

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
            list(backgroundColor = FLAG_COLOR_FLAGGED)
          } else if (flagged_value == "MISSING") {
            list(backgroundColor = FLAG_COLOR_MISSING)
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

# Color constants for flagged results
FLAG_COLOR_FLAGGED <- "#f5b4b4"
FLAG_COLOR_MISSING <- "#cbaddd"
