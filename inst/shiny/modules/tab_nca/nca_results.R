#' Module for displaying NCA results.
#'
#' Provides a parameter picker, units table, interactive results table with
#' color-coded acceptance flags and a download button for the NCA output.
#'
#' @param id Module namespace ID.

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
    card(reactable_ui(ns("myresults")), class = "border-0 shadow-none"),

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
      }),
      ratio_table
    )

    final_results <- reactive({
      req(res_nca())
      res <- res_nca()

      #' Apply units
      if (!is.null(session$userData$units_table())) {
        res$data$units <- session$userData$units_table()
        custom_units <- session$userData$units_table() %>%
          mutate(PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTESTCD"))

        # Split into PKNCA units (with group cols) and ratio units (NA groups)
        group_cols <- setdiff(
          names(custom_units),
          c("PPTESTCD", "PPORRESU", "PPSTRESU", "conversion_factor")
        )
        has_groups <- length(group_cols) > 0
        if (has_groups) {
          is_ratio_row <- rowSums(is.na(custom_units[, group_cols, drop = FALSE])) > 0
        } else {
          is_ratio_row <- rep(FALSE, nrow(custom_units))
        }
        pknca_custom <- custom_units[!is_ratio_row, , drop = FALSE]
        ratio_custom <- custom_units[is_ratio_row, , drop = FALSE] %>%
          select(PPTESTCD, PPSTRESU, conversion_factor)

        result <- res$result %>%
          select(-PPSTRESU, -PPSTRES) %>%
          mutate(PPSTRESU = NA_character_, conversion_factor = NA_real_)

        # Join PKNCA parameters by all shared columns (including groups)
        if (nrow(pknca_custom) > 0) {
          pknca_join_cols <- intersect(names(result), names(pknca_custom))
          pknca_join_cols <- setdiff(pknca_join_cols, c("PPSTRESU", "conversion_factor"))
          matched <- result %>%
            select(-PPSTRESU, -conversion_factor) %>%
            left_join(pknca_custom, by = pknca_join_cols)
          result$PPSTRESU <- matched$PPSTRESU
          result$conversion_factor <- matched$conversion_factor
        }
        # Join ratio parameters by PPTESTCD only for unmatched rows
        if (nrow(ratio_custom) > 0) {
          unmatched <- is.na(result$PPSTRESU)
          if (any(unmatched)) {
            ratio_matched <- result[unmatched, ] %>%
              select(-PPSTRESU, -conversion_factor) %>%
              left_join(ratio_custom, by = "PPTESTCD")
            result$PPSTRESU[unmatched] <- ratio_matched$PPSTRESU
            result$conversion_factor[unmatched] <- ratio_matched$conversion_factor
          }
        }

        res$result <- result %>%
          mutate(PPSTRES = ifelse(
            !is.na(conversion_factor),
            PPORRES * conversion_factor,
            PPORRES
          )) %>%
          select(-conversion_factor)
      }

      #' Transform results
      # Use res (with unit conversions applied) rather than re-reading res_nca()
      results <- res

      # Transform results
      extra_vars_to_keep <- c(grouping_vars(), "DOSEA", "ATPTREF", "ROUTE")
      session$userData$extra_vars_to_keep <- extra_vars_to_keep

      final_results <- pivot_wider_pknca_results(
        results,
        flag_rules = settings()$flags,
        extra_vars_to_keep = extra_vars_to_keep
      )

      final_results
    })

    observeEvent(final_results(), {
      req(final_results())

      # Save the latest version of the object
      session$userData$results$nca_results$nca_pkparam <- final_results()

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
        select(c(all_of(col_names[!(col_base_names %in% params_rem_cols)])))
    })

    reactable_server(
      "myresults",
      output_results,
      rowStyle = function(x) {
        function(index) {
          if (!"flagged" %in% names(x)) return(NULL)

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
        paste0(session$userData$project_prefix("-"), "pivoted_NCA_results.csv")
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
