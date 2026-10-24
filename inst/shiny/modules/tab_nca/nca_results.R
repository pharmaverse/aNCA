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
    uiOutput(ns("select_params_ui_wrapper")
    ),
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
    downloadButton(ns("local_download_NCAres"), "Download NCA Data")
  )
}

# nca_results Server Module
nca_results_server <- function(id, pknca_data, res_nca, settings, ratio_table, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
          mutate(PPSTRES = ifelse(
            !is.null(conversion_factor),
            PPORRES * conversion_factor,
            PPORRES
          )) %>%
          select(-conversion_factor)
      }

      #' Transform results
      results <- res_nca()

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

      parameter_columns <- aNCA:::.result_parameter_columns(res_nca()$result)
      param_inputnames <- unique(parameter_columns$parameter[
        parameter_columns$column %in% names(final_results())
      ])

      selector_label(input = input,
                     output = output,
                     session = session,
                     choices = sort(param_inputnames),
                     initial_selection = param_inputnames,
                     selector_ui_wrapper = "select_params_ui_wrapper",
                     id = "params",
                     label = "Select Parameters:",
                     metadata_type = "parameter")
    })

    output_results <- reactive({
      req(final_results())
      if (is.null(input$params)) return(final_results())
      parameter_columns <- aNCA:::.result_parameter_columns(res_nca()$result)
      remove_columns <- parameter_columns$column[
        !parameter_columns$parameter %in% input$params
      ]
      final_results() %>% select(-any_of(remove_columns))
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
