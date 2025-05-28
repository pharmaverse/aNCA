download_settings_ui <- function(id) {
  ns <- NS(id)

  downloadButton(ns("save_settings"), "Save Project Settings")
}

download_settings_server <- function(id, pknca_data, res_nca) {
  moduleServer(id, function(input, output, session) {
    #' TODO: this was migrated 1:1, the handler reads some inputs directly, and it should
    #' not. This is broken currently. Needs deeper refactor.
    output$save_settings <- downloadHandler(
      filename = function() {
        paste(pknca_data()$conc$data$STUDYID[1], "_NCA_settings.csv", sep = "_")
      },
      content = function(file) {

        # Get the data settings from the NCA results (data run)
        res_conc <- res_nca()$data$conc

        # Create a settings file that the user can download/upload
        #for establishing the same configuration
        setts_lambda <- res_conc$data %>%
          # Identify the points that the user has manually selected for the half-life calculation
          mutate(
            TYPE = case_when(is.excluded.hl ~ "Exclusion", is.included.hl ~ "Selection", TRUE ~ NA)
          ) %>%
          filter(is.excluded.hl | is.included.hl)  %>%
          select(any_of(c(
            unname(unlist(res_conc$columns$groups)),
            "IX",
            res_conc$columns$time,
            res_conc$columns$concentration,
            "TYPE",
            "REASON"
          )))

        # Make sure that there is at least one row so the settings can be considered
        if (nrow(setts_lambda) == 0) {
          setts_lambda <- setts_lambda %>%
            add_row()
        }

        # Consider the intervals defined by the user for the AUC calculation
        input_names_aucmin <- grep("^timeInputMin_", names(input), value = TRUE)
        input_names_aucmax <- grep("^timeInputMax_", names(input), value = TRUE)
        auc_mins <- unlist(lapply(input_names_aucmin, function(name) input[[name]]))
        auc_maxs <- unlist(lapply(input_names_aucmax, function(name) input[[name]]))

        # Include the rule settings as additional columns
        setts <- setts_lambda %>%
          mutate(
            PARAM %in% input$select_analyte,
            doses_selected = ifelse(
              !is.null(input$select_nca_profile),
              paste0(input$select_nca_profile, collapse = ","),
              unique(pknca_data()$conc$data$NCA_PROFILE)
            ),
            method = input$method,
            adj.r.squared_threshold = ifelse(
              input$adj.r.squared_rule, input$adj.r.squared_threshold, NA
            ),
            aucpext.obs_threshold = ifelse(
              input$aucpext.obs.rule, input$aucpext.obs_threshold, NA
            ),
            aucpext.pred_threshold = ifelse(
              input$aucpext.pred.rule, input$aucpext.pred_threshold, NA
            ),
            span.ratio_threshold = ifelse(
              input$span.ratio.rule, input$span.ratio_threshold, NA
            ),
            auc_mins = if (is.null(auc_mins)) NA else paste(auc_mins, collapse = ","),
            auc_maxs = if (is.null(auc_maxs)) NA else paste(auc_maxs, collapse = ",")
          ) %>%
          write.csv(file, row.names = FALSE)
      },
      contentType = "text/csv"
    )
  })
}
