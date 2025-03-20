# NCA TAB -----
# This module contains nested modules and all the ui and server code for the NCA tab
# NCA UI function ----
tab_nca_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    actionButton(ns("nca"), "Run NCA", class = "run-nca-btn"),
    nav_panel("Save setts", save_settings_ui(ns("save_settings"))),

    navset_tab(
      id = ns("ncapanel"),
      nav_panel(
        "Setup", fluid = TRUE,
        navset_pill_list(
          nav_panel("NCA settings", nca_setup_ui(ns("nca_setup"))),
          nav_panel("Slope Selector", slope_selector_ui(ns("slope_selector")))
        )
      ),
      nav_panel(
        "Results", fluid = TRUE,
        navset_pill_list(
          nca_results_ui(ns("nca_results")),
          nav_panel(
            "Slopes Information",
            navset_pill(
              nav_panel(
                "Slopes Results",
                DTOutput(ns("preslopesettings"))
              ),
              nav_panel(
                "Manual Adjustments",
                tableOutput(ns("manual_slopes2"))
              ),
            )
          ),
          nav_panel(
            "Descriptive Statistics",
            descriptive_statistics_ui(ns("descriptive_stats"))
          ),
          nav_panel("Parameter Datasets", parameter_datasets_ui(ns("parameter_datasets")))
        )
      ),
      nav_panel("Additional Analysis", additional_analysis_ui(ns("non_nca")))
    )

  )

}

# NCA Server Function ----
# Requires processed data and grouping vars from tab_data module
tab_nca_server <- function(id, data, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize PKNCAdata ----

    mydata <- reactiveVal(NULL)

    observeEvent(data(), priority = 2, {
      req(data())

      # Define column names
      group_columns <- intersect(colnames(data()), c("STUDYID", "PCSPEC", "ROUTE", "DRUG"))
      usubjid_column <- "USUBJID"
      time_column <- "AFRLT"
      dosno_column <- "DOSNO"
      route_column <- "ROUTE"
      analyte_column <- "ANALYTE"
      matrix_column <- "PCSPEC"
      std_route_column <- "std_route"

      # Create concentration data
      df_conc <- format_pkncaconc_data(
        ADNCA = data(),
        group_columns = c(group_columns, usubjid_column, analyte_column),
        time_column = time_column,
        route_column = route_column,
        dosno_column = dosno_column
      ) %>%
        arrange(across(all_of(c(usubjid_column, time_column))))

      # Create dosing data
      df_dose <- format_pkncadose_data(
        pkncaconc_data = df_conc,
        group_columns = c(group_columns, usubjid_column),
        time_column = time_column,
        dosno_column = dosno_column,
        since_lastdose_time_column = "ARRLT"
      )

      # Set default settings
      df_conc$is.excluded.hl <- FALSE
      df_conc$is.included.hl <- FALSE
      df_conc$REASON <- NA
      df_conc$exclude_half.life <- FALSE

      # Create PKNCA objects

      myconc <- PKNCA::PKNCAconc(
        df_conc,
        formula = AVAL ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID / ANALYTE,
        exclude_half.life = "exclude_half.life",
        time.nominal = "NFRLT"
      )

      mydose <- PKNCA::PKNCAdose(
        data = df_dose,
        formula = DOSEA ~ TIME | STUDYID + PCSPEC + DRUG + USUBJID,
        route = std_route_column,
        time.nominal = "NFRLT",
        duration = "ADOSEDUR"
      )

      #create basic intervals so that PKNCAdata can be created
      #TODO: investigate if this is required
      intervals <-
        data.frame(
          start = 0, end = Inf,
          cmax = TRUE,
          tmax = TRUE,
          auclast = FALSE,
          aucinf.obs = FALSE
        )

      mydata <- PKNCA::PKNCAdata(
        data.conc = myconc,
        data.dose = mydose,
        intervals = intervals,
        units = PKNCA::pknca_units_table(
          concu = myconc$data$AVALU[1],
          doseu = myconc$data$DOSEU[1],
          amountu = myconc$data$AVALU[1],
          timeu = myconc$data$RRLTU[1]
        )
      )

      # Update units
      unique_analytes <- unique(mydata$conc$data[[mydata$conc$columns$groups$group_analyte]])
      analyte_column <- mydata$conc$columns$groups$group_analyte
      mydata$units <- tidyr::crossing(mydata$units,
                                      !!sym(analyte_column) := unique_analytes)  %>%
        mutate(PPSTRESU = PPORRESU, conversion_factor = 1)

      mydata(mydata)
    })

    # NCA SETUP MODULE ----
    rules <- nca_setup_server("nca_setup", data, mydata, res_nca)

    # NCA RESULTS ----
    res_nca <- reactiveVal(NULL)
    pk_nca_trigger <- reactiveVal(0)

    observeEvent(input$nca, {
      pk_nca_trigger(pk_nca_trigger() + 1)
    })

    observeEvent(pk_nca_trigger(), {
      req(mydata())

      withProgress(message = "Calculating NCA...", value = 0, {
        tryCatch({
          myres <- PKNCA::pk.nca(data = mydata(), verbose = FALSE)

          myres$result <- myres$result %>%
            inner_join(select(mydata()$dose$data, -exclude,
                              -mydata()$conc$columns$groups$group_analyte)) %>%
            mutate(start_dose = start - !!sym(myres$data$dose$columns$time),
                   end_dose = end - !!sym(myres$data$dose$columns$time)) %>%
            select(names(myres$result), start_dose, end_dose)

          res_nca(myres)
          updateTabsetPanel(session, "ncapanel", selected = "Results")

        }, error = function(e) {
          full_error <- e$parent$message
          if (grepl("pk.calc.", x = full_error)) {
            param_of_error <- gsub(".*'pk\\.calc\\.(.*)'.*", "\\1", full_error)
            full_error <- paste0("Problem in calculation of NCA parameter: ", param_of_error,
                                 "<br><br>", full_error)
          }
          modified_error <- gsub("Please report a bug.\n:", "", x = full_error, fixed = TRUE) %>%
            paste0("<br><br>If the error is unexpected, please report a bug.")
          showNotification(HTML(modified_error), type = "error", duration = NULL)
        })
      })
    })

    nca_results_server("nca_results", res_nca, rules(), grouping_vars)

    # SLOPE SELECTOR ----
    # Keep the UI table constantly actively updated
    observeEvent(input, {
      req(mydata())
      dynamic_columns <- c(setdiff(unname(unlist(mydata()$conc$columns$groups)), "DRUG"), "DOSNO")
      for (input_name in grep(
        paste0("(", paste(c(dynamic_columns, "TYPE", "RANGE", "REASON"),
                          collapse = "|"), ")_Ex\\d+$"),
        names(input), value = TRUE
      )) {
        observeEvent(input[[input_name]], {
          # Get the ID of the exclusion
          id <- gsub("_(Ex\\d+)$", "", input_name)

          # Update the reactive list of exclusion IDs
          manual_slopes <- manual_slopes()
          set_selected_value(
            manual_slopes[manual_slopes$id == id, ], paste0(input[[input_name]])
          ) <- manual_slopes[manual_slopes$id == id, ]
          manual_slopes(manual_slopes)

        })
      }
    })

    slope_rules <- slope_selector_server(
      "slope_selector",
      mydata,
      res_nca,
      pk_nca_trigger,
      reactive(input$settings_upload)
    )

    output$preslopesettings <- DT::renderDataTable({
      req(res_nca())
      pivot_wider_pknca_results(res_nca()) %>%
        select(
          any_of(c("USUBJID", "DOSNO", "ANALYTE", "PCSPEC")),
          starts_with("lambda.z"),
          starts_with("span.ratio"),
          starts_with("half.life"),
          "Exclude"
        ) %>%
        DT::datatable(
          extensions = "FixedHeader",
          options = list(scrollX = TRUE, scrollY = "80vh",
                         lengthMenu = list(c(10, 25, -1), c("10", "25", "All")),
                         pageLength = -1, fixedHeader = TRUE)
        ) %>%
        formatStyle("Exclude", target = "row",
                    backgroundColor = styleEqual(NA, NA, default = "#f5b4b4"))
    })

    output$manual_slopes2 <- renderTable({
      slope_rules()
    })

    # TAB: Descriptive Statistics ---------------------------------------------
    # This tab computes and visualizes output data from the NCA analysis

    descriptive_statistics_server("descriptive_stats", res_nca, grouping_vars)

    # NCA SETTINGS ----
    # TODO: move this section to a new module
    # Save the project settings
    save_settings_server("save_settings", mydata, session)

    # ADDITIONAL ANALYSIS ----
    additional_analysis_server("non_nca", mydata, grouping_vars)

    # PARAMETER DATASETS ----
    parameter_datasets_server("parameter_datasets", res_nca)

    # return results for use in other modules
    res_nca
  })
}
