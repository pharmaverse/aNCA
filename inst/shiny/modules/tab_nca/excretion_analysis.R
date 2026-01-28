#' NCA Excretion Analysis Module
#'
#' This module handles logic for excretion analysis in NCA.
#' It allows users to select matrices, map end time columns,
#' adjust for body weight, and select parameters for analysis.
#'
#' @param id A character string used to uniquely identify the module.
#' @param input_pknca_data the input data, which should be a PKNCAdata object
#'
excretion_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "excretion-section-container",
      id = ns("excretion_section_container"),
      div(
        class = "excretion-overlay",
        "Excretion analysis is disabled: 'VOLUME' column required."
      ),
      div(style = "position: relative;",  # Wrapping makes overlay work
        card(
          card_body(
            selectInput(ns("matrix_select"), "Select Matrices:", choices = NULL, multiple = TRUE),
            uiOutput(ns("map_end_col_ui_wrapper")),
            checkboxInput(ns("adjust_bw"), "Adjust for Body Weight", value = TRUE),
            uiOutput(ns("param_select_ui_wrapper")),
            checkboxGroupInput(
              ns("interval_types"),
              "Select Interval Types:",
              c("Samples" = "sample", "Profiles" = "profile"),
              selected = c("sample", "profile")
            ),
            actionButton(ns("submit_btn"), "Submit")
          )
        ),
        card(reactable_ui(ns("results_table")), class = "border-0 shadow-none")
      )
    )
  )
}


# Server function
excretion_server <- function(id, input_pknca_data) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Update choices dynamically
    observe({
      req(input_pknca_data())

      conc_data <- input_pknca_data()$conc$data
      volume_missing <- !all(c("VOLUME", "VOLUMEU") %in% names(conc_data))

      shinyjs::toggle(selector = ".excretion-overlay", condition = volume_missing)

      available_cols <- names(conc_data)
      # Check if VOLUME exists before trying to filter
      pcspecs <- {
        if (!volume_missing) {
          conc_data %>%
            filter(!is.na(VOLUME)) %>%
            distinct(PCSPEC) %>%
            pull(PCSPEC)
        } else {
          character(0)
        }
      }
      
      updateSelectInput(session, "matrix_select", choices = pcspecs,
                        selected = if ("Urine" %in% pcspecs) "Urine" else NULL)
      
      variables_choices <- reactive({
        req(metadata_nca_variables)
        
        # Taking the variables and labels from the metadata
        choices_df <- metadata_nca_variables %>%
          select(Variable, Label) %>%
          distinct(Variable, .keep_all = TRUE) %>%
          filter(!is.na(Variable), Variable != "") %>%
          filter(Variable %in% available_cols)
        
        unname(purrr::pmap(list(choices_df$Variable, choices_df$Label), function(var, lab) {
          list(
            label = as.character(var),
            value = as.character(var),
            description = as.character(lab)
          )
        }))
      })
      
      # Rendering the map end time column selector
      output$map_end_col_ui_wrapper <- renderUI({
        req(variables_choices())
        end_time_col_vars <- variables_choices()
        
        shinyWidgets::virtualSelectInput(
          inputId = ns("end_time_col"),
          label = "Map End Time Column:",
          choices = end_time_col_vars,
          multiple = TRUE,
          selected =  if ("AEFRLT" %in% available_cols) "AEFRLT" else NULL,
          search = TRUE,
          hasOptionDescription = TRUE,
          position = "bottom",
          dropboxWrapper = "body"
        )
      })

      urine_params_to_select <- metadata_nca_parameters %>%
        filter(TYPE == "Urine")
      
      parameters_to_select <- urine_params_to_select$PPTESTCD
      
      parameters_choices <- reactive({
        req(metadata_nca_parameters)
        
        # Taking the parameters and labels from the metadata
        choices_df <- metadata_nca_parameters %>%
          select(PPTESTCD, PPTEST) %>%
          distinct(PPTESTCD, .keep_all = TRUE) %>%
          filter(!is.na(PPTESTCD), PPTESTCD != "") %>%
          filter(PPTESTCD %in% parameters_to_select)
        
        unname(purrr::pmap(list(choices_df$PPTESTCD, choices_df$PPTEST), function(var, lab) {
          list(
            label = as.character(var),
            value = as.character(var),
            description = as.character(lab)
          )
        }))
      })
      
      # Rendering the parameters to select selector
      output$param_select_ui_wrapper <- renderUI({
        req(parameters_choices())
        parameters_select <- parameters_choices()
        
        shinyWidgets::virtualSelectInput(
          inputId = ns("param_select"),
          label = "Select Parameters:",
          choices = parameters_select,
          multiple = TRUE,
          selected = c("RCAMINT", "FREXINT"),
          search = TRUE,
          hasOptionDescription = TRUE,
          dropboxDirection = "bottom"
        )
      })
    })

    # Perform calculations
    analysis_result <- reactive({
      req(input_pknca_data())
      data <- input_pknca_data()

      dose_col <- data$dose$columns$dose
      doseu <- data$dose$columns$doseu
      weight_col <- "WEIGHT"
      weightu <- "WEIGHTU"

      # Adjust dose by bodyweight if selected
      if (input$adjust_bw) {
        # Check if weight columns exist
        if (!(weight_col %in% names(data$dose$data)) || !(weightu %in% names(data$dose$data))) {
          showNotification("Please ensure WEIGHT and WEIGHTU columns exist in the dose data.
                           No adjustments can be made.", type = "warning")
          return(NULL)
        }

        # mutate dose_col and doseu to be dose * weight
        data$dose$data <- data$dose$data %>%
          mutate(
            # Create units objects per row
            dose_with_unit = pmap(list(.d = !!sym(dose_col), .u = !!sym(doseu)),
                                  ~ set_units(..1, ..2, mode = "standard")),
            weight_with_unit = pmap(list(.w = !!sym(weight_col), .wu = !!sym(weightu)),
                                    ~ set_units(..1, ..2, mode = "standard")),

            # Multiply units objects
            dose_total_unit = map2(dose_with_unit, weight_with_unit, ~ .x * .y),

            # Assign numeric result and unit string to original columns
            !!dose_col := map_dbl(dose_total_unit, drop_units),
            !!doseu := map_chr(dose_total_unit, deparse_unit)
          )

        # Update units
        data$units <- PKNCA_build_units_table(data$conc, data$dose)
      }

      # Update intervals
      # dose profile intervals
      data$intervals <- format_pkncadata_intervals(data$conc,
                                                   data$dose) %>%
        mutate(across(any_of(input$param_select), ~ TRUE, .names = "{.col}"),
               type_interval = "profile") %>%
        filter(PCSPEC %in% input$matrix_select)

      # excretion sample intervals
      # add one row with start AFRLT and end_time_col
      end_time_col <- input$end_time_col

      if (is.null(end_time_col) || end_time_col == "") {
        showNotification("Please select an end time column.", type = "error")
        return(NULL)
      }

      # Obtain all possible pknca parameters
      all_pknca_params <- setdiff(names(PKNCA::get.interval.cols()),
                                  c("start", "end"))

      conc_groups <- unname(unlist(data$conc$columns$groups))

      excretion_intervals <- data$conc$data %>%
        group_by(!!!syms(conc_groups)) %>%
        select(any_of(c(conc_groups, end_time_col, "AFRLT"))) %>%
        mutate(PCSPEC = as.character(PCSPEC)) %>%
        rename(start = AFRLT, end = !!sym(end_time_col)) %>%
        filter(PCSPEC %in% input$matrix_select) %>%
        distinct() %>%
        # Create logical columns with only TRUE for the NCA parameters requested by the user
        mutate(!!!setNames(rep(FALSE, length(all_pknca_params)), all_pknca_params)) %>%
        mutate(across(any_of(input$param_select), ~ TRUE, .names = "{.col}"),
               type_interval = "sample")

      # Combine dose profile intervals and excretion sample intervals
      data$intervals <- bind_rows(data$intervals, excretion_intervals) %>%
        filter(type_interval %in% input$interval_types) %>%
        arrange(PCSPEC, start, end)

      data$units <- data$units  %>%
        mutate(PPSTRESU = ifelse(PPTESTCD == "fe", "%", PPSTRESU),
               conversion_factor = get_conversion_factor(PPORRESU, PPSTRESU))

      data$options$keep_interval_cols <- c("ATPTREF", "type_interval")
      # Run PKNCA analysis
      suppressWarnings(PKNCA::pk.nca(data, verbose = FALSE)) %>%
        # Apply standard CDISC names
        mutate(PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTESTCD"))
    }) %>%
      bindEvent(input$submit_btn)

    results_output <- reactive({
      req(analysis_result())

      conc_groups <- unname(unlist(input_pknca_data()$conc$columns$groups))
      #pivot wider
      analysis_result()$result %>%
        mutate(PPSTRESU = ifelse(PPSTRESU %in% c("unitless", "fraction"), "", PPSTRESU)) %>%
        mutate(PPTESTCD = ifelse(PPSTRESU != "",
                                 paste0(PPTESTCD, "[", PPSTRESU, "]"),
                                 PPTESTCD)) %>%
        select(-PPSTRESU, -PPORRES, -PPORRESU, -exclude) %>%
        pivot_wider(names_from = PPTESTCD, values_from = PPSTRES) %>%
        # Add "label" attribute to columns
        add_label_attribute(analysis_result())
    })

    # Render results
    reactable_server(
      "results_table",
      results_output,
      defaultPageSize = 10,
      pageSizeOptions = reactive(c(10, 25, 50, 100, nrow(results_output())))
    )

    # Save the results in the output folder
    observeEvent(results_output(), {
      session$userData$results$additional_analysis$excretion_results <- results_output()
    })
  })
}
