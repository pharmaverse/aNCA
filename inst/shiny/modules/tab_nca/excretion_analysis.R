
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
            selectInput(ns("end_time_col"), "Map End Time Column:", choices = NULL),
            checkboxInput(ns("adjust_bw"), "Adjust for Body Weight", value = FALSE),
            selectInput(ns("param_select"), "Select Parameters:", choices = NULL, multiple = TRUE),
            checkboxGroupInput(
              ns("interval_types"),
              "Select Interval Types:",
              c("Samples" = "sample", "Profiles" = "profile"),
              selected = c("sample", "profile")
            ),
            actionButton(ns("submit_btn"), "Submit")
          )
        ),
        card(
          reactableOutput(ns("results_table"))
        )
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
      updateSelectInput(session, "end_time_col", choices = available_cols,
                        selected = if ("AEFRLT" %in% available_cols) "AEFRLT" else NULL)
      updateSelectInput(session, "param_select", choices = metadata_nca_parameters %>%
                          filter(startsWith(PPTESTCD, "RCA")) %>%
                          pull(PKNCA, PPTESTCD),
                        selected = c("ae"))
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
      }

      # Update intervals
      # dose profile intervals
      data$intervals <- format_pkncadata_intervals(data$conc,
                                                   data$dose,
                                                   params = input$param_select) %>%
        mutate(type_interval = "profile") %>%
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

      # Run PKNCA analysis
      suppressWarnings(PKNCA::pk.nca(data, verbose = FALSE)) %>%
        # Apply standard CDISC names
        mutate(PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTESTCD"))
    }) |>
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
        select(-DOSNOA) %>%
        # Add "label" attribute to columns
        add_label_attribute(analysis_result())
    })

    # Render results
    output$results_table <- renderReactable({
      req(results_output())

      reactable(results_output(),
                defaultPageSize = 10,
                searchable = TRUE,
                highlight = TRUE)

    })
  })
}
