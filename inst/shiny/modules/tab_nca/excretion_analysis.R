
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
      class = "excretion_section_container",
      id = ns("excretion_section_container"),
      div(
        id = ns("excretion_overlay"),
        class = "excretion-overlay",
        style = "display: none;",
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

      if (volume_missing) {
        session$sendCustomMessage("showOverlay", list(id = session$ns("excretion_overlay")))
      } else {
        session$sendCustomMessage("hideOverlay", list(id = session$ns("excretion_overlay")))
      }

      available_cols <- names(conc_data)
      # Check if VOLUME exists before trying to filter
      if (!volume_missing) {
        pcspecs <- conc_data %>%
          filter(!is.na(VOLUME)) %>%
          distinct(PCSPEC) %>%
          pull(PCSPEC)
      } else {
        pcspecs <- character(0)
      }

      updateSelectInput(session, "matrix_select", choices = pcspecs,
                        selected = if ("Urine" %in% pcspecs) "Urine" else NULL)
      updateSelectInput(session, "end_time_col", choices = available_cols,
                        selected = if ("AEFRLT" %in% available_cols) "AEFRLT" else NULL)
      updateSelectInput(session, "param_select", choices = pknca_cdisc_terms %>%
                          filter(startsWith(PPTESTCD, "RCA") |
                                   startsWith(PPTESTCD, "FREX")) %>%
                          pull(PKNCA, PPTESTCD),
                        selected = c("ae"))
    })

    # Perform calculations
    analysis_result <- eventReactive(input$submit_btn, {
      req(input_pknca_data())
      data <- input_pknca_data()

      dose_col <- data$dose$columns$dose
      doseu <- data$dose$columns$doseu
      weight_col <- "WEIGHT"
      weightu <- "WEIGHTU"

      # Adjust dose by bodyweight if selected
      if (input$adjust_bw) {
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
      results <- suppressWarnings(PKNCA::pk.nca(data, verbose = FALSE)) %>%
        # Apply standard CDISC names
        mutate(
          PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTESTCD")
        )

      results
    })

    results_output <- reactive({
      req(analysis_result())

      conc_groups <- unname(unlist(input_pknca_data()$conc$columns$groups))
      #pivot wider
      df <- analysis_result()$result %>%
        mutate(PPSTRESU = ifelse(PPSTRESU %in% c("unitless", "fraction"), "", PPSTRESU)) %>%
        mutate(PPTESTCD = ifelse(PPSTRESU != "",
                                 paste0(PPTESTCD, "[", PPSTRESU, "]"),
                                 PPTESTCD)) %>%
        select(-PPSTRESU, -PPORRES, -PPORRESU, -exclude) %>%
        pivot_wider(names_from = PPTESTCD, values_from = PPSTRES) %>%
        select(-DOSNOA)

      # Add "label" attribute to columns
      df <- add_label_attribute(df, analysis_result())

      df
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
