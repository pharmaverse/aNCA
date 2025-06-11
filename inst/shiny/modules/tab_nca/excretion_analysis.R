# UI function
excretion_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    card(
      card_body(
        selectInput(ns("matrix_select"), "Select Matrices:", 
                    choices = NULL, multiple = TRUE),
        # selectInput(ns("end_time_col"), "Map End Time Column:", 
        #             choices = NULL),
        checkboxInput(ns("adjust_bw"), "Adjust for Body Weight", value = FALSE),
        selectInput(ns("param_select"), "Select Parameters:", 
                    choices = NULL, multiple = TRUE),
        actionButton(ns("submit_btn"), "Submit")
      )
    ),
    card(
      reactableOutput(ns("results_table"))
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
      
      available_cols <- names(input_pknca_data()$conc$data)
      
      updateSelectInput(session, "matrix_select", choices = unique(input_pknca_data()$conc$data$PCSPEC))
      #updateSelectInput(session, "end_time_col", choices = available_cols)
      updateSelectInput(session, "param_select", choices = pknca_cdisc_terms %>%
                          filter(startsWith(PPTESTCD, "RCA") | 
                                   startsWith(PPTESTCD, "RENAL") | 
                                   startsWith(PPTESTCD, "FREX")) %>%
                          pull(PKNCA, PPTESTCD))
    })
    
    # Perform calculations
    analysis_result <- eventReactive(input$submit_btn, {
      req(input_pknca_data())
      data <- input_pknca_data()
      
      dose_col <- data$dose$columns$dose
      doseu <- data$dose$columns$doseu
      weight_col <- "WEIGHT"
      weightu <- "WEIGHTU"
      # Check if VOLUME and VOLUMEU columns exist
      if (!("VOLUME" %in% names(data$conc$data)) || !("VOLUMEU" %in% names(data$conc$data))) {
        showNotification("VOLUME and VOLUMEU columns are required in the data.
                         Please go back to mapping.", type = "error")
        return(NULL)
      }
      
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
      data$intervals <- format_pkncadata_intervals(data$conc,
                                                   data$dose,
                                                   params = input$param_select) %>%
        filter(PCSPEC %in% input$matrix_select)

      # Run PKNCA analysis
      results <- suppressWarnings(PKNCA::pk.nca(data, verbose = FALSE))
      
      results
    })
    
    # Render results
    output$results_table <- renderReactable({
      req(analysis_result())
      
      reactable(analysis_result()$result,
                defaultPageSize = 10,
                searchable = TRUE,
                highlight = TRUE)
      
    })
  })
}
