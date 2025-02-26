#' This module provides a user interface and server function for additional analysis outside of NCA
#' calculations. It includes functions for blood-plasma partitioning, matrix ratios, excretion,
#' AUC ratios, and metabolite-parent ratios.
#' @param id A character string used to uniquely identify the module.
#' @param data A PKNCA data object that returns the data list containing the concentration and
#' dose data.
#' @param grouping_vars A character vector of grouping variables to use for the analysis.

# UI function for the non-nca analysis
additional_analysis_ui <- function(id) {
  ns <- NS(id)

  navset_pill(
    id = ns("non_nca_tabs"),
    nav_panel(
      title = "Concentration Ratios",
      value = "matrix_ratio_analysis",
      non_nca_ratio_ui(
        ns("matrix_ratio_analysis"),
        title = "Matrix Ratios",
        select_label1 = "Choose Numerator Specimens",
        select_label2 = "Choose Denominator Specimens"
      )
    ),
    nav_panel(
      title = "Excretion",
      value = "excretion_analysis",
      card(
        card_header("Excretion Analysis"),
        card_body(

          p("To be added")
        )
      )
    ),
    nav_panel(
      title = "AUC Ratios",
      value = "auc_analysis",
      card(
        card_header("Bioavailability Calculations"),
        card_body(
          fluidRow(
            p("Bioavailability is calculated as Extravascular_AUC / Intravascular_AUC."),
            selectInput(ns("select_aucs"), "Select AUC types",
                        choices = NULL,
                        multiple = TRUE),
            selectInput(ns("select_grouping"), "Group Summary by:",
                        choices = NULL,
                        multiple = TRUE)
          ),
          actionButton(ns("calc_auc_ratios"), "Calculate"),
          p("The AUC ratios are calculated with AUCs for individual subjects
              (if both routes are available), or using the Mean AUC for IV"),
          br(),
          h3("Results"),
          tableOutput(ns("auc_table")),
        )
      ),
    ),
    nav_panel(
      title = "Metabolite-Parent Ratios",
      value = "metabolite_analysis",
      card(
        card_header("Metabolite-Parent Ratios"),
        card_body(

          p("To be added")
        )
      )
    )
  )
}

# Server function for the module
additional_analysis_server <- function(id, data, res_nca, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Call module for Matrix Ratio Analysis
    non_nca_ratio_server(id = "matrix_ratio_analysis", data, grouping_vars)
    
    # Bioavailability calculations
      
    # Extract route information and add to AUC data
    id_groups <- reactive({
       res_nca()$data$conc$columns$groups %>%
      purrr::list_c() %>%
      append("DOSNO") %>%
      purrr::keep(\(col) {
        !is.null(col) && length(unique(data()$conc$data[[col]])) > 1
      })
    })
    
    auc_data <- reactive({
      req(res_nca())

      # transform the dataset
      auc_data <- pivot_wider_pknca_results(res_nca())
        
      route_col <- res_nca()$data$dose$columns$route
      dose_col <- res_nca()$data$dose$columns$dose

      dose_info <- res_nca()$data$dose$data%>%
        select(all_of(c(id_groups(), grouping_vars())), route_col, dose_col) %>%
        distinct()

      auc_data %>%
        inner_join(dose_info, by = id_groups(), relationship = "many-to-many") %>%
        rename(Route = route_col, Dose = dose_col)
    })
    # Update the auc choices dynamically
    observeEvent(auc_data(), {
      
      auc_choices <- unique(names(auc_data())[grepl("auc", names(auc_data()))])
      updateSelectInput(session, "select_aucs", choices = auc_choices)
      
      updateSelectInput(session, "select_grouping", choices = grouping_vars())
    })
    
    observeEvent(input$calc_auc_ratios, {
      data <- auc_data()
      selected_aucs <- input$select_aucs
      group_vars <- input$select_grouping
      #browser()
      results_list <- list()
      
      for (auc_type in selected_aucs) {

        data <- data %>%
          mutate(grouping = apply(select(., all_of(c(id_groups(), group_vars)), -USUBJID),
                                  1, paste, collapse = "_"))
        # Separate IV and EX data for the current AUC type
        iv_data <- data %>%
          filter(tolower(Route) == "intravascular") %>%
          rename(AUC_IV = !!sym(auc_type),
                 Dose_IV = Dose,
                 Grouping_IV = grouping) %>%
          select(USUBJID, Grouping_IV, AUC_IV, Dose_IV)
        
        ex_data <- data %>%
          filter(tolower(Route) == "extravascular") %>%
          rename(AUC_EX = !!sym(auc_type),
                 Dose_EX = Dose,
                 Grouping_EX = grouping)%>%
          select(USUBJID, Grouping_EX, AUC_EX, Dose_EX)
        
        # Merge IV and EX by USUBJID (for direct comparisons)
        merged_data <- left_join(ex_data, iv_data, by = c("USUBJID"))
        
        # 1. Compute bioavailability (F) for subjects with both EX and IV data
        individual_data <- merged_data %>%
          filter(!is.na(AUC_IV) & !is.na(Dose_IV)) %>%  # Ensure IV data exists
          mutate(
            f = pk.calc.f(Dose_IV, AUC_IV, Dose_EX, AUC_EX),
            Percentage = f * 100,
            Type = "Individual",
            AUC_Type = auc_type
          )
        
        # 2. Compute mean IV AUC for subjects with EX but no matching IV
        ex_without_match <- merged_data %>%
          filter(is.na(AUC_IV) | is.na(Dose_IV))  # Keep only subjects missing IV
        
        mean_iv <- iv_data %>%
          group_by(Grouping_IV) %>%
          summarize(
            Mean_AUC_IV = mean(AUC_IV, na.rm = TRUE),
            Mean_Dose_IV = mean(Dose_IV, na.rm = TRUE),
            .groups = "drop"
          )
        
        if (nrow(ex_without_match) > 0) {
          
          ex_without_match <- ex_without_match %>%
            mutate(
              f = pk.calc.f(mean_iv$Mean_Dose_IV, mean_iv$Mean_AUC_IV, Dose_EX, AUC_EX),
              Percentage = f * 100,
              Type = "Ind/Mean",
              AUC_Type = auc_type
            )
        }
        
        mean_ex <- ex_data %>%
          group_by(Grouping_EX) %>%
          summarize(
            Mean_AUC_EX = mean(AUC_EX, na.rm = TRUE),
            Mean_Dose_EX = mean(Dose_EX, na.rm = TRUE),
            .groups = "drop"
          )
        
        mean_summary <- cross_join(mean_ex, mean_iv) %>%
          mutate(
            f = pk.calc.f(Mean_Dose_IV, Mean_AUC_IV, Mean_Dose_EX, Mean_AUC_EX),
            Percentage = f * 100,
            Type = "Mean",
            AUC_Type = auc_type
          ) %>%
          select(Grouping_EX, Grouping_IV, AUC_Type, f, Percentage, Type)
        
        browser()
        # Combine individual and mean-based results for this AUC type
        auc_results <- bind_rows(
          individual_data %>% select(USUBJID, Grouping_EX, Grouping_IV, AUC_Type, f, Percentage, Type),
          ex_without_match %>% select(USUBJID, Grouping_EX, Grouping_IV, AUC_Type, f, Percentage, Type),
          mean_summary
        )
        
        # Append to results list
        results_list[[auc_type]] <- auc_results
      }
      
      final_results <- bind_rows(results_list)
      
      # Render results table
      output$auc_table <- renderTable({
        req(final_results)
        final_results
      })
      
    })
    
  })
}
