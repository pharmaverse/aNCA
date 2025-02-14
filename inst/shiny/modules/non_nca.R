#' This module provides a user interface and server function for additional analysis outside of NCA
#' calculations. It includes functions for blood-plasma partitioning, matrix ratios, excretion,
#' AUC ratios, and metabolite-parent ratios.
#' @param id A character string used to uniquely identify the module.
#' @param data A PKNCA data object that returns the data list containing the concentration and
#' dose data.
#' @param grouping_vars A character vector of grouping variables to use for the analysis.

# UI function for the non-nca analysis
non_nca_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tabsetPanel(
      id = ns("non_nca_tabs"),
      tabPanel(
        title = "Blood-Plasma Partitioning",
        value = "bpp_analysis",
        card(
          card_header("BPP Analysis"),
          card_body(
            uiOutput(ns("blood_selector")),
            uiOutput(ns("plasmaforbpp_selector")),
            p("Timepoints will automatically be selected by
              taking the shared timeponts for both variables"),
            actionButton(ns("submit_bpp"), "Submit", class = "btn-primary")

          )
        ),
        card(
          card_header("BPP Results"),
          card_body(
            DTOutput(ns("bpp_results"))
          )
        )
      ),
      tabPanel(
        title = "Matrix Ratios",
        value = "matrix_ratio_analysis",
        card(
          card_header("Matrix Ratio Setup"),
          card_body(
            uiOutput(ns("tissue_selector")),
            uiOutput(ns("plasma_selector")),
            actionButton(ns("submit_ratio"), "Submit", class = "btn-primary")
          )
        ),
        card(
          card_header("Matrix Ratio Results"),
          card_body(
            DTOutput(ns("matrix_ratio_results"))
          )
        )
      ),
      tabPanel(
        title = "Excretion",
        value = "excretion_analysis",
        card(
          card_header("Excretion Analysis"),
          card_body(

          )
        )
      ),
      tabPanel(
        title = "AUC Ratios",
        value = "auc_analysis",
        card(
          card_header("Bioavailability Calculations"),
          card_body(

          )
        ),
        card(
          card_header("AUC profile ratios"),
          card_body(

          )
        )
      ),
      tabPanel(
        title = "Metabolite-Parent Ratios",
        value = "metabolite_analysis",
        card(
          card_header("Metabolite-Parent Ratios"),
          card_body(

          )
        )
      ),
    )
  )
}

# Server function for the module
non_nca_server <- function(id, data, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #TODO: Remove hardcoding of columns ?? Ensure only non-distinct grouping columns are shown ?
    # BPP Analysis #########################################
    output$blood_selector <- renderUI({
      req(data())
      blood_options <- unique(data()$conc$data$PCSPEC)
      selectInput(ns("selected_blood"), "Choose Blood column for BPP",
                  choices = blood_options)
    })

    output$plasmaforbpp_selector <- renderUI({

      req(data())
      plasma_options <- unique(data()$conc$data$PCSPEC)
      selectInput(ns("selected_plasmaforbpp"), "Choose Plasma column for BPP",
                  choices = plasma_options)
    })


    # Filter & prepare data for BPP calculation
    filtered_samples_bpp <- reactive({
      req(input$selected_blood, input$selected_plasmaforbpp)

      data()$conc$data %>%
        filter(PCSPEC %in% c(input$selected_blood, input$selected_plasmaforbpp))

    })

    # Perform BPP Calculation on Submit
    bpp_results <- eventReactive(input$submit_bpp, {
      req(filtered_samples_bpp())

      blood <- input$selected_blood
      plasma <- input$selected_plasmaforbpp

      id_groups <- data()$conc$columns$groups %>%
        purrr::list_c() %>%
        append("DOSNO") %>%
        purrr::keep(\(col) {
          !is.null(col) && col != "PCSPEC" && length(unique(data()$conc$data[[col]])) > 1
        })

      bpp_groups <- c(grouping_vars(), id_groups,
                      data()$dose$columns$dose, data()$dose$columns$time,
                      data()$dose$columns$route)

      # Separate Blood and Plasma Samples
      df_blood <- filtered_samples_bpp() %>%
        filter(PCSPEC == blood) %>%
        rename(BLOOD_CONC = data()$conc$columns$concentration) %>%
        select(bpp_groups, BLOOD_CONC)

      df_plasma <- filtered_samples_bpp() %>%
        filter(PCSPEC == plasma) %>%
        rename(PLASMA_CONC = data()$conc$columns$concentration) %>%
        select(bpp_groups, PLASMA_CONC)

      # Merge Blood and Plasma Data
      df_bpp <- left_join(df_blood, df_plasma, by = bpp_groups) %>%
        filter(!is.na(BLOOD_CONC) & !is.na(PLASMA_CONC)) %>%
        mutate(
          BPP_RATIO = signif(BLOOD_CONC / PLASMA_CONC, 3)
        ) %>%
        select(bpp_groups, PLASMA_CONC, BLOOD_CONC, BPP_RATIO) %>%
        arrange(USUBJID, TIME)

      df_bpp
    })

    # Display results
    output$bpp_results <- renderDT({
      req(bpp_results())
      datatable(bpp_results(), options = list(pageLength = 5))
    })

    #Tissue-Plasma Analysis ###############################
    # Dynamically generate the tissue selection input
    output$tissue_selector <- renderUI({
      req(data())

      tissue_options <- unique(data()$conc$data$PCSPEC)
      selectInput(ns("selected_tissues"), "Choose Tissues",
                  choices = tissue_options, multiple = TRUE,
                  selected = tissue_options)
    })

    output$plasma_selector <- renderUI({
      req(data())

      tissue_options <- unique(data()$conc$data$PCSPEC)
      selectInput(ns("selected_plasma"), "Choose Plasma",
                  choices = tissue_options)
    })

    # Filter & prepare data for tissue-plasma ratio calculation
    filtered_samples <- reactive({
      req(data(), input$selected_tissues)

      plasma <- input$selected_plasma
      tissue <- input$selected_tissues

      df_filtered <- data()$conc$data %>%
        filter(PCSPEC %in% c(input$selected_plasma, input$selected_tissues))

      df_filtered
    })

    # Perform Ratio Calculation on Submit
    ratio_results <- eventReactive(input$submit_ratio, {
      req(filtered_samples())

      plasma <- input$selected_plasma
      tissue <- input$selected_tissues

      id_groups <- data()$conc$columns$groups %>%
        purrr::list_c() %>%
        append("DOSNO") %>%
        purrr::keep(\(col) {
          !is.null(col) && col != "PCSPEC" && length(unique(data()$conc$data[[col]])) > 1
        })

      ratio_groups <- c(grouping_vars(), id_groups,
                        data()$dose$columns$dose, data()$dose$columns$time)

      # Separate Tissue and Plasma Samples
      df_plasma <- filtered_samples() %>%
        filter(PCSPEC == plasma) %>%
        rename(PLASMA_CONC = data()$conc$columns$concentration) %>%
        select(ratio_groups, PLASMA_CONC)

      df_tissue <- filtered_samples() %>%
        filter(PCSPEC %in% tissue) %>%
        rename(TISSUE_CONC = data()$conc$columns$concentration) %>%
        select(ratio_groups, PCSPEC, TISSUE_CONC)

      # Merge Plasma and Tissue Data
      df_ratio <- left_join(df_tissue, df_plasma, by = ratio_groups) %>%
        filter(!is.na(TISSUE_CONC) & !is.na(PLASMA_CONC)) %>%
        mutate(
          RATIO = signif(TISSUE_CONC / PLASMA_CONC, 3)
        ) %>%
        select(ratio_groups, PCSPEC,
               TISSUE_CONC, PLASMA_CONC, RATIO) %>%
        arrange(USUBJID, TIME)

      df_ratio
    })

    # Display results
    output$matrix_ratio_results <- renderDT({
      req(ratio_results())
      datatable(ratio_results(), options = list(pageLength = 5))
    })

  })
}
