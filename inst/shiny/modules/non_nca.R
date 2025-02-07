# UI function for the non-nca analysis
non_nca_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tabsetPanel(
      id = ns("non_nca_tabs"),
      tabPanel(
        title = "BPP Analysis",
        value = "bpp_analysis",
        card(
          card_header("BPP Analysis"),
          card_body(

          )
        )
      ),
      tabPanel(
        title = "Excretion Analysis",
        value = "excretion_analysis",
        card(
          card_header("Excretion Analysis"),
          card_body(

          )
        )
      ),
      tabPanel(
        title = "Matrix Ratio Analysis",
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
      )
    )
  )
}

# Server function for the module
non_nca_server <- function(id, data, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #Tissue-Plasma Analysis
    # Dynamically generate the tissue selection input
    output$tissue_selector <- renderUI({
      req(data())

      tissue_options <- unique(data()$PCSPEC)
      selectInput(ns("selected_tissues"), "Choose Tissues",
                  choices = tissue_options, multiple = TRUE,
                  selected = tissue_options)
    })

    output$plasma_selector <- renderUI({
      req(data())

      tissue_options <- unique(data()$PCSPEC)
      selectInput(ns("selected_plasma"), "Choose Plasma",
                  choices = tissue_options)
    })

    # Filter & prepare data for tissue-plasma ratio calculation
    filtered_samples <- reactive({
      req(data(), input$selected_tissues)

      plasma <- input$selected_plasma
      tissue <- input$selected_tissues

      df_filtered <- data() %>%
        filter(PCSPEC %in% c(input$selected_plasma, input$selected_tissues))

      df_filtered
    })

    # Perform Ratio Calculation on Submit
    ratio_results <- eventReactive(input$submit_ratio, {
      req(filtered_samples())

      plasma <- input$selected_plasma
      tissue <- input$selected_tissues
      ratio_groups <- c(grouping_vars(), "USUBJID", "ANALYTE", "DOSEA", "DOSNO", "AFRLT")
      #TODO: update this to mydata()$ obj when parameters branch merged

      # Separate Tissue and Plasma Samples
      df_plasma <- filtered_samples() %>%
        filter(PCSPEC == plasma) %>%
        rename(PLASMA_CONC = AVAL,
               PLASMA = PCSPEC) %>%
        select(ratio_groups, PLASMA_CONC)

      df_tissue <- filtered_samples() %>%
        filter(PCSPEC %in% tissue) %>%
        rename(TISSUE_CONC = AVAL) %>%
        select(ratio_groups, PCSPEC, TISSUE_CONC)

      # Merge Plasma and Tissue Data
      df_ratio <- left_join(df_tissue, df_plasma, by = ratio_groups) %>%
        filter(!is.na(TISSUE_CONC) & !is.na(PLASMA_CONC)) %>%
        mutate(
          RATIO = signif(TISSUE_CONC / PLASMA_CONC, 3)
        ) %>%
        select(ratio_groups, PCSPEC,
               TISSUE_CONC, PLASMA_CONC, RATIO) %>%
        rename(TIME = AFRLT) %>%
        arrange(USUBJID, ANALYTE, TIME)

      df_ratio
    })

    # Display results
    output$matrix_ratio_results <- renderDT({
      req(ratio_results())
      datatable(ratio_results(), options = list(pageLength = 5))
    })

  })
}
