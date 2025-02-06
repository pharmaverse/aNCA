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
            p("Placeholder for BPP Analysis content.")
          )
        )
      ),
      tabPanel(
        title = "Excretion Analysis",
        value = "excretion_analysis",
        card(
          card_header("Excretion Analysis"),
          card_body(
            p("Placeholder for Excretion Analysis content.")
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
non_nca_server <- function(id, data) {
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
    
    # Filter & prepare data for tissue-plasma ratio calculation
    filtered_samples <- reactive({
      req(data(), input$selected_tissues)
      
      df <- data() %>%
        mutate(PCSPEC = tolower(PCSPEC))
      
      plasma <- grepl("^plasma$", df$PCSPEC, ignore.case = TRUE)
      tissue <- df$PCSPEC %in% tolower(input$selected_tissues)
      
      df_filtered <- df %>%
        filter(plasma | tissue) %>%
        arrange(USUBJID, ANALYTE, DOSNO, AFRLT, PCSPEC)
      
      df_filtered
    })
    
    # Perform Ratio Calculation on Submit
    ratio_results <- eventReactive(input$submit_ratio, {
      req(filtered_samples())
      
      plasma <- grepl("^plasma$", filtered_samples()$PCSPEC, ignore.case = TRUE)
      
      # Separate Tissue and Plasma Samples
      df_plasma <- filtered_samples() %>%
        filter(plasma) %>%
        rename(PLASMA_CONC = AVAL) %>%
        select(USUBJID, ANALYTE, DOSEA, DOSNO, AFRLT, PLASMA_CONC)
      
      df_tissue <- filtered_samples() %>%
        filter(!plasma) %>%
        rename(TISSUE_CONC = AVAL) %>%
        select(USUBJID, ANALYTE, DOSEA, DOSNO, AFRLT, PCSPEC, TISSUE_CONC)
      
      # Merge Plasma and Tissue Data
      df_ratio <- left_join(df_tissue, df_plasma, by = c("USUBJID", "ANALYTE", "DOSEA", "DOSNO", "AFRLT")) %>%
        filter(!is.na(TISSUE_CONC) & !is.na(PLASMA_CONC)) %>% 
        mutate(
          RATIO = signif(TISSUE_CONC / PLASMA_CONC, 3),
          RATIO_NAME = paste(PCSPEC, "- plasma", sep = " ")
        ) %>%
        rename(TIME = AFRLT)%>%
        select(ANALYTE, DOSEA, DOSNO, TIME, USUBJID, 
               TISSUE_CONC, PLASMA_CONC, RATIO_NAME, RATIO) %>%
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
