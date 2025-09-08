qc_plot_ui <- function(id) {
  ns <- NS(id)
  # The nav_panel function creates the tab
  layout_sidebar(
    sidebar = sidebar(
      position = "right",
      open = TRUE,
      pickerInput(
        inputId = ns("group_var"),
        label = "Choose the variables to group by:",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        inputId = ns("colour_var"),
        label = "Choose the variables to colour by:",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        inputId = ns("pcspec"),
        label = "Select the specimen:",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        inputId = ns("usubjid"),
        label = "Select Subjects:",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        inputId = ns("show_samples_doses"),
        label = "Show:",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      )
    ),
    plotlyOutput(ns("faceted_qc_plot"))
  )
}

# -- Module Server
qc_plot_server <- function(id, data, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update inputs based on the main data
    observeEvent(data(), {
      req(data())

      param_choices_usubjid <- data() %>%
        pull(USUBJID) %>%
        unique()

      updatePickerInput(
        session,
        "usubjid",
        choices = param_choices_usubjid,
        selected = param_choices_usubjid
      )
      
      param_choices_pcspec <- data() %>%
        pull(PCSPEC) %>%
        unique()
      
      updatePickerInput(
        session,
        "pcspec",
        choices = param_choices_pcspec,
        selected = param_choices_pcspec
      )

      param_choices_colour <- c("DOSEA", "ROUTE")

      updatePickerInput(
        session,
        "colour_var",
        choices = param_choices_colour,
        selected = param_choices_colour[1]
      )

      updatePickerInput(
        session,
        "group_var",
        choices = grouping_vars(),
        selected = grouping_vars()[1]
      )

      param_choices_samples_doses <- c("PK Samples", "Doses")
      
      updatePickerInput(
        session,
        "show_samples_doses",
        choices = param_choices_samples_doses,
        selected = param_choices_samples_doses
      )
    })
    
    pknca_data <- reactive({
      req(data())
      PKNCA_create_data_object(data())
    }) |>
      bindEvent(data())

    # Render the QC plot
    output$faceted_qc_plot <- renderPlotly({
      req(pknca_data())
      req(input$colour_var, input$group_var, input$usubjid, input$show_samples_doses)

      pknca_data_conc_processed <- pknca_data()$conc$data %>%
        filter(USUBJID %in% input$usubjid,
               PCSPEC %in% c(input$pcspec))
      
      pknca_data_dose_processed <- pknca_data()$dose$data %>%
        filter(USUBJID %in% input$usubjid)
      
      show_pk_samples = "PK Samples" %in% input$show_samples_doses
      show_doses = "Doses" %in% input$show_samples_doses

      p <- faceted_qc_plot(
        data_conc = pknca_data_conc_processed,
        data_dose = pknca_data_dose_processed,
        x_var = "AFRLT",
        y_var = "USUBJID",
        colour_var = input$colour_var,
        shape_var = "PCSPEC",
        grouping_vars = input$group_var,
        other_tooltip_vars = c("NFRLT", "DRUG"),
        x_var_units = "RRLTU",
        colour_var_units = NULL,
        title = "Dose/PK Sample QC Plot",
        show_pk_samples = show_pk_samples,
        show_doses = show_doses,
        as_plotly = TRUE
      )
      p
    })
  })
}