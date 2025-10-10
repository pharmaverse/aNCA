#' Module UI for the Dose/PK Sample QC Plot.
#'
#' @details
#' Creates the user interface for the QC plot tab. This includes input
#' controls for selecting subjects, specimens, grouping variables, and coloring
#' variables. It also defines the output area for the interactive plot.
#'
#' @param id A character string giving the namespace for the module.
#'
#' @returns A UI element representing the QC plot tab.

pk_dose_qc_plot_ui <- function(id) {
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
    plotlyOutput(ns("pk_dose_qc_plot"), height = "100%")
  )
}

# -- Module Server
pk_dose_qc_plot_server <- function(id, pknca_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data <- reactive(pknca_data()$conc$data)

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
        filter(!is.na(PCSPEC)) %>%
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
        choices = group_vars(pknca_data()$conc),
        selected = group_vars(pknca_data()$conc)[1]
      )

      param_choices_samples_doses <- c("PK Samples", "Doses")

      updatePickerInput(
        session,
        "show_samples_doses",
        choices = param_choices_samples_doses,
        selected = param_choices_samples_doses
      )

    })

    filtered_data <- reactive({
      req(pknca_data(), input$usubjid, input$pcspec)

      # Filter the conc and dose data frames
      filtered_conc <- pknca_data()$conc$data %>%
        filter(
          USUBJID %in% input$usubjid,
          PCSPEC %in% input$pcspec
        )

      filtered_dose <- pknca_data()$dose$data %>%
        filter(USUBJID %in% input$usubjid)

      list(conc = filtered_conc, dose = filtered_dose)
    })

    # Render the PK Dose QC plot
    output$pk_dose_qc_plot <- renderPlotly({
      req(filtered_data())
      req(input$colour_var, input$group_var, input$usubjid, input$show_samples_doses)

      show_pk_samples <- "PK Samples" %in% input$show_samples_doses
      show_doses <- "Doses" %in% input$show_samples_doses

      colour_var_units <- if (input$colour_var == "DOSEA") "DOSEU" else NULL

      # Adjust height based on number of subjects and groups
      height_adjust <- 200 + 20 * filtered_data()$dose %>%
        group_by(across(all_of(input$group_var))) %>%
        summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
        pull(n) %>%
        max(na.rm = TRUE) * length(unique(filtered_data()$dose[, input$group_var]))

      pk_dose_qc_plot(
        data_conc = filtered_data()$conc,
        data_dose = filtered_data()$dose,
        x_var = "AFRLT",
        y_var = "USUBJID",
        colour_var = input$colour_var,
        shape_var = "PCSPEC",
        grouping_vars = input$group_var,
        other_tooltip_vars = c("NFRLT", "DRUG"),
        x_var_units = "RRLTU",
        colour_var_units = colour_var_units,
        title = "Dose and Sample Events",
        show_pk_samples = show_pk_samples,
        show_doses = show_doses,
        as_plotly = TRUE,
        height = max(c(1000, height_adjust))
      ) %>%
        layout(xaxis = list(rangeslider = list(type = "time")))
    })
  })

}
