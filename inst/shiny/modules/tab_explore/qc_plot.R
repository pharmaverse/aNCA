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
pk_dose_qc_plot_server <- function(id, pknca_data, grouping_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data <- reactive(pknca_data()$conc$data)

    # Update inputs based on the main data
    observeEvent(pknca_data(), {
      req(pknca_data())

      conc_data <- pknca_data()$conc
      subj_col <- conc_data$columns$subject
      pcspec_col <- "PCSPEC"
      dose_col <- pknca_data()$dose$columns$dose
      route_col <- pknca_data()$dose$columns$route

      param_choices_usubjid <- unique(conc_data$data[[subj_col]])
      updatePickerInput(
        session,
        "usubjid",
        choices = param_choices_usubjid,
        selected = param_choices_usubjid
      )

      param_choices_pcspec <- unique(na.omit(conc_data$data[[pcspec_col]]))
      updatePickerInput(
        session,
        "pcspec",
        choices = param_choices_pcspec,
        selected = param_choices_pcspec
      )

      param_choices_colour <- c(dose_col, route_col)
      updatePickerInput(
        session,
        "colour_var",
        choices = param_choices_colour,
        selected = param_choices_colour[1]
      )

      param_choices_group <- grouping_vars()
      updatePickerInput(
        session,
        "group_var",
        choices = param_choices_group,
        selected = param_choices_group[1]
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

      subj_col <- pknca_data()$conc$columns$subject
      pcspec_col <- "PCSPEC"

      # Filter the conc and dose data frames
      filtered_conc <- pknca_data()$conc$data %>%
        filter(
          .[[subj_col]] %in% input$usubjid,
          .[[pcspec_col]] %in% input$pcspec
        )

      filtered_dose <- pknca_data()$dose$data %>%
        filter(.[[subj_col]] %in% input$usubjid)

      list(conc = filtered_conc, dose = filtered_dose)
    })

    # Render the PK Dose QC plot
    output$pk_dose_qc_plot <- renderPlotly({
      req(filtered_data())
      req(input$colour_var, input$group_var, input$usubjid, input$show_samples_doses)

      subj_col <- pknca_data()$conc$columns$subject
      dose_col <- pknca_data()$dose$columns$dose
      doseu_col <- pknca_data()$dose$columns$doseu

      show_pk_samples <- "PK Samples" %in% input$show_samples_doses
      show_doses <- "Doses" %in% input$show_samples_doses

      colour_var_units <- if (input$colour_var == dose_col) doseu_col else NULL

      # Adjust height based on number of subjects and groups
      height_adjust <- 200 + 20 * filtered_data()$dose %>%
        group_by(across(all_of(input$group_var))) %>%
        summarise(n = n_distinct(.[[subj_col]]), .groups = "drop") %>%
        pull(n) %>%
        max(na.rm = TRUE) * length(unique(filtered_data()$dose[, input$group_var]))

      pk_dose_qc_plot(
        data_conc = filtered_data()$conc,
        data_dose = filtered_data()$dose,
        x_var = pknca_data()$conc$columns$time,
        y_var = pknca_data()$conc$columns$subject,
        colour_var = input$colour_var,
        shape_var = "PCSPEC",
        grouping_vars = input$group_var,
        other_tooltip_vars = c("NFRLT", "DRUG"),
        x_var_units = pknca_data()$conc$columns$timeu,
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
