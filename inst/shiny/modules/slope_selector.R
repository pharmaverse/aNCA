slope_selector_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 3,
        dropdown(
          actionButton(ns("add_excsel"), "+ Exclusion/Selection"),
          actionButton(ns("remove_excsel"), "- Remove selected rows"),
          DTOutput(ns("slope_manual_NCA_data")),
          actionButton(ns("save_excsel"), "Save"),
          style = "unite", icon = icon("chart-line"),
          status = "success", width = "500px",
        )
      ),
      column(
        width = 4,
        selectInput(
          ns("plots_per_page"),
          "Plots per page:",
          choices = c(2, 4, 6, 8, 10),
          selected = 2,
          width = "50%"
        ),
      ),
      column(
        width = 5,
        pickerInput(
          ns("search_patient"),
          label = "Search Patient",
          width = "70%",
          choices = NULL,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
      )
    ),
    fluidRow(
      column(4, align = "left", actionButton(ns("previous_page"), "Previous Page")),
      column(
        4, align = "center",
        #' TODO(mateusz): the 'jump to page' select input could be part of the page number
        #' display for better visuals
        uiOutput(ns("page_number")),
        selectInput(
          ns("select_page"),
          "Jump to page:",
          choices = c()
        )
      ),
      column(4, align = "right", actionButton(ns("next_page"), "Next Page"))
    ),
    uiOutput(ns("slope_plots_ui")),
    # Include details for modal message in slope_helpIcon (Instruction details)
    #' TODO(mateusz): make this work, possibly refactor
    #' NOTE: I am not namespacing it yet, as the implementation could change
    bsModal(
      "modal1",
      title = tags$a(
        style = "color: white",
        icon("chart-line"),
        "Slope Selector Guide"
      ),
      "slope_helpIcon",
      size = "large",
      div(class = "modal-body",
        p(
          class = "modal-intro",
          "Upon initial NCA run, the plots will display the optimal slope selection.
          However, you have the flexibility to change it. Remember to save/update
          your results once you are done!"
        ),
        div(class = "gif-container",
          div(class = "gif-title", "Zoom"),
          img(
            id = "zoom-gif",
            src = system.file("images/zoom_slopeselector.gif", package = "aNCA"),
            alt = "Zoom"
          )
        ),
        div(class = "gif-container",
          div(class = "gif-title", "Select"),
          img(
            id = "select-gif",
            src = system.file("images/selection_slopeselector.gif", package = "aNCA"),
            alt = "Select"
          )
        ),
        div(class = "gif-container",
          div(class = "gif-title", "Exclude"),
          img(
            id = "exclude-gif",
            src = system.file("images/exclusion_slopeselector.gif", package = "aNCA"),
            alt = "Exclude"
          )
        ),
        div(class = "gif-container",
          div(class = "gif-title", "Check"),
          img(
            id = "check-gif",
            src = system.file("images/status_slopeselector.gif", package = "aNCA"),
            alt = "Check"
          )
        )
      )
    )
  )


}

slope_selector_server <- function(
  id, mydata, res_nca, profiles_per_patient, cycle_nca
) {
  moduleServer(id, function(input, output, session) {
    log_trace("{id}: Attaching server")

    current_page <- reactiveVal(1)

    observeEvent(input$next_page, current_page(current_page() + 1))
    observeEvent(input$previous_page, current_page(current_page() - 1))
    observeEvent(input$select_page, current_page(as.numeric(input$select_page)))
    observeEvent(input$plots_per_page, current_page(1))

    observeEvent(list(
      res_nca(), input$plots_per_page, input$search_patient, current_page()
    ), {
      log_trace("{id}: Updating displayed plots")

      # Make sure the search_patient input is not NULL
      search_patient <- {
        if (is.null(input$search_patient) || length(input$search_patient) == 0) {
          unique(res_nca()$result$USUBJID)
        } else {
          input$search_patient
        }
      }

      # create plot ids based on available data #
      patient_profile_plot_ids <- mydata()$conc$data %>%
        filter(
          DOSNO %in% cycle_nca,
          USUBJID %in% search_patient
        ) %>%
        select(USUBJID, DOSNO) %>%
        unique() %>%
        arrange(USUBJID, DOSNO) %>%
        mutate(id = session$ns(paste0("slope_plot_", USUBJID, "_", DOSNO))) %>%
        pull(id)

      num_plots <- length(patient_profile_plot_ids)

      # find which plots should be displayed based on page #
      plots_per_page <- as.numeric(input$plots_per_page)
      page_end <- current_page() * plots_per_page
      page_start <- page_end - plots_per_page + 1
      if (page_end > num_plots) page_end <- num_plots

      plots_to_render <- patient_profile_plot_ids[page_start:page_end]

      # render plot outputs #
      plot_outputs <- lapply(plots_to_render, \(id) {
        plotlyOutput(id)
      })

      output$slope_plots_ui <- renderUI({
        plot_outputs
      })

      # update page number display #
      num_pages <- ceiling(num_plots / plots_per_page)
      output$page_number <- renderUI({
        tags$span(stringr::str_glue("Page {current_page()} of {num_pages}"))
      })

      # update jump to page selector #
      updateSelectInput(
        session = session,
        inputId = "select_page",
        choices = 1:num_pages,
        selected = current_page()
      )

      # disable buttons if necessary #
      shinyjs::toggleState(id = "previous_page", condition = current_page() > 1)
      shinyjs::toggleState(id = "next_page", condition = current_page() < num_pages)
    })

    observeEvent(res_nca(), {
      req(
        res_nca(),
        profiles_per_patient()
      )
      log_trace("{id}: Rendering plots")

      # TODO(mateusz): do we need to update the `profiles_per_patient()` reactiveVal?
      # or can we just have it locally?

      # Define the profiles selected (dosno) that each patient (usubjid) has
      profiles_per_patient(tapply(res_nca()$result$DOSNO, res_nca()$result$USUBJID, unique))

      # Update the patient search input to make available choices for the user
      updatePickerInput(
        session = session,
        inputId = "search_patient",
        label = "Search Patient",
        choices = unique(res_nca()$result$USUBJID)
      )

      # Generate output lambda slope plots for each patient/profile
      for (patient in unique(names(profiles_per_patient()))) {
        for (profile in profiles_per_patient()[[patient]]) {
          local({
            patient <- patient
            profile <- profile

            force(patient)  # Ensure patient is captured correctly
            force(profile)  # Ensure profile is captured correctly

            output_name <- paste0("slope_plot_", patient, "_", profile)
            output[[output_name]] <- renderPlotly({
              lambda_slope_plot(
                res_nca()$result,
                res_nca()$data$conc$data,
                profile,
                patient,
                0.7
              )
            })
          })
        }
      }
    })
  })
}