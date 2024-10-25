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
  id, mydata, res_nca, profiles_per_patient, cycle_nca, rv
) {
  moduleServer(id, function(input, output, session) {
    log_trace("{id}: Attaching server")

    current_page <- reactiveVal(1)

    #' updating current page based on user input
    observeEvent(input$next_page, current_page(current_page() + 1))
    observeEvent(input$previous_page, current_page(current_page() - 1))
    observeEvent(input$select_page, current_page(as.numeric(input$select_page)))
    observeEvent(list(input$plots_per_page, input$search_patient), current_page(1))

    #' Updating plot outputUI, dictating which plots get displayed to the user.
    #' Scans for any related reactives (page number, patient filter etc) and updates the plot output
    #' UI to have only plotlyOutput elements for desired plots.
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

    #' Rendering slope plots based on nca data.
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

    #' Object for storing exclusion and selection data for lambda slope calculation
    #' TODO(mateusz): think about better name
    slope_manual_nca_data <- reactiveVal({
      data.frame(
        TYPE = character(),
        PATIENT = character(),
        PROFILE = character(),
        IXrange = character(),
        REASON = character(),
        id = character()
      )
    })

    row_counter <- reactiveVal(0)

    # Render as output the table ignoring the Shiny-ID column
    output$slope_manual_NCA_data <- DT::renderDataTable({
      log_trace("{id}: rendering slope edit data table")
      datatable(
        data = slope_manual_nca_data()[, c(1:5)],
        escape = FALSE,
        rownames = FALSE,
        editable = TRUE,
        extensions = "FixedHeader",
        options = list(
          paging = FALSE,
          ordering = FALSE,
          searching = FALSE,
          fixedHeader = TRUE,
          preDrawCallback = JS("function() { Shiny.unbindAll(this.api().table().node()); }"),
          drawCallback = JS("function() { Shiny.bindAll(this.api().table().node()); } ")
        )
      )
    })

    #' Update object with edits
    observeEvent(input$slope_manual_NCA_data_cell_edit, {
      log_trace("{id}: saving slope edits")
      info <- input$slope_manual_NCA_data_cell_edit

      # Update the reactive data frame with the new value
      slope_manual_nca_data <- slope_manual_nca_data()
      slope_manual_nca_data[info$row, (info$col + 1)] <- info$value
      slope_manual_nca_data(slope_manual_nca_data)
    })

    # Define a function that saves the inputs as values in
    # the table once the user has finished inputting them
    update_slope_manual_nca_data <- function(slope_manual_nca_data) {
      # Define the columns to be updated
      columns_to_update <- c("TYPE", "PATIENT", "PROFILE", "IXrange", "REASON")

      # Retrieve the last row"s ID for input prefix
      last_id <- slope_manual_nca_data$id[length(slope_manual_nca_data$id)]

      # Update each specified column with the corresponding input value
      for (col in columns_to_update) {
        input_id <- paste0(col, "_", last_id)
        if (!is.null(input[[input_id]])) {
          slope_manual_nca_data[nrow(slope_manual_nca_data), col] <- input[[input_id]]
        }
      }

      # The function returns the object
      return(slope_manual_nca_data)
    }

    observeEvent(input$add_excsel, {
      # Make previous rows values instead of input widgets
      slope_manual_nca_data <- update_slope_manual_nca_data(slope_manual_nca_data())

      # Create an ID for the new row
      row_counter(row_counter() + 1)
      id <- paste0("Ex_", row_counter())

      # Create the new row as a set of input widgets for the UI
      new_row_slope_manual_nca_data <- data.frame(
        TYPE = as.character(
          selectInput(
            inputId = session$ns(paste0("TYPE_", id)),
            label = "",
            width = "60%",
            choices = c("Selection", "Exclusion"),
            selected = "Selection",
            selectize = TRUE
          )
        ),
        PATIENT = as.character(
          selectInput(
            inputId = session$ns(paste0("PATIENT_", id)),
            label = "",
            width = "60%",
            choices = names(profiles_per_patient()),
            selectize = TRUE
          )
        ),
        PROFILE = as.character(
          selectInput(
            inputId = session$ns(paste0("PROFILE_", id)),
            label = "",
            width = "60%",
            choices = unname(unique(unlist(profiles_per_patient()))),
            selectize = TRUE
          )
        ),
        IXrange = as.character(
          textInput(
            inputId = session$ns(paste0("IXrange_", id)),
            label = "",
            width = "70%",
            value = "1:3"
          )
        ),
        REASON = as.character(
          textInput(
            inputId = session$ns(paste0("REASON_", id)),
            value = "",
            label = "",
            width = "80%"
          )
        ),
        id = id
      )

      # Bind the row to the reactive value displayed in the UI
      slope_manual_nca_data(rbind(slope_manual_nca_data, new_row_slope_manual_nca_data))
    })

    # Allow the user to delete the rows selected in the UI table
    observeEvent(input$remove_excsel, {
      if (!is.null(input$slope_manual_NCA_data_rows_selected)) {
        # Make previous rows values instead of input widgets
        slope_manual_nca_data <- update_slope_manual_nca_data(slope_manual_nca_data())

        # Reset the variable deleting the selected rows
        slope_manual_nca_data(
          slope_manual_nca_data[-as.numeric(input$slope_manual_NCA_data_rows_selected), ]
        )
      }
    })

    # Save the exclusion/selection data to the server data and rerun the NCA results
    handle_nca_excsel <- function() {
      if (is.null(mydata())) {
        showNotification(
          "Data issue: Please select an Analyte in the Data Selection tab",
          duration = NULL,
          closeButton = TRUE,
          type = "error"
        )
        return(NULL)
      }

      mydata <- mydata()

      # Reset to 0 all previous (if done) changes
      mydata$conc$data$is.included.hl <- FALSE
      mydata$conc$data$is.excluded.hl <- FALSE
      mydata$conc$data$exclude_half.life <- FALSE

      # If there is no specification there is nothing to save
      if (nrow(slope_manual_nca_data()) == 0) {

        # Rerun the NCA with the modified data
        mydata(mydata)

        # Stop the observeEvent
        return()
      }

      # Make previous rows values instead of input widgets
      slope_manual_nca_data <- update_slope_manual_nca_data(slope_manual_nca_data())

      # Eliminate all rows with conflicting or blank values
      slope_manual_nca_data(slope_manual_nca_data  %>%
        filter(
          PATIENT %in% names(profiles_per_patient()),
          PROFILE %in% unname(unlist(profiles_per_patient()[PATIENT])),
          all(!is.na(sapply(IXrange, function(x) eval(parse(text = x))))) &
            all(!is.null(sapply(IXrange, function(x) eval(parse(text = x))))),
        )  %>%
        # Eliminate duplicated records within the same profile
        filter(
          !duplicated(
            paste0(PATIENT, PROFILE, IXrange, fromLast = TRUE),
            !(duplicated(paste0(PATIENT, PROFILE), fromLast = TRUE))
          )
        )
      )

      # Update the exclusion/selection data for Lambda based on the current exc/sel table
      for (i in seq_len(nrow(slope_manual_nca_data()))) {
        #
        #update inclusions
        if (slope_manual_nca_data()$TYPE[i] == "Selection") {
          mydata$conc$data <- mydata$conc$data %>%
            mutate(
              is.included.hl = ifelse(
                USUBJID == slope_manual_nca_data()$PATIENT[i] &
                  DOSNO == slope_manual_nca_data()$PROFILE[i] &
                  IX %in% eval(parse(text = slope_manual_nca_data()$IXrange[i])),
                TRUE,
                is.included.hl
              ),
              REASON = ifelse(
                USUBJID == slope_manual_nca_data()$PATIENT[i] &
                  DOSNO == slope_manual_nca_data()$PROFILE[i] &
                  IX %in% eval(parse(text = slope_manual_nca_data()$IXrange[i])),
                slope_manual_nca_data()$REASON[i],
                REASON
              )
            )
        } else {
          #update exclusions
          mydata$conc$data <- mydata$conc$data %>%
            mutate(
              is.excluded.hl = ifelse(
                USUBJID == slope_manual_nca_data()$PATIENT[i] &
                  DOSNO == slope_manual_nca_data()$PROFILE[i] &
                  IX %in% eval(parse(text = slope_manual_nca_data()$IXrange[i])),
                TRUE,
                is.excluded.hl
              ),
              REASON = ifelse(
                USUBJID == slope_manual_nca_data()$PATIENT[i] &
                  DOSNO == slope_manual_nca_data()$PROFILE[i] &
                  IX %in% eval(parse(text = slope_manual_nca_data()$IXrange[i])),
                slope_manual_nca_data()$REASON[i],
                REASON
              )
            )
        }
      }
      mydata$conc$data <- mydata$conc$data %>%
        group_by(STUDYID, USUBJID, PCSPEC, DOSNO) %>%
        mutate(
          exclude_half.life = {
            if (any(is.included.hl)) {
              is.excluded.hl | !is.included.hl
            } else {
              is.excluded.hl
            }
          }
        )

      mydata(mydata)
    }

    # Observe input$nca
    observeEvent(profiles_per_patient(), {
      handle_nca_excsel()
    })

    # Observe input$save_excsel
    observeEvent(input$save_excsel, {
      handle_nca_excsel()
      rv$trigger <- rv$trigger + 1
    })

    mydata2 <- reactiveVal()
    observeEvent(mydata(), mydata2(mydata()))

    # Define the click events for the point exclusion and selection in the slope plots
    click_counter <- reactiveVal(0)
    firstclick_vals <- reactiveValues(patient = NULL, profile = NULL, idx_pnt = NULL)

    observeEvent(event_data("plotly_click", priority = "event"), {
      log_trace("{id}: plotly click detected")
      # Store the information of the last click event
      click_data <- event_data("plotly_click")
      if (!is.null(click_data) & !is.null(click_data$customdata)) {
        # Get identifiers of the clicked plot
        patient <- gsub("(.*)_.*_.*", "\\1",  click_data$customdata)
        profile <- gsub(".*_(.*)_.*", "\\1",  click_data$customdata)
        idx_pnt <- gsub(".*_.*_(.*)", "\\1",  click_data$customdata)

        # Increment the click counter
        click_counter(click_counter() + 1)

        if (click_counter() %% 2 == 1) {
          log_trace("{id}: first click")
          firstclick_vals$patient <- patient
          firstclick_vals$profile <- profile
          firstclick_vals$idx_pnt <- idx_pnt
        }

        # When second click happens in the plot an event should occur
        if (click_counter() %% 2 == 0 & click_counter() > 0) {
          log_trace("{id}: second click, adding new selection rule")
          # If the user clicks another plot after one click, reset everything and start over
          if (patient != firstclick_vals$patient | profile != firstclick_vals$profile) {
            click_counter(1)
            firstclick_vals$patient <- patient
            firstclick_vals$profile <- profile
            firstclick_vals$idx_pnt <- idx_pnt

            # If the user clicked in the same plot, perform an action over the temporary data
          } else {
            # Define a temporary data that does not affect the original until is
            # saved by the user (save_excsel)
            mydata2 <- mydata2()

            # Modify the data for the plot according to the user's clicks
            mydata2$conc$data <- mydata2$conc$data %>%
              # If the user clicked two different points, do their selection
              mutate(
                is.included.hl = case_when(
                  idx_pnt == firstclick_vals$idx_pnt ~ is.included.hl,
                  USUBJID == patient &
                    DOSNO == profile & IX %in% firstclick_vals$idx_pnt:idx_pnt ~ TRUE,
                  TRUE ~ FALSE
                ),
                # If the user clicked two times the same point, do its exclusion
                is.excluded.hl = case_when(
                  idx_pnt != firstclick_vals$idx_pnt ~ is.excluded.hl,
                  USUBJID == patient & DOSNO == profile & IX %in% idx_pnt ~ !is.excluded.hl,
                  TRUE ~ is.excluded.hl
                )
              ) %>%
              group_by(STUDYID, USUBJID, PCSPEC, DOSNO) %>%
              mutate(
                exclude_half.life = {
                  if (any(is.included.hl)) {
                    is.excluded.hl | !is.included.hl
                  } else {
                    is.excluded.hl
                  }
                }
              )
            mydata2(mydata2)

            # Change the plot of only the profile and patient selected
            mydata2$conc$data <- mydata2$conc$data %>% filter(USUBJID == patient, DOSNO == profile)
            mydata2$dose$data <- mydata2$dose$data %>% filter(USUBJID == patient, DOSNO == profile)
            myres2 <- suppressWarnings(PKNCA::pk.nca(data = mydata2, verbose = FALSE))

            # Alter the output with the transitory changes and the new slope plot
            output[[paste0("slopetestplot_", patient, "_", profile)]] <- renderPlotly({
              lambda_slope_plot(
                myres2$result,
                myres2$data$conc$data,
                profile,
                patient,
                ifelse(input$rule_adj.r.squared, input$adj.r.squared_threshold, 0.7)
              )
            })

            ## Make UI changes in the table displayed
            # 1) If the point selected is a exclusion that was already indicated, then remove
            #    previous records from the UI table and stop the observeEvent
            if (idx_pnt == firstclick_vals$idx_pnt &&
              all(!mydata2$conc$data$is.excluded.hl[
                mydata2$conc$data$USUBJID == patient &
                  mydata2$conc$data$DOSNO == profile &
                  mydata2$conc$data$IX == idx_pnt
              ])) {

              condition_vr <- {
                slope_manual_nca_data()$PATIENT == patient &
                  slope_manual_nca_data()$PROFILE == profile &
                  sapply(
                    slope_manual_nca_data()$IXrange,
                    function(x) idx_pnt %in% eval(parse(text = paste0("c(", x, ")")))
                  )
              }

              slope_manual_nca_data <- slope_manual_nca_data() %>%
                mutate(
                  IXrange = ifelse(
                    condition_vr,
                    yes = {
                      ixrange <- eval(parse(text = paste0("c(", IXrange, ")")))
                      ixrange <- ixrange[ixrange != idx_pnt]
                      paste(ixrange, collapse = ",")
                    },
                    no = IXrange
                  )
                ) %>%
                # delete all rows where IXrange does not contain a numeric value
                filter(grepl("\\d.*", IXrange))

              slope_manual_nca_data(slope_manual_nca_data)
            } else {
              # 2) If the point selected is a selection or a exclusion that was not indicated
              #    then include it also in the UI table
              row_counter(row_counter() + 1)
              id <- paste0("Ex_", row_counter())

              new_row_slope_manual_nca_data <- data.frame(
                TYPE = ifelse(idx_pnt != firstclick_vals$idx_pnt, "Selection", "Exclusion"),
                PATIENT = patient,
                PROFILE = as.character(profile),
                IXrange = paste0(firstclick_vals$idx_pnt, ":", idx_pnt),
                REASON = "[Graphical selection. Double click here to include a reason]",
                id = id
              )
              slope_manual_nca_data(rbind(slope_manual_nca_data(), new_row_slope_manual_nca_data))
            }
          }
        }
      }
    })


    #' return reactive with slope exclusions data to be displayed in Results -> Exclusions tab
    return(
      reactive({
        update_slope_manual_nca_data(slope_manual_nca_data()[, c(1:5)])
      })
    )
  })
}