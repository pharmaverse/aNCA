slope_selector_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 3,
        dropdown(
          actionButton(ns("add_excsel"), "+ Exclusion/Selection"),
          actionButton(ns("remove_excsel"), "- Remove selected rows"),
          reactable.extras::reactable_extras_dependency(),
          reactableOutput(ns("manual_slopes")),
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
      profiles_per_patient(tapply(res_nca()$result$DOSNO, res_nca()$result$USUBJID, unique, simplify = FALSE))

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
    
    ## Manual Slopes Table----

    #' Object for storing exclusion and selection data for lambda slope calculation
    #' TODO(mateusz): think about better name
    manual_slopes <- reactiveVal({
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

    # Function to update data frame with input values
    update_manual_slopes <- function(manual_slopes) {
      columns_to_update <- c("TYPE", "PATIENT", "PROFILE", "IXrange", "REASON")
      last_id <- manual_slopes$id[length(manual_slopes$id)]

      for (col in columns_to_update) {
        input_id <- paste0(col, "_", last_id)
        if (!is.null(input[[input_id]])) {
          manual_slopes[nrow(manual_slopes), col] <- input[[input_id]]
        }
      }
      return(manual_slopes)
    }
    # 
    # # Observe cell edits
    # observeEvent(input$manual_slopes_cell_edit, {
    #   log_trace("{id}: saving slope edits")
    #   info <- input$manual_slopes_cell_edit
    #   manual_slopes <- manual_slopes()
    #   manual_slopes[info$row, (info$col + 1)] <- info$value
    #   manual_slopes(manual_slopes)
    # })
    
    # Add new row
    observeEvent(input$add_excsel, {
      
      row_counter(row_counter() + 1)
      id <- paste0("Ex_", row_counter())
      
      new_row <- data.frame(
        TYPE = "Selection",
        PATIENT = "",
        PROFILE = "",
        IXrange = "1:3",
        REASON = "",
        stringsAsFactors = FALSE
      )
      updated_data <- rbind(manual_slopes(), new_row)
      manual_slopes(updated_data)
    })
    
    #TODO (mateusz): figure out how to remove selected rows
    # # Remove selected rows
    # observeEvent(input$remove_excsel, {
    #   }
    # })
    
    # Render as output the table ignoring the Shiny-ID column
    output$manual_slopes <- reactable::renderReactable({
      log_trace("{id}: rendering slope edit data table")
      data <- manual_slopes()
      profiles <- unique(unlist(profiles_per_patient()))
      
      reactable(
        data = data[ , c(1:5)],
        columns = list(
          TYPE = colDef(
            cell = dropdown_extra(
              id = "dropdowntype",
              choices = c("Selection", "Exclusion"),
              class = "dropdown-extra"
            )
          ),
          PATIENT = colDef(
            cell = dropdown_extra(
              id = "dropdownpt",
              choices = names(profiles_per_patient()),
              class = "dropdown-extra"
            )
          ),
          PROFILE = colDef(
            cell = dropdown_extra(
              id = "dropdownprof",
              choices = c("", profiles),
              class = "dropdown-extra"
            )
          ),
          IXrange = colDef(
            cell = text_extra(
              id = "textrange"
            )
          ),
          REASON = colDef(
            cell = text_extra(
              id = "textreason"
            )
          )
        )
      )
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
      if (nrow(manual_slopes()) == 0) {

        # Rerun the NCA with the modified data
        mydata(mydata)

        # Stop the observeEvent
        return()
      }

      # Make previous rows values instead of input widgets
      manual_slopes <- update_manual_slopes(manual_slopes())

      # Eliminate all rows with conflicting or blank values
      manual_slopes(manual_slopes  %>%
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
      for (i in seq_len(nrow(manual_slopes()))) {
        #
        #update inclusions
        if (manual_slopes()$TYPE[i] == "Selection") {
          mydata$conc$data <- mydata$conc$data %>%
            mutate(
              is.included.hl = ifelse(
                USUBJID == manual_slopes()$PATIENT[i] &
                  DOSNO == manual_slopes()$PROFILE[i] &
                  IX %in% eval(parse(text = manual_slopes()$IXrange[i])),
                TRUE,
                is.included.hl
              ),
              REASON = ifelse(
                USUBJID == manual_slopes()$PATIENT[i] &
                  DOSNO == manual_slopes()$PROFILE[i] &
                  IX %in% eval(parse(text = manual_slopes()$IXrange[i])),
                manual_slopes()$REASON[i],
                REASON
              )
            )
        } else {
          #update exclusions
          mydata$conc$data <- mydata$conc$data %>%
            mutate(
              is.excluded.hl = ifelse(
                USUBJID == manual_slopes()$PATIENT[i] &
                  DOSNO == manual_slopes()$PROFILE[i] &
                  IX %in% eval(parse(text = manual_slopes()$IXrange[i])),
                TRUE,
                is.excluded.hl
              ),
              REASON = ifelse(
                USUBJID == manual_slopes()$PATIENT[i] &
                  DOSNO == manual_slopes()$PROFILE[i] &
                  IX %in% eval(parse(text = manual_slopes()$IXrange[i])),
                manual_slopes()$REASON[i],
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
                manual_slopes()$PATIENT == patient &
                  manual_slopes()$PROFILE == profile &
                  sapply(
                    manual_slopes()$IXrange,
                    function(x) idx_pnt %in% eval(parse(text = paste0("c(", x, ")")))
                  )
              }

              manual_slopes <- manual_slopes() %>%
                mutate(
                  IXrange = ifelse(
                    condition_vr,
                    yes = {
                      ixrange <- eval(parse(text = paste0("c(", IXrange, ")")))
                      ixrange <- ixrange[ixrange != idx_pnt]
                      paste(ixrange, collapse = ":")
                    },
                    no = IXrange
                  )
                ) %>%
                # delete all rows where IXrange does not contain a numeric value
                filter(grepl("\\d.*", IXrange))

              manual_slopes(manual_slopes)
            } else {
              # 2) If the point selected is a selection or a exclusion that was not indicated
              #    then include it also in the UI table
              row_counter(row_counter() + 1)
              id <- paste0("Ex_", row_counter())

              new_row_manual_slopes <- data.frame(
                TYPE = ifelse(idx_pnt != firstclick_vals$idx_pnt, "Selection", "Exclusion"),
                PATIENT = patient,
                PROFILE = as.character(profile),
                IXrange = paste0(firstclick_vals$idx_pnt, ":", idx_pnt),
                REASON = "[Graphical selection. Click here to include a reason]",
                id = id
              )
              manual_slopes(rbind(manual_slopes(), new_row_manual_slopes))
            }
          }
        }
      }
    })


    #' return reactive with slope exclusions data to be displayed in Results -> Exclusions tab
    return(
      reactive({
        update_manual_slopes(manual_slopes()[, c(1:5)])
      })
    )
  })
}
