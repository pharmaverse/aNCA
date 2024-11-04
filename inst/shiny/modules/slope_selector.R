slope_selector_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "slope-selector-module",
    includeCSS(file.path(system.file("shiny/www", package = "aNCA"), "slope_selector.css")),
    fluidRow(
      actionButton(ns("add_excsel"), "+ Exclusion/Selection"),
      actionButton(ns("remove_excsel"), "- Remove selected rows"),
      actionButton(ns("save_excsel"), "Apply"),
      dropdown(
        tags$div("Here will be manual"),
        style = "unite",
        right = TRUE,
        icon = icon("question"),
        status = "primary"
      )
    ),
    fluidRow(
      reactableOutput(ns("manual_slopes")),
    ),
    fluidRow(
      class = "plot-widgets-container",
      div(
        tags$span(
          class = "inline-select-input",
          tags$label(
            "Plots per page: "
          ),
          selectInput(
            ns("plots_per_page"),
            "",
            choices = c(2, 4, 6, 8, 10),
            selected = 2
          )
        )
      ),
      div(
        pickerInput(
          ns("search_patient"),
          label = "Search Patient",
          choices = NULL,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
      ),
      div(align = "left", actionButton(ns("previous_page"), "Previous Page")),
      div(
        align = "center",
        tags$span(
          class = "inline-select-input",
          tags$span("Page "),
          selectInput(ns("select_page"), "", choices = c()),
          tags$span(" out of "),
          uiOutput(ns("page_number"), inline = TRUE)
        )
      ),
      div(align = "right", actionButton(ns("next_page"), "Next Page"))
    ),
    uiOutput(ns("slope_plots_ui"), class = "slope-plots-container"),
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

.SLOPE_SELECTOR_COLUMNS <- c("TYPE", "PATIENT", "PROFILE", "IXrange", "REASON")

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
      output$page_number <- renderUI(num_pages)

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
      profiles_per_patient(
        tapply(res_nca()$result$DOSNO, res_nca()$result$USUBJID, unique, simplify = FALSE)
      )

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

    #' Adds new row to the selection/exclusion datatable
    observeEvent(input$add_excsel, {
      log_trace("{id}: adding manual slopes row")

      new_row <- data.frame(
        TYPE = "Selection",
        PATIENT = names(profiles_per_patient())[1],
        PROFILE = unique(unlist(profiles_per_patient()))[1],
        IXrange = "1:3",
        REASON = "",
        stringsAsFactors = FALSE
      )
      updated_data <- rbind(manual_slopes(), new_row)
      manual_slopes(updated_data)
    })

    #' Removes selected row
    observeEvent(input$remove_excsel, {
      log_trace("{id}: removing manual slopes row")

      selected <- getReactableState("manual_slopes", "selected")
      req(selected)
      edited_slopes <- manual_slopes()[-selected, ]
      manual_slopes(edited_slopes)
    })


    #' Render manual slopes table
    output$manual_slopes <- renderReactable({
      log_trace("{id}: rendering slope edit data table")
      data <- manual_slopes()
      profiles <- unique(unlist(profiles_per_patient()))

      reactable(
        data = data,
        columns = list(
          TYPE = colDef(
            cell = dropdown_extra(
              id = session$ns("edit_TYPE"),
              choices = c("Selection", "Exclusion"),
              class = "dropdown-extra"
            )
          ),
          PATIENT = colDef(
            cell = dropdown_extra(
              id = session$ns("edit_PATIENT"),
              choices = names(profiles_per_patient()),
              class = "dropdown-extra"
            )
          ),
          PROFILE = colDef(
            cell = dropdown_extra(
              id = session$ns("dropdown_PROFILE"),
              choices = profiles,
              class = "dropdown-extra"
            )
          ),
          IXrange = colDef(
            cell = text_extra(
              id = session$ns("edit_IXrange")
            )
          ),
          REASON = colDef(
            cell = text_extra(
              id = session$ns("edit_REASON")
            )
          )
        ),
        selection = "multiple",
        borderless = TRUE,
        theme = reactableTheme(
          rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
        )
      )
    })

    #' For each of the columns in slope selector data frame, attach an even that will read
    #' edits for that column made in the reactable.
    purrr::walk(.SLOPE_SELECTOR_COLUMNS, \(colname) {
      observeEvent(input[[paste0("edit_", colname)]], {
        log_trace("{id}: manual slope edit")

        edit <- input[[paste0("edit_", colname)]]
        edited_slopes <- manual_slopes()
        edited_slopes[edit$row, edit$column] <- edit$value
        manual_slopes(edited_slopes)
      })
    })

    # Save the exclusion/selection data to the server data and rerun the NCA results
    #' TODO(mateusz): migrate this to a function outside of server, take proper arguments
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
        return(NULL)
      }

      manual_slopes <- manual_slopes()

      # Eliminate all rows with conflicting or blank values
      manual_slopes(manual_slopes %>%
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
        log_trace("{id}: plotly click with data detected")
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
                REASON = "[Graphical selection. Click here to include a reason]"
              )
              manual_slopes(rbind(manual_slopes(), new_row_manual_slopes))
            }
          }
        }
      }
    })

    #' return reactive with slope exclusions data to be displayed in Results -> Exclusions tab
    return(reactive({
      manual_slopes()
    }))
  })
}