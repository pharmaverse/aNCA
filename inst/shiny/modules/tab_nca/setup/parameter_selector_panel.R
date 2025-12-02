#' This module renders a Virtual Shiny Widget (`virtualSelectInput`) to allow 
#' parameter selection for a specific study type. It handles two-way binding:
#' broadcasting user changes to the parent module and updating the UI if the
#' parent module pushes a new state (e.g., via file upload).
#'
#' @param id A unique namespace ID for the module.
#' @param all_params_grouped A list of data frames, split by parameter group (TYPE). 
#'   Each data frame must contain `PPTESTCD`, `PKNCA`, and `PPTEST` columns 
#'   used to generate labels, values, and descriptions.
#' @param current_selection A `reactive` expression returning a character vector
#'   of currently selected PKNCA parameter codes for this study type.
#'
#' @returns A `reactive` expression returning a character vector of the 
#'   user's selected PKNCA parameter codes (flat vector). Returns `NULL` 
#'   or `character(0)` if nothing is selected.
study_type_param_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("virtual_select_ui"))
  )
}

study_type_param_selector_server <- function(id,
                                             all_params_grouped,
                                             current_selection) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1. Prepare Choices
    formatted_choices <- reactive({

      # Iterate over the parameter groups
      purrr::imap(all_params_grouped, function(df, group_name) {

        # Build the options list for this group
        # label = PPTESTCD
        # value = PKNCA
        # description = PPTEST
        options_list <- purrr::pmap(list(df$PPTESTCD, df$PKNCA, df$PPTEST), 
                                    function(lab, val, desc) {
                                      list(
                                        label = as.character(lab), 
                                        value = as.character(val), 
                                        description = as.character(desc)
                                      )
                                    })

        # Return the group structure
        list(
          label = group_name,
          options = options_list
        )
      }) %>% unname() # Ensure it's a list of groups, not a named list
    })

    # 2. Initial render
    output$virtual_select_ui <- renderUI({
      req(formatted_choices())

      initial_selection <- isolate(current_selection())

      shinyWidgets::virtualSelectInput(
        inputId = ns("nca_parameters_selector"),
        label = "Select Parameters:",
        choices = formatted_choices(),
        selected = initial_selection,
        multiple = TRUE,
        search = TRUE,
        showSelectedOptionsFirst = TRUE,
        hasOptionDescription = TRUE,
        showValueAsTags = TRUE,
        width = "100%"
      )
    })

    # 3. Observer for External Updates to keep the UI in sync
    # if the master state changes outside this module
    observeEvent(current_selection(), {
      new_selection <- current_selection()
      # Prevent circular update if the value matches current input
      if (!identical(sort(new_selection), sort(input$nca_parameters_selector))) {
        shinyWidgets::updateVirtualSelect(
          session = session,
          inputId = "nca_parameters_selector",
          selected = new_selection
        )
      }
    }, ignoreInit = TRUE)
    
    # Return the grouped selections reactive to the main app
    return(reactive(input$nca_parameters_selector))
  })
}