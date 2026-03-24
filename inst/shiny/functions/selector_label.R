#' Selector with Label Module Server
#'
#' A Shiny module server-side function that generates a dynamic `virtualSelectInput`
#' based on NCA metadata. It supports conditional logic for switching between
#' variables and parameters, providing descriptions and automated initial selections.
#'
#' @param input,output,session Standard shiny module arguments.
#' @param choices Character vector of raw values to be filtered from the metadata
#'   and displayed in the selector.
#' @param initial_selection Optional; the initial value(s) to be selected.
#'   Defaults to `NULL`.
#' @param selector_ui_wrapper Character string matching the `uiOutput` ID in the
#'   UI where the selector should be rendered.
#' @param id Character string for the internal `virtualSelectInput` ID.
#' @param label Character string providing the display label for the selector.
#' @param metadata_type Character; either `"variable"` or `"parameter"`.
#'   Determines which metadata source (`metadata_nca_variables` or
#'   `metadata_nca_parameters`) to use for labels and descriptions.
#' @param pknca_data Optional; a PKNCA data object. If provided, the module
#'   attempts to intelligently guess the `initial_selection` based on
#'   subject or dose columns.
#'
#' @returns A reactive UI output rendered into the specified `selector_ui_wrapper`.
selector_label <- function(input, output, session,
                           choices,
                           initial_selection = NULL,
                           selector_ui_wrapper,
                           id,
                           label,
                           metadata_type = "variable",
                           pknca_data = NULL,
                           multiple = TRUE,
                           has_option_description = TRUE,
                           show_value_as_tags = TRUE,
                           show_selected_options_first = TRUE,
                           position = "bottom",
                           dropbox_wrapper = "body") {
  ns <- session$ns

  # Dataframe of choices
  # Conditional logic for if the data are variables or parameters
  formatted_choices <- reactive({
    if (metadata_type == "variable") {
      req(metadata_nca_variables)
      choices_df <- data.frame(Variable = choices, stringsAsFactors = FALSE)
      choices_df <- choices_df %>%
        left_join(
          metadata_nca_variables %>% select(Variable, Label) %>% distinct(),
          by = "Variable"
        ) %>%
        mutate(desc = ifelse(is.na(Label), Variable, Label)) %>%
        rename(val = Variable)
    } else if (metadata_type == "parameter") {
      req(metadata_nca_parameters)
      choices_df <- data.frame(PPTESTCD = choices, stringsAsFactors = FALSE)
      choices_df <- choices_df %>%
        left_join(
          metadata_nca_parameters %>% select(PPTESTCD, PPTEST) %>% distinct(),
          by = "PPTESTCD"
        ) %>%
        mutate(desc = ifelse(is.na(PPTEST), PPTESTCD, PPTEST)) %>%
        rename(val = PPTESTCD)
    } else {
      data.frame(val = choices, desc = choices)
    }

    # Mapping to virtualSelectInput format
    unname(purrr::pmap(list(choices_df$val, choices_df$desc), function(var, lab) {
      list(label = as.character(var),
           value = as.character(var),
           description = as.character(lab))
    }))
  })

  # Rendering UI output of the selector with label
  output[[selector_ui_wrapper]] <- renderUI({
    req(formatted_choices())
    selected_choices <- formatted_choices()

    # Optional code block for exploration: line plots USUBJID default choice
    if (!is.null(pknca_data)) {
      isolate({
        current_selection <- input[[id]] # Dynamic ID lookup

        if (is.null(current_selection)) {
          subject_col <- pknca_data$conc$columns$subject
          dose_col <- pknca_data$dose$columns$dose

          # Smart logic: prioritizes subject_col if "usubjid" is in input names
          initial_selection <- if ("usubjid" %in% names(input)) subject_col else dose_col
        } else {
          initial_selection <- current_selection
        }
      })
    } else {
      # Fallback: Just use the standard initial_selection argument
      initial_selection <- initial_selection
    }

    shinyWidgets::virtualSelectInput(
      inputId = ns(id),
      label = label,
      choices = selected_choices,
      multiple = multiple,
      selected = initial_selection,
      search = TRUE,
      hasOptionDescription = has_option_description,
      showValueAsTags = show_value_as_tags,
      showSelectedOptionsFirst = show_selected_options_first,
      position = "bottom",
      dropboxWrapper = dropbox_wrapper # Making sure the selector doesn't automatically open upwards
    )
  })
}
