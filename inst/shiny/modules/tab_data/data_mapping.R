# Add information for non-official CDISC mapping columns
NON_STD_MAPPING_INFO <- data.frame(
  Variable = c("Grouping_Variables", "Metabolites"),
  Label = c("Variables to group and summarise results", "PARAM values to flag as metabolites"),
  Order = c(100, 16),
  Values = c("", ""),
  mapping_tooltip = c(
    "Additional column(s) to use to group the data in the outputs (i.e, 'AGE', 'SEX')",
    paste0(
      "Choose the PARAM values to flag as metabolites of the parent drug (METABFL = 'Y'). ",
      "If empty, it is assumed that all PARAM values correspond to the parent drug (METABFL = '')"
    )
  ),
  mapping_section = c("Supplemental Variables", "Sample Variables"),
  mapping_alternatives = c(
    paste0(
      "TRTA, TRTAN, ACTARM, TRT01A, TRT01P, RACE, SEX, GROUP, DOSEFRM, ",
      "STRAIN, DOSFRM, NOMDOSE, DOSEP, COHORT, PART, PERIOD, FEDSTATE"
    ),
    ""
  ),
  is_multiple_choice = c(TRUE, TRUE),
  mapping_order = c(18, 5),
  allow_create_numeric = c(FALSE, FALSE)
)

# Make an unique dataset with all the variables for the mapping
MAPPING_INFO <- metadata_nca_variables %>%
  filter(is.mapped, Dataset == "ADNCA") %>%
  select(
    Variable, Label, Values, mapping_tooltip,
    mapping_section, mapping_alternatives, mapping_order,
    allow_create_numeric
  ) %>%
  mutate(
    is_multiple_choice = ifelse(Variable == "NCAwXRS", TRUE, FALSE)
  ) %>%
  bind_rows(NON_STD_MAPPING_INFO) %>%
  arrange(mapping_order)

MAPPING_BY_SECTION <- split(MAPPING_INFO, MAPPING_INFO$mapping_section)
sections_order <- c(
  "Group Identifiers", "Sample Variables", "Dose Variables",
  "Time Variables", "Unit Variables", "Supplemental Variables"
)
MAPPING_BY_SECTION <- MAPPING_BY_SECTION[sections_order]

# Column order is the default in apply_mapping()

#' Column Mapping Widget
#'
#' A reusable UI component for mapping dataset columns to specific identifiers or roles.
#'
#' @param ns A namespace function to generate IDs for Shiny inputs.
#' @param id A string representing the unique identifier for the widget.
#' @param tooltip_text A string containing the tooltip text to guide users.
#' @param multiple A logical indicating if multiple selections are allowed (default is FALSE).
#'
#' @return A Shiny `div` containing a `selectizeInput` with associated labels and tooltip.
#'
#' @examples
#' column_mapping_widget(
#'   ns = NS("example"), id = "STUDYID",
#'   tooltip_text = "Select the study identifier column."
#' )
.column_mapping_widget <- function(ns, id, tooltip_text, multiple = FALSE,
                                   allow_create_numeric = FALSE) {
  selectize_options <- if (allow_create_numeric) {
    list(
      create = TRUE,
      placeholder = "Select column or type a numeric value"
    )
  } else {
    list(placeholder = "Select Column")
  }

  div(
    class = "column-mapping-row",
    tooltip(
      selectizeInput(
        ns(paste0("select_", id)),
        "",
        choices = NULL,
        multiple = multiple,
        options = selectize_options,
        width = "40%"
      ),
      tooltip_text,
      placement = "top"
    ),
    div(
      class = "column-mapping-output",
      span(paste0(id, ":"))
    ),
    div(
      class = "column-mapping-label",
      span(textOutput(ns(paste0("label_", id))))
    )
  )
}

.column_mapping_section <- function(ns, mapping_df) {
  section_title <- unique(mapping_df$mapping_section)
  if (length(section_title) != 1) {
    stop("mapping_df must contain exactly one unique mapping_section value.")
  }
  tags$section(
    h5(section_title),
    lapply(seq_len(nrow(mapping_df)), function(i) {
      row <- mapping_df[i, ]
      .column_mapping_widget(
        ns, row$Variable, row$mapping_tooltip, row$is_multiple_choice,
        allow_create_numeric = isTRUE(row$allow_create_numeric)
      )
    })
  )
}
.observe_numeric_inputs <- function(input, session, adnca_data, mapping_info) {
  numeric_vars <- mapping_info$Variable[mapping_info$allow_create_numeric %in% TRUE]
  lapply(numeric_vars, function(var) {
    input_id <- paste0("select_", var)
    observeEvent(input[[input_id]], {
      value <- input[[input_id]]
      if (!is.null(value) && value != "" && !value %in% names(adnca_data())) {
        if (!grepl("^[0-9]+(\\.[0-9]+)?$", value)) {
          showNotification(
            paste0(var, ": only numeric values are allowed. '", value, "' is not valid."),
            type = "warning",
            duration = 5
          )
          updateSelectizeInput(session, input_id, selected = "")
        }
      }
    })
  })
}

.process_imported_mapping <- function(mapping, adnca_data, session) {

  if (!is.null(mapping)) {
    column_names <- names(adnca_data)
    skipped <- character(0)

    for (var in MAPPING_INFO$Variable) {
      if (!var %in% names(mapping) || var == "Metabolites") next
      val <- mapping[[var]]

      # Build the set of valid values: column names + predefined Values +
      # numeric literals for allow_create_numeric variables.
      var_info <- MAPPING_INFO[MAPPING_INFO$Variable == var, ]
      predefined <- strsplit(var_info$Values, ", ")[[1]]
      valid_values <- c(column_names, predefined)

      is_numeric_ok <- isTRUE(var_info$allow_create_numeric)
      invalid <- val[val != "" & !val %in% valid_values]
      if (is_numeric_ok) {
        invalid <- invalid[!grepl("^[0-9]+(\\.[0-9]+)?$", invalid)]
      }

      if (length(invalid) > 0) {
        skipped <- c(skipped, paste0(var, " (", paste(invalid, collapse = ", "), ")"))
        next
      }

      # For allow_create_numeric variables with custom numeric values,
      # the value must be added to choices or updateSelectizeInput ignores it.
      custom_numeric <- if (is_numeric_ok) {
        val[!val %in% c(column_names, predefined)]
      } else {
        character(0)
      }
      if (length(custom_numeric) > 0) {
        updateSelectizeInput(
          session, paste0("select_", var),
          choices = list(
            "Select Column" = "",
            "Mapping Columns" = c(column_names, custom_numeric),
            "Mapping Values" = predefined
          ),
          selected = val
        )
      } else {
        updateSelectizeInput(session, paste0("select_", var), selected = val)
      }
    }

    if (length(skipped) > 0) {
      showNotification(
        paste("Mapping skipped for missing columns:", paste(skipped, collapse = "; ")),
        type = "warning", duration = 10
      )
    }
  }
}

#' Column Mapping Module
#' This module provides implementation for mapping columns from a dataset to specific
#' roles required for analysis. It allows users to select columns for various categories such as
#' group identifiers, sample variables, dose variables, time variables, and unit variables.
#' It also checks for duplicates in the data that will cause problems later, and allows the user
#' to resolve them.
#'
#' @param id The module ID.
#' @param adnca_data A reactive expression that returns the dataset to be processed.
#' @param on_submit A callback function to be executed when the submit button is clicked.
#'
#' @returns A list containing:
#' \item{processed_data}{A reactive expression that returns the processed dataset.}
#' \item{grouping_variables}{A reactive expression that returns the selected grouping variables.}
#'
#' @details
#' The column_mapping_server module allows users to map columns from their dataset to specific
#' roles required for analysis. Users can select columns for the following categories:
#' \itemize{
#' \item \strong{Group Identifiers}:
#' Columns that identify groups, such as study ID and subject ID.
#' \item \strong{Sample Variables}:
#' Columns that describe sample characteristics, such as analyte and specimen.
#' \item \strong{Dose Variables}:
#' Columns that describe dosing information, such as dose number and dose amount.
#' \item \strong{Time Variables}:
#' Columns that describe time-related information, such as relative time.
#' \item \strong{Unit Variables}:
#' Columns that describe units for various measurements, such as concentration and dose units.
#' }
#'
#' The module also handles special cases, such as allowing users
#' to select "NA" for the dose duration
#' column and providing options for manual units.
#'
#' When the submit button is clicked, the module processes the dataset by:
#' \itemize{
#' \item Renaming columns based on user selections.
#' \item Handling manual units.
#' \item Reordering columns according to a desired order.
#' \item Filtering out concentration duplicates
#' \item Setting a flag for duplicate rows, as selected by the user.
#' }
#' The processed dataset and selected grouping variables are returned as reactive expressions.
data_mapping_ui <- function(id) {
  ns <- NS(id)

  div(
    card(
      div(
        class = "data-mapping-container",
        h3("Data Mapping"),
        p(
          "The following columns are required for data analysis.",
          " Please ensure each of these columns",
          " has been assigned a corresponding column from your dataset"
        ),
        # Define the input widgets for each variable to map
        lapply(MAPPING_BY_SECTION, function(mapping_section) {
          .column_mapping_section(ns, mapping_section)
        })
      )
    )
  )
}

data_mapping_server <- function(id, adnca_data, imported_mapping, trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    duplicates <- reactiveVal(NULL)
    # Derive input IDs from column_groups
    input_ids <- paste0("select_", MAPPING_INFO[["Variable"]])

    # Loop through each label and create the renderText outputs
    purrr::walk(MAPPING_INFO$Variable, function(var) {
      output[[paste0("label_", var)]] <- renderText(
        MAPPING_INFO$Label[MAPPING_INFO$Variable == var]
      )
    })

    # Populate the static inputs with column names
    observeEvent(c(adnca_data(), imported_mapping()), {
      column_names <- names(adnca_data())
      update_selectize_inputs(session, input_ids, column_names, MAPPING_INFO)

      # Exceptions:
      # If by default VOLUME is not mapped, then neither is VOLUMEU
      if (!"VOLUME" %in% column_names) {
        updateSelectizeInput(session, "select_VOLUMEU", selected = "")
      }
      # If by default WTBL is not mapped, then neither is WTBLU
      if (!"WTBL" %in% column_names) {
        updateSelectizeInput(session, "select_WTBLU", selected = "")
      }

      mapping <- imported_mapping()
      if (!is.null(mapping)) {
        # process mapping using settings to override default selections
        .process_imported_mapping(mapping, adnca_data(), session)
      }
    })
    # Populate the dynamic input Metabolites
    observe({
      req(input$select_PARAM != "")
      param_col <- input$select_PARAM
      choices_metab <- unique(adnca_data()[[param_col]])
      # Use pending import if available, otherwise fall back to METABFL default
      selected_metab <- if (!is.null(imported_mapping()$Metabolites)) {
        imported <- imported_mapping()$Metabolites
        imported
      } else if ("METABFL" %in% names(adnca_data())) {
        unique(adnca_data()[adnca_data()$METABFL == "Y", ][[param_col]])
      } else {
        NULL
      }
      updateSelectizeInput(
        session, "select_Metabolites",
        choices = choices_metab, selected = selected_metab
      )
    })

    # Validate numeric inputs for variables with allow_create_numeric = TRUE
    .observe_numeric_inputs(input, session, adnca_data, MAPPING_INFO)

    # Observe submit button click and update processed_data
    mapping <- reactive({
      mapping_list <- setNames(lapply(input_ids, function(id) input[[id]]), input_ids)
      supplemental_ids <- paste0("select_", MAPPING_BY_SECTION$`Supplemental Variables`$Variable)

      # Get the names to keep
      names_to_keep <- names(mapping_list) %>%
        keep(\(name) {
          # The logical condition with the any() fix
          !(name %in% supplemental_ids) || any(mapping_list[[name]] != "")
        })

      # Subset the list with the final names
      mapping_list[names_to_keep]
    })
    observe({
      m <- mapping()
      names(m) <- gsub("^select_", "", names(m))
      session$userData$mapping <- m
    })

    mapped_data <- reactive({
      req(adnca_data())
      log_info("Processing data mapping...")

      mapping_ <- mapping()
      names(mapping_) <- gsub("^select_", "", names(mapping_))

      tryCatch(
        withCallingHandlers(
          {
            adnca_data() %>%
              apply_mapping(
                mapping_,
                silent = FALSE
              ) %>%
              create_metabfl(input$select_Metabolites) %>%
              adjust_class_and_length(metadata_nca_variables, adjust_length = FALSE)
          },
          warning = function(w) {
            log_warn(conditionMessage(w))
            showNotification(conditionMessage(w), type = "warning", duration = 10)
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          log_error(conditionMessage(e))
          showNotification(conditionMessage(e), type = "error", duration = NULL)
          NULL
        }
      )
    }) %>%
      bindEvent(trigger(), ignoreInit = TRUE)

    # Check for blocking duplicates using annotate_duplicates()
    df_duplicates <- reactiveVal(NULL)
    resolved_time_duplicate_rows <- reactiveVal(NULL)
    observe({
      session$userData$time_duplicate_rows <- resolved_time_duplicate_rows()
    })

    processed_data <- reactive({
      req(mapped_data())

      tryCatch(
        {
          result <- annotate_duplicates(mapped_data(), resolved_time_duplicate_rows())
          select(result, any_of(c(names(mapped_data()), "DTYPE")))
        },
        time_duplicate_error = function(e) {
          df_duplicates(e$duplicate_data)
          NULL
        }
      )
    }) %>%
      bindEvent(
        list(mapped_data(), resolved_time_duplicate_rows()),
        ignoreInit = FALSE
      )

    observeEvent(input$keep_selected_btn, {
      req(df_duplicates())
      selected <- getReactableState("duplicate_modal_table", "selected")

      # Derive rows to EXCLUDE (all duplicate rows the user did NOT select)
      dup_data <- df_duplicates()
      dup_row_indices <- dup_data$ROWID
      keep_indices <- if (!is.null(selected)) dup_row_indices[selected] else integer(0)
      exclude_indices <- setdiff(dup_row_indices, keep_indices)

      # Combine with any previously resolved rows
      prev <- resolved_time_duplicate_rows()
      new_exclusions <- unique(c(prev, exclude_indices))

      # Validate: check if the selection resolves all time duplicates
      tryCatch(
        {
          annotate_duplicates(mapped_data(), new_exclusions)
          # Selection resolves all duplicates — proceed
          resolved_time_duplicate_rows(new_exclusions)
          removeModal()
        },
        time_duplicate_error = function(e) {
          # Still unresolved duplicates — update and re-show modal
          resolved_time_duplicate_rows(new_exclusions)
          df_duplicates(e$duplicate_data)
          showNotification(
            "There are still unresolved time duplicates. Please select rows to keep.",
            type = "warning"
          )
        }
      )
    })

    observeEvent(df_duplicates(), {
      showModal(
        modalDialog(
          title = "Duplicate Rows Detected",
          class = "modal-duplicates",
          tagList(
            p("The following rows are duplicates based on TIME, STUDYID,
              PCSPEC, DOSETRT, USUBJID, PARAM."),
            p("Please select the rows you would like to KEEP.
              Rows not selected will be flagged and filtered."),
            p("Alternatively, click 'Cancel' to discard the changes,
              and clean the dataset yourself."),
            div(reactableOutput(ns("duplicate_modal_table")))
          ),
          easyClose = FALSE,
          footer = tagList(
            actionButton(ns("keep_selected_btn"), "Keep Selected", class = "btn-primary"),
            modalButton("Cancel")
          ),
          size = "l"
        )
      )
    })

    output$duplicate_modal_table <- renderReactable({
      req(df_duplicates())
      dup_data <- df_duplicates()
      reactable(
        dup_data,
        columns = list(
          .dup_group = colDef(show = FALSE),
          .is_time_dup = colDef(show = FALSE),
          ROWID = colDef(show = FALSE)
        ),
        rowStyle = function(index) {
          if (dup_data[index, ".dup_group"] %% 2 == 0) {
            list(background = "white")
          } else {
            list(background = "#e6f2ff")
          }
        },
        selection = "multiple",
        onClick = "select",
        compact = TRUE,
        wrap = FALSE,
        resizable = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 25, 50, 100, nrow(dup_data)),
        defaultPageSize = 10,
        style = list(fontSize = "0.75em")
      )
    })

    # Cleaned mapping with select_ prefix removed
    cleaned_mapping <- reactive({
      m <- mapping()
      names(m) <- gsub("^select_", "", names(m))
      m
    })

    list(
      processed_data = processed_data,
      mapping = cleaned_mapping,
      grouping_variables = reactive(input$select_Grouping_Variables),
      time_duplicate_rows = resolved_time_duplicate_rows
    )
  })
}
