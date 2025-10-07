MAPPING_INFO <- metadata_nca_variables %>%
  filter(is.mapped, Dataset == "ADPC") %>%
  select(Variable, Values, mapping_tooltip, mapping_section) %>%
  mutate(is_multiple_choice = FALSE)

# Add non-standard mapping columns
NON_STD_MAPPING_INFO <- data.frame(
  Variable = c("Grouping_Variables", "TAU"),
  Values = c("", ""),
  mapping_tooltip = c(
    "Additional column(s) to use to group the data in the outputs (i.e, 'AGE', 'SEX')",
    "Numeric column for dose interval. Optional for multiple dose studies and assumed to have same units as RRTLU"
  ),
  mapping_section = c("Supplemental Variables", "Supplemental Variables"),
  is_multiple_choice = c(TRUE, FALSE)
)

MAPPING_INFO_LIST <- bind_rows(MAPPING_INFO, NON_STD_MAPPING_INFO) %>%
  split(.$mapping_section)

#' Define the manual units for concentration, dose, and time in a format recognized by PKNCA
MANUAL_UNITS <- list(
  concentration = c(
    "mg/L", "µg/mL", "ng/mL", "pg/mL", "mol/L", "mmol/L", "µmol/L", "nmol/L", "pmol/L", "mg/dL",
    "µg/dL", "ng/dL"
  ),
  dose = c(
    "mg", "g", "µg", "ng", "pg", "mol", "mmol", "µmol", "nmol", "pmol", "mg/kg", "g/kg", "µg/kg",
    "ng/kg", "pg/kg", "mol/kg", "mmol/kg", "µmol/kg", "nmol/kg", "pmol/kg"
  ),
  time = c("sec", "min", "hr", "day", "week", "month", "year")
)

# Define the required columns and group them into categories
MAPPING_COLUMN_GROUPS <- list(
  "Group Identifiers" = c("STUDYID", "USUBJID", "NCA_PROFILE"),
  "Sample Variables" = c("PARAM", "PCSPEC", "ROUTE", "AVAL"),
  "Dose Variables" = c("DRUG", "DOSEA"),
  "Time Variables" = c("AFRLT", "ARRLT", "NFRLT", "NRRLT"),
  "Unit Variables" = c("AVALU", "DOSEU", "RRLTU"),
  "Supplemental Variables" = c("Grouping_Variables", "TAU", "ADOSEDUR", "VOLUME", "VOLUMEU")
)

# Define the desired column order
MAPPING_DESIRED_ORDER <- c(
  "STUDYID", "USUBJID", "PARAM", "PCSPEC", "NCA_PROFILE",
  "AVAL", "AVALU", "AFRLT", "ARRLT", "NRRLT", "NFRLT",
  "RRLTU", "ROUTE", "DRUG", "DOSEA", "DOSEU", "ADOSEDUR",
  "VOLUME", "VOLUMEU", "TAU"
)

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
#' column_mapping_widget(ns = NS("example"), id = "STUDYID",
#' tooltip_text = "Select the study identifier column.")
.column_mapping_widget <- function(ns, id, tooltip_text, multiple = FALSE) {
  div(
    class = "column-mapping-row",
    tooltip(
      selectizeInput(
        ns(paste0("select_", id)),
        "",
        choices = NULL,
        multiple = multiple,
        options = list(placeholder = "Select Column"),
        width = "40%"
      ),
      tooltip_text
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
    lapply(1:nrow(mapping_df), function(i) {
      row <- mapping_df[i, ]
      .column_mapping_widget(ns, row$Variable, row$mapping_tooltip, row$is_multiple_choice)
    })
  )
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
        # Adjusted layout using CSS flexbox
        .column_mapping_section(ns, MAPPING_INFO_LIST$`Group Identifiers`),
        .column_mapping_section(ns, MAPPING_INFO_LIST$`Sample Variables`),
        .column_mapping_section(ns, MAPPING_INFO_LIST$`Dose Variables`),
        .column_mapping_section(ns, MAPPING_INFO_LIST$`Time Variables`),
        .column_mapping_section(ns, MAPPING_INFO_LIST$`Unit Variables`),
        .column_mapping_section(ns, MAPPING_INFO_LIST$`Supplemental Variables`)
      )
    )
  )
}

data_mapping_server <- function(id, adnca_data, trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    duplicates <- reactiveVal(NULL)
    # Derive input IDs from column_groups
    input_ids <- paste0("select_", bind_rows(MAPPING_INFO_LIST)[["Variable"]])
    
    # Loop through each label and create the renderText outputs
    purrr::walk(MAPPING_DESIRED_ORDER, \(label) {
      output[[paste0("label_", label)]] <- renderText(
        get_label(label, "ADPC")
      )
    })
    
    # Populate the static inputs with column names
    observeEvent(adnca_data(), {
      column_names <- names(adnca_data())
      update_selectize_inputs(session, input_ids, column_names, MANUAL_UNITS, MAPPING_DESIRED_ORDER)
    })

    # Observe submit button click and update processed_data
    mapping <- reactive({
      mapping_list <- setNames(lapply(input_ids, \(id) input[[id]]), input_ids)
      supplemental_ids <- paste0("select_", MAPPING_INFO_LIST$`Supplemental Variables`$Variable)

      # Get the names to keep
      names_to_keep <- names(mapping_list) |>
        keep(\(name) {
          # The logical condition with the any() fix
          !(name %in% supplemental_ids) || any(mapping_list[[name]] != "")
        })

      # Subset the list with the final names
      mapping_list[names_to_keep]
    })

    mapped_data <- reactive({
      req(adnca_data())
      log_info("Processing data mapping...")

      mapping_ <- mapping()
      names(mapping_) <- gsub("select_", "", names(mapping_))

      tryCatch({
        apply_mapping(
          adnca_data(),
          mapping_,
          MAPPING_DESIRED_ORDER,
          silent = FALSE
        )
      }, warning = function(w) {
        withCallingHandlers(
          {
            apply_mapping(
              adnca_data(),
              mapping_,
              MAPPING_DESIRED_ORDER,
              silent = FALSE
            )
          },
          warning = function(w) {
            log_warn(conditionMessage(w))
            showNotification(conditionMessage(w), type = "warning", duration = 10)
          }
        )
      }, error = function(e) {
        log_error(conditionMessage(e))
        showNotification(conditionMessage(e), type = "error", duration = NULL)
        NULL
      })
    }) |>
      bindEvent(trigger(), ignoreInit = TRUE)
    
    #Check for blocking duplicates
    # groups based on PKNCAconc formula
    df_duplicates <- reactive({
      req(mapped_data)
      mapped_data() %>%
        group_by(AFRLT, STUDYID, PCSPEC, DRUG, USUBJID, PARAM) %>%
        filter(n() > 1) %>%
        ungroup() %>%
        mutate(.dup_group = paste(AFRLT, STUDYID, PCSPEC, DRUG, USUBJID, PARAM, sep = "_"))
    })
    
    processed_data <- reactive({
      req(mapped_data())
      dataset <- mapped_data()
      
      if (nrow(df_duplicates()) == 0) {
        return(mutate(dataset, DFLAG = FALSE))
      }
      
      # User resolves duplicates, apply DFLAG
      duplicates(df_duplicates())
      
      if (!is.null(input$keep_selected_btn) && input$keep_selected_btn > 0) {
        # Get selected rows from the reactable
        selected <- getReactableState("duplicate_modal_table", "selected")
        req(length(selected) > 0)
        
        kept <- df_duplicates()[selected, , drop = FALSE]
        removed <- anti_join(df_duplicates(), kept, by = colnames(kept))
        dataset <- dataset %>%
          mutate(DFLAG = FALSE)
        
        # Set DFLAG to TRUE for the removed rows
        dataset <- dataset %>%
          rows_update(
            removed %>%
              mutate(DFLAG = TRUE) %>%
              select(names(dataset)),
            by = intersect(names(dataset), names(removed))
          )
        
        return(dataset)
      }
      # Don't return anything until duplicates are resolved
      return(NULL)
      
    })
    
    observeEvent(duplicates(), {
      showModal(
        modalDialog(
          title = "Duplicate Rows Detected",
          class = "modal-duplicates",
          tagList(
            p("The following rows are duplicates based on TIME, STUDYID,
              PCSPEC, DRUG, USUBJID, PARAM."),
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
    
    observeEvent(input$keep_selected_btn, {
      duplicates(NULL)
      removeModal()
    })
    
    output$duplicate_modal_table <- renderReactable({
      group_ids <- unique(df_duplicates()$.dup_group)
      color_map <- setNames(rep(c("white", "#e6f2ff"), length.out = length(group_ids)), group_ids)
      
      reactable(
        df_duplicates(),
        columns = list(.dup_group = colDef(show = FALSE)),
        rowStyle = function(index) {
          group <- df_duplicates()$.dup_group[index]
          list(background = color_map[[group]])
        },
        selection = "multiple",
        onClick = "select",
        compact = TRUE,
        wrap = FALSE,
        resizable = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 25, 50, 100, nrow(df_duplicates())),
        defaultPageSize = 10,
        style = list(fontSize = "0.75em")
      )
    })
    list(
      processed_data = processed_data,
      grouping_variables = reactive(input$select_Grouping_Variables)
    )
  })
}
