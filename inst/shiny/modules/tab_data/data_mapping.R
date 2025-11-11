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
    "TRTA, TRTAN, ACTARM, TRT01A, TRT01P, AGE, RACE, SEX, GROUP, NOMDOSE, DOSEP",
    ""
  ),
  is_multiple_choice = c(TRUE, TRUE)
)

# Make an unique dataset with all the variables for the mapping
MAPPING_INFO <- metadata_nca_variables %>%
  filter(is.mapped, Dataset == "ADPC") %>%
  select(Variable, Label, Order, Values, mapping_tooltip, mapping_section, mapping_alternatives) %>%
  mutate(is_multiple_choice = FALSE) %>%
  bind_rows(NON_STD_MAPPING_INFO) %>%
  arrange(Order)

MAPPING_BY_SECTION <- split(MAPPING_INFO, MAPPING_INFO$mapping_section)
sections_order <- c(
  "Group Identifiers", "Sample Variables", "Dose Variables",
  "Time Variables", "Unit Variables", "Supplemental Variables"
)
MAPPING_BY_SECTION <- MAPPING_BY_SECTION[sections_order]

# Define the desired column order
MAPPING_DESIRED_ORDER <- c(
  "STUDYID", "USUBJID", "PARAM", "PCSPEC", "ATPTREF",
  "AVAL", "AVALU", "AFRLT", "ARRLT", "NRRLT", "NFRLT",
  "RRLTU", "ROUTE", "DOSETRT", "DOSEA", "DOSEU", "ADOSEDUR",
  "VOLUME", "VOLUMEU", "TRTRINT", "METABFL"
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
        # Define the input widgets for each variable to map
        lapply(MAPPING_BY_SECTION, function(mapping_section) {
          .column_mapping_section(ns, mapping_section)
        })
      )
    )
  )
}

data_mapping_server <- function(id, adnca_data, trigger) {
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
    observeEvent(adnca_data(), {
      column_names <- names(adnca_data())
      update_selectize_inputs(session, input_ids, column_names, MAPPING_INFO)

      # Exception: If by default VOLUME is not mapped, then neither is VOLUMEU
      if (!"VOLUME" %in% column_names) {
        updateSelectizeInput(session, "select_VOLUMEU", selected = "")
      }
    })
    # Populate the dynamic input Metabolites
    observe({
      req(input$select_PARAM != "")
      param_col <- input$select_PARAM
      choices_metab <- unique(adnca_data()[[param_col]])
      selected_metab <- if ("METABFL" %in% names(adnca_data())) {
        unique(adnca_data()[adnca_data()$METABFL == "Y", ][[param_col]])
      } else {
        NULL
      }
      updateSelectizeInput(
        session, "select_Metabolites",
        choices = choices_metab, selected = selected_metab
      )
    })

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

    mapped_data <- reactive({
      req(adnca_data())
      log_info("Processing data mapping...")

      mapping_ <- mapping()
      names(mapping_) <- gsub("select_", "", names(mapping_))

      tryCatch({
        adnca_data() %>%
          apply_mapping(
            mapping_,
            MAPPING_DESIRED_ORDER,
            silent = FALSE
          ) %>%
          create_metabfl(input$select_Metabolites)
      }, warning = function(w) {
        withCallingHandlers(
          {
            adnca_data() %>%
              apply_mapping(
                mapping_,
                MAPPING_DESIRED_ORDER,
                silent = FALSE
              ) %>%
              create_metabfl(input$select_Metabolites)
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
    }) %>%
      bindEvent(trigger(), ignoreInit = TRUE)

    #Check for blocking duplicates
    # groups based on PKNCAconc formula

    df_duplicates <- reactiveVal(NULL)
    processed_data <- reactive({
      req(mapped_data())

      dataset <- mapped_data() %>%
        # Annotate exact duplicate records
        group_by(AVAL, AFRLT, STUDYID, PCSPEC, DOSETRT, USUBJID, PARAM) %>%
        mutate(DTYPE = ifelse(row_number() > 1, "COPY", "")) %>%
        # Annotate duplicate time records
        group_by(AFRLT, STUDYID, PCSPEC, DOSETRT, USUBJID, PARAM) %>%
        mutate(is.time.duplicate = (n() - sum(DTYPE != "")) > 1) %>%
        mutate(.dup_group = cur_group_id()) %>%
        ungroup() %>%
        mutate(ROWID = row_number())

      if (!is.null(input$keep_selected_btn) && input$keep_selected_btn > 0) {

        # Get selected rows from the reactable
        selected <- getReactableState("duplicate_modal_table", "selected")
        dataset <- df_duplicates() %>%
          group_by(is.time.duplicate) %>%
          mutate(
            DTYPE = ifelse(
              is.time.duplicate & row_number() %in% selected,
              "TIME DUPLICATE",
              DTYPE
          )) %>%
          group_by(AFRLT, STUDYID, PCSPEC, DOSETRT, USUBJID, PARAM) %>%
          mutate(is.time.duplicate = (n() - sum(DTYPE != "")) > 1) %>%
          ungroup()
        if (any(dataset$is.time.duplicate, na.rm = TRUE)) {
          showNotification("There are still duplicate time records. Please resolve them before proceeding.", type = "error", duration = NULL)
          return(NULL)
        } else {
          removeModal()
          return(dataset)
        }
      }

      if (any(dataset$is.time.duplicate, na.rm = TRUE)) {
        df_duplicates(dataset)
        return(NULL)
      } else {
        dataset
      }
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
      reactable(
        df_duplicates() %>%
          filter(is.time.duplicate),
        columns = list(.dup_group = colDef(show = FALSE)),
        rowStyle = function(index) {
          if (df_duplicates()[index, ".dup_group"] %% 2 == 0) {
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
