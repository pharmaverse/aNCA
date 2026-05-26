# --- ADNCA mapping constants --------------------------------------------------

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
      "TRTA, TRTAN, ACTARM, TRT01A, TRT01P, RACE, SEX, GROUP, DOSFRM, ",
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

# --- SDTM mapping constants --------------------------------------------------

SDTM_NON_STD_MAPPING_INFO <- data.frame(
  Variable = c("Metabolites", "Grouping_Variables"),
  Label = c(
    "PCTEST values to flag as metabolites",
    "Variables to group and summarise results"
  ),
  Values = c("", ""),
  mapping_tooltip = c(
    paste0(
      "Choose the PCTEST values to flag as metabolites of the parent drug (METABFL = 'Y'). ",
      "If empty, all PCTEST values are treated as parent drug (METABFL = '')"
    ),
    "Additional column(s) to use to group the data in the outputs (e.g. AGE, SEX, ARM)"
  ),
  mapping_section = c("PC Concentrations", "Supplemental Variables"),
  mapping_alternatives = c("", paste0(
    "ARM, ACTARM, AGE, SEX, RACE, GROUP, COHORT, PART, PERIOD, FEDSTATE"
  )),
  mapping_order = c(7.5, 21),
  allow_create_numeric = c(FALSE, FALSE),
  is_multiple_choice = c(TRUE, TRUE),
  sdtm_domain = c("PC", "ALL"),
  stringsAsFactors = FALSE
)

SDTM_MAPPING_INFO <- metadata_nca_variables %>%
  filter(.data$is.mapped, .data$Dataset %in% c("SDTM", "PC", "EX", "DM")) %>%
  select(
    Variable, Label, Values, mapping_tooltip,
    mapping_section, mapping_alternatives, mapping_order,
    allow_create_numeric
  ) %>%
  mutate(
    is_multiple_choice = FALSE,
    sdtm_domain = metadata_nca_variables$Dataset[
      metadata_nca_variables$is.mapped &
        metadata_nca_variables$Dataset %in% c("SDTM", "PC", "EX", "DM")
    ]
  ) %>%
  bind_rows(SDTM_NON_STD_MAPPING_INFO) %>%
  arrange(.data$mapping_order)

SDTM_MAPPING_BY_SECTION <- split(
  SDTM_MAPPING_INFO, SDTM_MAPPING_INFO$mapping_section
)
SDTM_MAPPING_BY_SECTION <- SDTM_MAPPING_BY_SECTION[c(
  "SDTM General", "PC Concentrations", "EX Dosing", "Supplemental Variables"
)]

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
                                   allow_create_numeric = FALSE,
                                   id_prefix = "") {
  input_id <- paste0("select_", id_prefix, id)
  label_id <- paste0("label_", id_prefix, id)

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
        ns(input_id),
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
      span(textOutput(ns(label_id)))
    )
  )
}

.column_mapping_section <- function(ns, mapping_df, id_prefix = "") {
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
        allow_create_numeric = isTRUE(row$allow_create_numeric),
        id_prefix = id_prefix
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

#' Restore duplicate exclusions from stored key-based representation.
#' Matches stored keys against the current dataset and notifies the user
#' if some exclusions could not be matched.
#' @param data The current mapped dataset.
#' @param keys_df A data.frame of key columns (from settings), or NULL.
#' @returns Matched row indices, or NULL if nothing to restore.
#' @keywords internal
#' @noRd
.restore_duplicate_exclusions <- function(data, keys_df) {
  if (is.null(keys_df) || nrow(keys_df) == 0) return(NULL)

  matched_indices <- match_time_dup_keys(data, keys_df)
  n_stored <- nrow(keys_df)
  n_matched <- length(matched_indices %||% integer(0))

  if (n_matched < n_stored) {
    showNotification(
      sprintf(
        paste(
          "%d of %d stored duplicate exclusions could not be",
          "matched to the current dataset."
        ),
        n_stored - n_matched, n_stored
      ),
      type = "warning",
      duration = 10
    )
  }

  matched_indices
}

#' Validate and apply a single mapping variable.
#' @param var Variable name from MAPPING_INFO.
#' @param val Value(s) from the imported mapping.
#' @param column_names Available column names in the dataset.
#' @param session Shiny session for updating inputs.
#' @returns Error string if the mapping was skipped, or NULL on success.
#' @keywords internal
#' @noRd
.apply_single_mapping <- function(var, val, column_names, session,
                                  mapping_info = MAPPING_INFO) {
  var_info <- mapping_info[mapping_info$Variable == var, ]
  predefined <- strsplit(var_info$Values, ", ")[[1]]
  valid_values <- c(column_names, predefined)

  is_numeric_ok <- isTRUE(var_info$allow_create_numeric)
  invalid <- val[val != "" & !val %in% valid_values]
  if (is_numeric_ok) {
    invalid <- invalid[!grepl("^[0-9]+(\\.[0-9]+)?$", invalid)]
  }

  if (length(invalid) > 0) {
    return(paste0(var, " (", paste(invalid, collapse = ", "), ")"))
  }

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
    updateSelectizeInput(
      session, paste0("select_", var), selected = val
    )
  }

  NULL
}

.process_imported_mapping <- function(mapping, adnca_data, session,
                                     mapping_info = MAPPING_INFO) {
  if (is.null(mapping)) return(character(0))

  column_names <- names(adnca_data)
  skipped <- character(0)

  for (var in mapping_info$Variable) {
    if (!var %in% names(mapping) || var == "Metabolites") next
    result <- .apply_single_mapping(
      var, mapping[[var]], column_names, session, mapping_info
    )
    if (!is.null(result)) skipped <- c(skipped, result)
  }

  if (length(skipped) > 0) {
    showNotification(
      paste(
        "Mapping skipped for missing columns:",
        paste(skipped, collapse = "; ")
      ),
      type = "warning", duration = 10
    )
  }

  skipped
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
        # ADNCA mapping UI (visible by default)
        div(
          id = ns("adnca_mapping_panel"),
          h3("Data Mapping"),
          p(
            "The following columns are required for data analysis.",
            " Please ensure each of these columns",
            " has been assigned a corresponding column from your dataset"
          ),
          lapply(MAPPING_BY_SECTION, function(mapping_section) {
            .column_mapping_section(ns, mapping_section)
          })
        ),
        # SDTM mapping UI (hidden by default, prefixed IDs)
        shinyjs::hidden(div(
          id = ns("sdtm_mapping_panel"),
          h3("SDTM Data Mapping"),
          p(
            "Map columns from your PC, EX, and subject-level datasets.",
            " Standard SDTM column names are auto-detected when present."
          ),
          lapply(SDTM_MAPPING_BY_SECTION, function(mapping_section) {
            .column_mapping_section(ns, mapping_section, id_prefix = "sdtm_")
          })
        ))
      )
    )
  )
}

data_mapping_server <- function(id, adnca_data, imported_mapping, trigger,
                               input_mode = reactive("adnca"),
                               sdtm_raw = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    duplicates <- reactiveVal(NULL)
    sdtm_pknca_data <- reactiveVal(NULL)

    # --- Panel visibility -----------------------------------------------------
    observe({
      mode <- input_mode()
      shinyjs::toggle("adnca_mapping_panel", condition = (mode == "adnca"))
      shinyjs::toggle("sdtm_mapping_panel", condition = (mode == "sdtm"))
    })

    # --- Input ID constants ---------------------------------------------------
    input_ids <- paste0("select_", MAPPING_INFO[["Variable"]])
    sdtm_input_ids <- paste0("select_sdtm_", SDTM_MAPPING_INFO[["Variable"]])

    # --- Render labels for both panels ----------------------------------------
    purrr::walk(MAPPING_INFO$Variable, function(var) {
      output[[paste0("label_", var)]] <- renderText(
        MAPPING_INFO$Label[MAPPING_INFO$Variable == var][1]
      )
    })
    purrr::walk(SDTM_MAPPING_INFO$Variable, function(var) {
      output[[paste0("label_sdtm_", var)]] <- renderText(
        SDTM_MAPPING_INFO$Label[SDTM_MAPPING_INFO$Variable == var][1]
      )
    })

    # --- ADNCA: Populate the static inputs with column names ------------------
    observeEvent(c(adnca_data(), imported_mapping()), {
      req(input_mode() == "adnca")
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

      mapping <- imported_mapping()$mapping
      if (!is.null(mapping)) {
        # process mapping using settings to override default selections
        skipped <- .process_imported_mapping(mapping, adnca_data(), session)
        session$userData$mapping_skipped <- skipped
      }
    })

    # --- SDTM: Populate inputs from domain data -------------------------------
    observeEvent(c(sdtm_raw(), imported_mapping()), {
      req(input_mode() == "sdtm")
      sdtm <- sdtm_raw()
      req(sdtm)

      # Collect all available columns across domains
      all_cols <- unique(c(
        names(sdtm$pc), names(sdtm$ex),
        if (!is.null(sdtm$dm)) names(sdtm$dm) else character(0)
      ))

      # Use the SDTM-aware update function that handles the sdtm_ prefix
      .update_sdtm_selectize_inputs(
        session, sdtm_input_ids, all_cols, SDTM_MAPPING_INFO
      )

      # Clear optional fields if not present
      if (!"VOLUME" %in% names(sdtm$pc)) {
        updateSelectizeInput(session, "select_sdtm_VOLUME", selected = "")
        updateSelectizeInput(session, "select_sdtm_VOLUMEU", selected = "")
      }
      if (is.null(sdtm$dm) || !"WTBL" %in% names(sdtm$dm)) {
        updateSelectizeInput(session, "select_sdtm_WTBL", selected = "")
        updateSelectizeInput(session, "select_sdtm_WTBLU", selected = "")
      }

      mapping <- imported_mapping()$mapping
      if (!is.null(mapping)) {
        # Prefix the mapping info variables to match the sdtm_ input IDs
        sdtm_info_for_import <- SDTM_MAPPING_INFO
        sdtm_info_for_import$Variable <- paste0(
          "sdtm_", sdtm_info_for_import$Variable
        )
        skipped <- .process_imported_mapping(
          mapping, sdtm$pc, session, sdtm_info_for_import
        )
        session$userData$mapping_skipped <- skipped
      }
    })

    # Populate the dynamic input Metabolites for ADNCA
    observe({
      req(input_mode() == "adnca")
      req(input$select_PARAM != "")
      param_col <- input$select_PARAM
      choices_metab <- unique(adnca_data()[[param_col]])
      selected_metab <- if (!is.null(imported_mapping()$mapping$Metabolites)) {
        imported_mapping()$mapping$Metabolites
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

    # Populate the dynamic input Metabolites for SDTM
    observe({
      req(input_mode() == "sdtm")
      req(input$select_sdtm_PCTEST != "")
      sdtm <- sdtm_raw()
      req(sdtm)
      pctest_col <- input$select_sdtm_PCTEST
      choices_metab <- unique(sdtm$pc[[pctest_col]])
      selected_metab <- imported_mapping()$mapping$Metabolites
      updateSelectizeInput(
        session, "select_sdtm_Metabolites",
        choices = choices_metab, selected = selected_metab
      )
    })

    # Validate numeric inputs for variables with allow_create_numeric = TRUE
    .observe_numeric_inputs(input, session, adnca_data, MAPPING_INFO)

    # --- Collect current mapping values ---------------------------------------
    mapping <- reactive({
      if (input_mode() == "adnca") {
        ids <- input_ids
        suppl_ids <- paste0(
          "select_", MAPPING_BY_SECTION$`Supplemental Variables`$Variable
        )
      } else {
        ids <- sdtm_input_ids
        suppl_ids <- paste0(
          "select_sdtm_",
          SDTM_MAPPING_BY_SECTION$`Supplemental Variables`$Variable
        )
      }

      mapping_list <- setNames(lapply(ids, function(id) input[[id]]), ids)

      # Get the names to keep
      names_to_keep <- names(mapping_list) %>%
        keep(\(name) {
          !(name %in% suppl_ids) || any(mapping_list[[name]] != "")
        })

      mapping_list[names_to_keep]
    })
    observe({
      m <- mapping()
      # Strip both "select_" and "select_sdtm_" prefixes
      names(m) <- gsub("^select_(sdtm_)?", "", names(m))
      session$userData$mapping <- m
      # Store metabolites for SDTM script template
      if (input_mode() == "sdtm") {
        session$userData$sdtm_metabolites <- input$select_sdtm_Metabolites
      }
    })

    # --- ADNCA: apply mapping and produce mapped data.frame -------------------
    mapped_data <- reactive({
      req(input_mode() == "adnca")
      req(adnca_data())
      log_info("Processing ADNCA data mapping...")

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

    # --- SDTM: apply mapping and produce PKNCAdata ----------------------------
    sdtm_mapped_data <- reactive({
      req(input_mode() == "sdtm")
      sdtm <- sdtm_raw()
      req(sdtm)
      log_info("Processing SDTM data mapping...")

      mapping_ <- mapping()
      # Strip both "select_" and "select_sdtm_" prefixes
      names(mapping_) <- gsub("^select_(sdtm_)?", "", names(mapping_))

      tryCatch(
        withCallingHandlers(
          {
            # Rename columns per user mapping
            log_trace("SDTM: renaming PC columns...")
            pc <- .apply_sdtm_column_rename(sdtm$pc, mapping_, "PC")
            log_trace("SDTM: renaming EX columns...")
            ex <- .apply_sdtm_column_rename(sdtm$ex, mapping_, "EX")
            dm <- if (!is.null(sdtm$dm)) {
              log_trace("SDTM: renaming DM columns...")
              .apply_sdtm_column_rename(sdtm$dm, mapping_, "DM")
            }

            metabolites <- input$select_sdtm_Metabolites
            log_info(
              "SDTM: calling sdtm_to_PKNCAdata ",
              "(PC: ", nrow(pc), " rows, EX: ", nrow(ex), " rows",
              if (!is.null(dm)) paste0(", DM: ", nrow(dm), " rows") else "",
              if (length(metabolites) > 0)
                paste0(", metabolites: ", paste(metabolites, collapse = ", "))
              else "",
              ")"
            )
            pknca_obj <- sdtm_to_PKNCAdata(
              pc = pc, ex = ex, dm = dm, metabolites = metabolites
            )
            log_success(
              "SDTM: PKNCAdata created. ",
              "Conc: ", nrow(pknca_obj$conc$data), " rows, ",
              "Dose: ", nrow(pknca_obj$dose$data), " rows."
            )
            sdtm_pknca_data(pknca_obj)

            # Return the concentration data for filtering/preview
            pknca_obj$conc$data
          },
          warning = function(w) {
            log_warn(conditionMessage(w))
            showNotification(conditionMessage(w), type = "warning", duration = 10)
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          log_error("SDTM mapping failed: ", conditionMessage(e))
          showNotification(
            paste("SDTM conversion error:", conditionMessage(e)),
            type = "error", duration = NULL
          )
          NULL
        }
      )
    }) %>%
      bindEvent(trigger(), ignoreInit = TRUE)

    # --- Unified mapped data output -------------------------------------------
    unified_mapped_data <- reactive({
      if (input_mode() == "sdtm") sdtm_mapped_data() else mapped_data()
    })

    # Check for blocking duplicates using annotate_duplicates()
    df_duplicates <- reactiveVal(NULL)
    resolved_time_duplicate_rows <- reactiveVal(NULL)
    observe({
      session$userData$time_duplicate_rows <- resolved_time_duplicate_rows()
      # Store key-based representation for settings export
      session$userData$time_duplicate_keys <- extract_time_dup_keys(
        unified_mapped_data(), resolved_time_duplicate_rows()
      )
    })

    processed_data <- reactive({
      req(unified_mapped_data())

      dup_rows <- resolved_time_duplicate_rows()
      if (is.null(dup_rows)) {
        restored <- .restore_duplicate_exclusions(
          unified_mapped_data(), imported_mapping()$time_duplicate_keys
        )
        if (!is.null(restored)) {
          resolved_time_duplicate_rows(restored)
          dup_rows <- restored
        }
      }

      tryCatch(
        {
          result <- annotate_duplicates(unified_mapped_data(), dup_rows)
          select(result, any_of(c(names(unified_mapped_data()), "DTYPE")))
        },
        time_duplicate_error = function(e) {
          df_duplicates(e$duplicate_data)
          NULL
        }
      )
    }) %>%
      bindEvent(
        list(unified_mapped_data(), resolved_time_duplicate_rows()),
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
          annotate_duplicates(unified_mapped_data(), new_exclusions)
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
      names(m) <- gsub("^select_(sdtm_)?", "", names(m))
      m
    })

    list(
      processed_data = processed_data,
      mapping = cleaned_mapping,
      grouping_variables = reactive({
        if (input_mode() == "sdtm") {
          input$select_sdtm_Grouping_Variables
        } else {
          input$select_Grouping_Variables
        }
      }),
      time_duplicate_rows = resolved_time_duplicate_rows,
      sdtm_pknca_data = sdtm_pknca_data
    )
  })
}

#' Update selectize inputs for SDTM mapping.
#'
#' Handles the sdtm_ prefix on input IDs: strips it before looking up
#' the variable in the mapping info, then updates the prefixed input.
#'
#' @param session Shiny session.
#' @param input_ids Prefixed input IDs (e.g. "select_sdtm_PCTEST").
#' @param data_colnames Column names from the uploaded data.
#' @param mapping_info SDTM_MAPPING_INFO (unprefixed Variable names).
#' @noRd
.update_sdtm_selectize_inputs <- function(session, input_ids, data_colnames,
                                          mapping_info) {
  info_list <- split(mapping_info, mapping_info$Variable)

  for (input_id in input_ids) {
    # Strip "select_sdtm_" to get the original variable name
    var_name <- sub("^select_sdtm_", "", input_id)
    input_info <- info_list[[var_name]]
    if (is.null(input_info)) next

    alternatives <- strsplit(input_info$mapping_alternatives, ", ")[[1]]
    value_choices <- strsplit(input_info$Values, ", ")[[1]]

    potential_mappings <- c(
      intersect(c(var_name, alternatives), data_colnames),
      value_choices
    )

    selected_vals <- if (length(potential_mappings) == 0) {
      NULL
    } else if (input_info$is_multiple_choice) {
      potential_mappings
    } else {
      potential_mappings[[1]]
    }

    updateSelectizeInput(
      session, input_id,
      choices = list(
        "Select Column" = "",
        "Mapping Columns" = data_colnames,
        "Mapping Values" = value_choices
      ),
      selected = selected_vals
    )
  }
}

#' Rename columns in an SDTM domain data.frame based on user mapping.
#'
#' For each SDTM variable in the mapping, if the user selected a
#' non-standard column name, rename it to the expected SDTM name.
#' Only renames columns belonging to the specified domain.
#'
#' @param df Data.frame to rename columns in.
#' @param mapping Named list of mapping values (variable = selected column).
#' @param domain Character, one of "PC", "EX", "DM".
#' @returns The data.frame with renamed columns.
#' @noRd
.apply_sdtm_column_rename <- function(df, mapping, domain) {
  # Include domain-specific vars and SDTM General vars (shared across domains)
  domain_vars <- SDTM_MAPPING_INFO$Variable[
    SDTM_MAPPING_INFO$sdtm_domain %in% c(domain, "SDTM", "ALL")
  ]

  for (var in domain_vars) {
    selected <- mapping[[var]]
    if (is.null(selected) || selected == "" || length(selected) != 1) next
    if (selected != var && selected %in% names(df)) {
      names(df)[names(df) == selected] <- var
    }
  }

  df
}

