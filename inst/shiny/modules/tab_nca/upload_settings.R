upload_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(
      ns("settings_upload"),
      width = "60%",
      label = "Upload Settings",
      buttonLabel = list(icon("folder"), "Browse"),
      accept = c(".xlsx", ".rds")
    )
  )
}

#' Load Settings Server Module
#'
#' This module handles the server-side logic for loading project settings from a file,
#' including updating the UI elements:
#' a) selectInputs for dosno, analyte, pcspec
#' b) NCA parameters, method, units and C0 imputation
#' c) The flag rules and their thresholds
#' d) The slope adjustments for the half life calculation
#'
#'Its arguments are:
#' - id The module's ID.
#' - processed_pknca_data A reactive expression containing the project data to be updated.
#' - parent_session The parent Shiny session (tab_nca). Used to update all mentioned inputs
#' - auc_counter A reactive value for counting AUC intervals.
#' - manual_slopes A reactive value for handling manual slopes.
#'
upload_settings_server <- function(id, processed_pknca_data, parent_session,
                                   auc_counter, manual_slopes) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$settings_upload, {
browser()
      # Identify the file submited
      file_path <- input$settings_upload$datapath

      if (tools::file_ext(file_path) == "rds") {
        # Allow loading of big rds files into R
        options(shiny.maxRequestSize = 10 * 1024 ^ 2) # 10 Mb (generally ~5Mb)
        setts <- tryCatch(
          readRDS(file_path),
          error = function(e) {
            showNotification("Error: Uploaded RDS file is not valid.", type = "error")
            return(NULL)
          }
        )
      } else if (tools::file_ext(file_path) == "xlsx") {
        # Load and restructure into PKNCA object the excel file
        sheets <- openxlsx::getSheetNames(file_path)
        setts <- lapply(sheets, function(sheet) openxlsx::read.xlsx(file_path, sheet = sheet))
        names(setts) <- sheets

        # Make some validations of the file uploaded
        .validate_excel_file(setts, sheets)

        # Once all validations are done rearrange the data
        setts <- list(
          conc = list(
            data = setts$conc_data,
            columns = list(
              groups = list(
                group_analyte = setts$conc_columns %>%
                  filter(ind == "groups.group_analyte") %>%
                  pull(values),
                group_group_vars = setts$conc_columns %>%
                  filter(ind == "groups.group_vars") %>%
                  pull(values)
              ),
              time = setts$conc_columns %>%
                filter(ind == "time") %>%
                pull(values),
              concentration = setts$conc_columns %>%
                filter(ind == "concentration") %>%
                pull(values),
              subject = setts$conc_columns %>%
                filter(ind == "subject") %>%
                pull(values),
              time.nominal = setts$conc_columns %>%
                filter(ind == "time.nominal") %>%
                pull(values),
              exclude = setts$conc_columns %>%
                filter(ind == "exclude") %>%
                pull(values),
              duration = setts$conc_columns %>%
                filter(ind == "duration") %>%
                pull(values),
              exclude_half.life = setts$conc_columns %>%
                filter(ind == "exclude_half.life") %>%
                pull(values)
            )
          ),
          dose = list(
            data = setts$dose_data,
            columns = list(
              groups = list(
                group_vars = setts$dose_columns %>%
                  filter(ind == "groups.group_vars") %>%
                  pull(values)
              ),
              time = setts$dose_columns %>%
                filter(ind == "time") %>%
                pull(values),
              dose = setts$dose_columns %>%
                filter(ind == "dose") %>%
                pull(values),
              time.nominal = setts$dose_columns %>%
                filter(ind == "time.nominal") %>%
                pull(values),
              exclude = setts$dose_columns %>%
                filter(ind == "exclude") %>%
                pull(values),
              duration = setts$dose_columns %>%
                filter(ind == "duration") %>%
                pull(values),
              route = setts$dose_columns %>%
                filter(ind == "route") %>%
                pull(values)
            )
          ),
          intervals = setts$intervals,
          options = setts$options,
          units = setts$units,
          flag_rules = setts$flag_rules
        )
      } else {
        showNotification("Invalid settings file format. Formats accepted are: .xlsx, .rds")
        return()
      }
      # Update all select inputs in the NCA setup tab
      .updateselectinput_with_setts(
        "DOSNO", "DOSNO",
        setts_df = setts$intervals, data_df = processed_pknca_data()$conc$data,
        parent_session, "select_dosno"
      )
      .updateselectinput_with_setts(
        var_setts_col = setts$conc$columns$groups$group_analyte,
        var_data_col = processed_pknca_data()$conc$columns$groups$group_analyte,
        setts_df = setts$intervals,
        data_df = processed_pknca_data()$conc$data,
        parent_session, "select_analyte"
      )
      .updateselectinput_with_setts(
        "PCSPEC", "PCSPEC",
        setts, processed_pknca_data()$conc$data,
        parent_session, "select_pcspec"
      )

      updateSelectInput(
        parent_session,
        inputId = "method",
        label = "Extrapolation Method:",
        choices = c("lin-log", "lin up/log down", "linear"),
        selected = setts$options$auc.method
      )

      # Update the parameter picker input
      all_param_options <- setdiff(names(PKNCA::get.interval.cols()), c("start", "end"))

      params_setts <- setts$intervals %>%
        filter(type_interval == "main") %>%
        mutate(across(all_of(all_param_options), ~ifelse(. == FALSE & is.logical(.), NA, .))) %>%
        select(any_of(all_param_options)) %>%
        select_if(~any(!is.na(.))) %>%
        names()

      updatePickerInput(
        parent_session,
        inputId = "nca_params",
        selected = params_setts
      )

      # Load the data object and update its units
      data <- processed_pknca_data()
      data$units <- dplyr::left_join(data$units, setts$units,
                                     by = c("PARAM", "PPTESTCD"),
                                     suffix = c(".data", ".setts")) %>%
        mutate(across(ends_with(".setts"),
                      ~coalesce(., get(str_replace(cur_column(), ".setts", ".data"))))) %>%
        select(-ends_with(".data")) %>%
        rename_with(~str_remove(., ".setts"))

      # Check if any start imputation is defined in the settings intervals
      if (any(grepl("start.*", setts$intervals$impute))) {
        updateCheckboxInput(
          parent_session,
          inputId = "should_impute_c0",
          value = TRUE
        )
      } else {
        updateCheckboxInput(
          parent_session,
          inputId = "should_impute_c0",
          value = FALSE
        )
      }

      # Consider if there were any custom AUC intervals defined by the user and update
      dose_time_col <- setts$dose$columns$time
      intervals_userinput_setts <- setts$intervals %>%
        ungroup() %>%
        filter(type_interval == "manual") %>%
        left_join(setts$dose$data, by = c(unlist(unname(setts$dose$columns$groups)), "DOSNO")) %>%
        mutate(start = start - !!sym(dose_time_col),
               end = end - !!sym(dose_time_col)) %>%
        select(start, end) %>%
        filter(!duplicated(paste0(start, end)))

      if ((nrow(intervals_userinput_setts) > 0)) {
        updateCheckboxInput(parent_session, inputId = "AUCoptions",
                            label = "Select Partial AUC", value = TRUE)

        for (i in seq_along(nrow(intervals_userinput_setts))) {

          insertUI(
            selector = paste0("#", parent_session$ns("AUCInputs")),
            where = "beforeEnd",
            ui = partial_auc_input(
              id = paste0("AUC_", auc_counter()),
              ns = parent_session$ns,
              min_sel_value = as.numeric(intervals_userinput_setts$start[i]),
              max_sel_value = as.numeric(intervals_userinput_setts$end[i])
            )
          )
        }
        auc_counter(nrow(intervals_userinput_setts))
      }

      # Update Flag rules based on saved file details
      if (any(!is.na(setts$flag_rules$values))) {
        flag_rules_to_apply <- setNames(setts$flag_rules$values[!is.na(setts$flag_rules$values)],
                                        nm = setts$flag_rules$ind[!is.na(setts$flag_rules$values)])
        for (rule_input in names(flag_rules_to_apply)) {
          threshold_input <- gsub(pattern = "rule_", replacement = "", rule_input)
          threshold_input <- paste0(gsub("_", ".", threshold_input), "_threshold")

          updateCheckboxInput(parent_session, inputId = rule_input, value = TRUE)
          updateNumericInput(parent_session,
                             inputId = threshold_input,
                             value = flag_rules_to_apply[[rule_input]])
        }
      }
      ########################################################################################
      # ToDo: Loading manual slope settings still not working because of manual_slopes()
      # Needs still to be worked on when refactoring is done
      conc_setts_cols <- unname(unlist(setts$conc$columns[c("groups", "time", "concentration")]))
      conc_data_cols <- unname(unlist(data$conc$columns[c("groups", "time", "concentration")]))

      if (nrow(setts$conc$data) > 0) {

        # Using the exclusions/selections of the settings update the concentration dataset
        new_conc_data <- dplyr::left_join(
          data$conc$data,
          setts$conc$data %>%
            select(
              any_of(c(conc_setts_cols, "is.included.hl",
                       "is.excluded.hl", "exclude_half.life", "REASON")
              )
            ),
          by = intersect(conc_setts_cols, conc_data_cols),
          suffix = c(".data", ".setts")
        ) %>%
          mutate(
            across(
              ends_with(".setts"),
              ~coalesce(., get(str_replace(cur_column(), ".setts", ".data")))
            )
          ) %>%
          select(-ends_with(".data")) %>%
          rename_with(~str_remove(., ".setts"))

        changed_rows <- anti_join(new_conc_data, data$conc$data)
        mismatched_rows <- anti_join(
          setts$conc$data %>%
            select(any_of(c(conc_data_cols, "is.included.hl",
                            "is.excluded.hl", "exclude_half.life"))),

          new_conc_data %>%
            select(any_of(c(conc_data_cols, "is.included.hl",
                            "is.excluded.hl", "exclude_half.life")))
        )

        # If there are mismatched rows between the datasets, inform the user
        if (nrow(mismatched_rows) > 0) {
          showModal(modalDialog(
            title = "Mismatched Data Points",
            tags$h4("The following data rows in the settings file do not match the uploaded dataset.
                    The slopes for these profiles will be reset to best slope:"),
            DTOutput(ns("mismatched_table")),
            easyClose = TRUE,
            footer = NULL
          ))

          output$mismatched_table <- DT::renderDT({
            datatable(mismatched_points %>% select(-IX))
          })
        }

        # If there were changed rows update the concentration data & exclusions table
        if (nrow(changed_rows) > 0) {

          # Update the manual slopes object
          data$conc$data <- new_conc_data
          conc_groups <- unname(unlist(processed_pknca_data()$conc$columns$groups))
          manual_slopes <- new_conc_data %>%
            filter(is.included.hl + is.excluded.hl > 0) %>%
            rename(Inclusion = is.included.hl, Exclusion = is.excluded.hl) %>%
            pivot_longer(cols = c("Inclusion", "Exclusion"),
                         names_to = "Type",
                         values_to = "is.selected") %>%
            filter(is.selected) %>%
            group_by(conc_groups, Type) %>%
            mutate(Range = paste0(IX, collapse = ",")) %>%
            select(any_of(c(conc_groups, "Type", "Range", "REASON"))) %>%
            unique()
        }
      }
      ########################################################################################
    })
  })
}

# Helper function to validate the Excel file structure
.validate_excel_file <- function(setts, sheets) {
  expected_sheets <- c("intervals", "units", "conc_data", "conc_columns",
                       "dose_data", "dose_columns", "flag_rules", "options")
  missing_sheets <- setdiff(expected_sheets, sheets)

  if (length(missing_sheets) > 0) {
    showNotification(paste0("Invalid Excel settings file. Missing sheet/s: ",
                            paste0(missing_sheets, collapse = ", ")))
    stop("Invalid Excel file structure.")
  }

  if (!all(c("ind", "values") %in% names(setts$conc_columns))) {
    showNotification("Error: Excel file has incorrect column structure in 'conc_columns'.",
                     type = "error")
    stop("Invalid Excel file structure.")
  }

  if (!all(c("ind", "values") %in% names(setts$dose_columns))) {
    showNotification("Error: Excel file has incorrect column structure in 'dose_columns'.",
                     type = "error")
    stop("Invalid Excel file structure.")
  }
}

# Create a function to update the selectInputs based on the matches between settings and data
# If there are mismatches send a notification to the user
.updateselectinput_with_setts <- function(var_setts_col, var_data_col, setts_df,
                                          data_df, session, inputid) {

  if (!is.null(var_data_col) && !is.null(var_setts_col)) {
    vals_data <- unique(data_df[[var_data_col]])
    vals_setts <- unique(setts_df[[var_setts_col]])

    selected_vals <- intersect(vals_setts, vals_data)

    if (length(selected_vals) < length(vals_setts)) {
      showNotification(
        validate("The ", attr(data_df[[var_data_col]], "label"), ":",
                 setdiff(analytes_setts, selected_vals),
                 "selected in the settings file is not present in the data."),
        type = "warning"
      )
    }

    if (length(selected_vals) > 0) {
      updateSelectInput(
        session,
        inputId = inputid,
        choices = vals_data,
        selected = selected_vals
      )
    }

  } else {
    return()
  }
}
