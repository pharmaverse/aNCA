#' Auto-Analysis Module
#'
#' Runs the full NCA pipeline programmatically using the loaded data and
#' default (or auto-detected) settings, then exports results as a ZIP file.
#' Bypasses all manual UI steps (mapping, filtering, NCA setup).

#' @param id Module ID.
auto_analysis_ui <- function(id) {
  ns <- NS(id)
  # Hidden download button triggered after pipeline completes
  shinyjs::hidden(downloadButton(ns("download"), label = ""))
}

#' @param id Module ID.
#' @param raw_data_reactive Reactive returning the raw uploaded data frame.
#' @param settings_override_reactive Reactive returning uploaded settings (or NULL).
auto_analysis_server <- function(id, raw_data_reactive, settings_override_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    auto_zip_path <- reactiveVal(NULL)

    # Triggered by the button in data_upload_ui (via session$userData)
    observeEvent(session$userData$auto_analysis_trigger(), {
      req(session$userData$auto_analysis_trigger() > 0)

      raw_data <- tryCatch(raw_data_reactive(), error = function(e) NULL)
      if (is.null(raw_data) || nrow(raw_data) == 0) {
        showNotification("No data loaded. Please upload a dataset first.",
                         type = "error", duration = 5)
        return()
      }

      settings <- tryCatch(settings_override_reactive(), error = function(e) NULL)

      loading_popup("Running auto-analysis...")

      tryCatch({
        results <- .run_auto_pipeline(raw_data, settings)

        # Build ZIP
        output_dir <- file.path(tempdir(), "auto_analysis_output")
        unlink(output_dir, recursive = TRUE)
        dir.create(output_dir, recursive = TRUE)

        .save_auto_results(results, output_dir)

        zip_path <- file.path(tempdir(), "auto_analysis.zip")
        wd <- getwd()
        on.exit(setwd(wd), add = TRUE)
        setwd(output_dir)
        files <- list.files(".", recursive = TRUE)
        zip::zipr(zipfile = zip_path, files = files, mode = "mirror")

        auto_zip_path(zip_path)

        log_success("Auto-analysis complete.")
        showNotification("Auto-analysis complete! Download starting...",
                         type = "message", duration = 5)

        # Trigger the hidden download button via JS
        shinyjs::runjs(sprintf(
          "document.getElementById('%s').click();",
          ns("download")
        ))
      }, error = function(e) {
        log_error("Auto-analysis failed: {conditionMessage(e)}")
        showNotification(
          paste0("Auto-analysis failed: ", conditionMessage(e)),
          type = "error", duration = NULL
        )
      }, finally = {
        later::later(~removeModal(session = session), delay = 0.5)
      })
    })

    output$download <- downloadHandler(
      filename = function() {
        pn <- tryCatch(session$userData$project_name(), error = function(e) "")
        if (is.null(pn) || pn == "") {
          raw <- tryCatch(raw_data_reactive(), error = function(e) NULL)
          if (!is.null(raw) && "STUDYID" %in% names(raw)) {
            ids <- unique(raw[["STUDYID"]])
            ids <- ids[!is.na(ids)]
            if (length(ids) > 3) ids <- ids[1:3]
            pn <- if (length(ids) > 0) paste0("NCA_", paste(ids, collapse = "_")) else "NCA"
          } else {
            pn <- "NCA"
          }
        }
        pn <- gsub("[^A-Za-z0-9_-]", "_", pn)
        paste0(pn, "_auto.zip")
      },
      content = function(fname) {
        req(auto_zip_path())
        file.copy(auto_zip_path(), fname)
      }
    )
  })
}

# --- Internal pipeline functions ---

#' Build a column mapping from data column names using the same auto-detection
#' logic as the mapping UI (update_selectize_inputs).
#' @param col_names Character vector of column names in the dataset.
#' @return Named list suitable for PKNCA_create_data_object(mapping = ...).
#' @noRd
.build_auto_mapping <- function(col_names) {
  mapping <- list()

  for (i in seq_len(nrow(MAPPING_INFO))) {
    var <- MAPPING_INFO$Variable[i]
    alternatives <- strsplit(MAPPING_INFO$mapping_alternatives[i], ", ")[[1]]
    values <- strsplit(MAPPING_INFO$Values[i], ", ")[[1]]
    is_multi <- MAPPING_INFO$is_multiple_choice[i]

    # Find matching columns: variable name itself + alternatives
    potential <- intersect(c(var, alternatives), col_names)

    if (length(potential) == 0) {
      # For non-multi-choice, try predefined values as fallback
      val_matches <- values[nchar(values) > 0]
      mapping[[var]] <- if (length(val_matches) > 0) val_matches[[1]] else ""
    } else if (is_multi) {
      mapping[[var]] <- potential
    } else {
      mapping[[var]] <- potential[[1]]
    }
  }

  mapping
}

#' Run the full NCA pipeline on raw data.
#' Uses uploaded settings when available, otherwise auto-detects defaults.
#' @param raw_data Data frame with raw PK data.
#' @param settings_override Optional settings list from an uploaded YAML file.
#'   Expected keys: mapping, filters, settings, slope_rules.
#' @return List with pipeline outputs.
#' @noRd
.run_auto_pipeline <- function(raw_data, settings_override = NULL) {
  sett <- settings_override$settings
  has_settings <- !is.null(sett)

  # --- Mapping ---
  if (!is.null(settings_override$mapping)) {
    log_info("Auto-analysis: using uploaded mapping.")
    mapping <- settings_override$mapping
  } else {
    log_info("Auto-analysis: auto-detecting mapping from column names...")
    mapping <- .build_auto_mapping(names(raw_data))
  }

  # --- Filters ---
  applied_filters <- settings_override$filters

  log_info("Auto-analysis: creating PKNCA data object...")
  pknca_obj <- PKNCA_create_data_object(
    adnca_data = raw_data,
    mapping = mapping,
    applied_filters = applied_filters,
    time_duplicate_rows = NULL
  )

  # Simplify volume units (same as tab_data_server)
  pknca_obj$units <- pknca_obj$units %>%
    mutate(
      PPSTRESU = {
        new_ppstresu <- ifelse(
          PPTESTCD %in% metadata_nca_parameters$PKNCA[
            metadata_nca_parameters$unit_type == "volume"
          ],
          sapply(PPSTRESU, function(x) simplify_unit(x, as_character = TRUE)),
          PPSTRESU
        )
        ifelse(nchar(new_ppstresu) < 3, new_ppstresu, .data[["PPSTRESU"]])
      },
      conversion_factor = ifelse(
        PPTESTCD %in% metadata_nca_parameters$PKNCA[
          metadata_nca_parameters$unit_type == "volume"
        ],
        get_conversion_factor(PPORRESU, PPSTRESU),
        conversion_factor
      )
    )

  # --- NCA settings: use uploaded values or derive from data ---
  conc_data <- pknca_obj$conc$data
  method <- if (has_settings && !is.null(sett$method)) sett$method else "linear"
  analytes <- if (has_settings && !is.null(sett$analyte)) sett$analyte else unique(conc_data[["PARAM"]])
  profiles <- if (has_settings && !is.null(sett$profile)) sett$profile else unique(conc_data[["ATPTREF"]])
  pcspec <- if (has_settings && !is.null(sett$pcspec)) sett$pcspec else unique(conc_data[["PCSPEC"]])
  start_impute <- if (has_settings && !is.null(sett$data_imputation$impute_c0)) sett$data_imputation$impute_c0 else TRUE
  blq_rule <- if (has_settings) sett$data_imputation$blq_imputation_rule else NULL
  exclusion_list <- if (has_settings) sett$general_exclusions else NULL
  param_selections <- if (has_settings && !is.null(sett$parameters$selections)) sett$parameters$selections else NULL
  int_parameters <- if (has_settings) sett$int_parameters else NULL
  slope_rules <- settings_override$slope_rules
  bioavailability <- if (has_settings) sett$bioavailability else NULL
  flag_rules <- if (has_settings) sett$flags else NULL
  ratio_table <- if (has_settings) sett$ratio_table else NULL

  log_info("Auto-analysis: configuring NCA settings...")
  log_info("  Method: {method}")
  log_info("  Analytes: {paste(analytes, collapse = ', ')}")
  log_info("  Profiles: {paste(profiles, collapse = ', ')}")
  log_info("  Specimens: {paste(pcspec, collapse = ', ')}")
  if (has_settings) log_info("  Using uploaded settings file.")

  pknca_obj <- PKNCA_update_data_object(
    adnca_data = pknca_obj,
    method = method,
    selected_analytes = analytes,
    selected_profile = profiles,
    selected_pcspec = pcspec,
    start_impute = start_impute,
    hl_adj_rules = slope_rules,
    exclusion_list = exclusion_list,
    keep_interval_cols = NULL,
    parameter_selections = param_selections,
    int_parameters = int_parameters,
    blq_imputation_rule = blq_rule
  )

  log_info("Auto-analysis: running NCA calculations...")
  pknca_res <- pknca_obj %>%
    PKNCA_calculate_nca(blq_rule = blq_rule) %>%
    add_f_to_pknca_results(bioavailability) %>%
    mutate(PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTESTCD"))

  # Apply flag rules if provided
  if (!is.null(flag_rules)) {
    pknca_res <- pknca_res %>%
      PKNCA_hl_rules_exclusion(
        rules = flag_rules %>%
          purrr::keep(\(x) x$is.checked) %>%
          purrr::map(\(x) x$threshold)
      )
  }

  # Apply ratio calculations if provided
  if (!is.null(ratio_table)) {
    pknca_res <- pknca_res %>%
      calculate_table_ratios(ratio_table = ratio_table)
  }

  pknca_res <- pknca_res %>%
    remove_pp_not_requested()

  log_info("Auto-analysis: generating CDISC datasets...")
  cdisc_datasets <- export_cdisc(pknca_res, grouping_vars = character(0))

  log_info("Auto-analysis: pivoting results...")
  pivoted_results <- pivot_wider_pknca_results(
    myres = pknca_res,
    flag_rules = flag_rules
  )

  log_info("Auto-analysis: computing summary statistics...")
  summary_stats <- tryCatch(
    calculate_summary_stats(data = pknca_res$result),
    error = function(e) {
      log_warn("Summary statistics failed: {conditionMessage(e)}")
      NULL
    }
  )

  list(
    pknca_res = pknca_res,
    pknca_obj = pknca_obj,
    cdisc_datasets = cdisc_datasets,
    pivoted_results = pivoted_results,
    summary_stats = summary_stats
  )
}

#' Save auto-analysis results to a directory structure compatible with the ZIP export.
#' @param results List from .run_auto_pipeline().
#' @param output_dir Target directory.
#' @noRd
.save_auto_results <- function(results, output_dir) {
  # NCA results (pivoted table)
  nca_dir <- file.path(output_dir, "nca_results")
  dir.create(nca_dir, recursive = TRUE, showWarnings = FALSE)
  save_table_format(results$pivoted_results, file.path(nca_dir, "nca_pkparam"),
                    formats = c("csv", "rds"))

  # Summary statistics
  if (!is.null(results$summary_stats)) {
    save_table_format(results$summary_stats, file.path(nca_dir, "nca_statistics"),
                      formats = c("csv", "rds"))
  }

  # CDISC datasets
  cdisc_dir <- file.path(output_dir, "CDISC")
  dir.create(cdisc_dir, recursive = TRUE, showWarnings = FALSE)
  for (ds_name in names(results$cdisc_datasets)) {
    save_table_format(results$cdisc_datasets[[ds_name]],
                      file.path(cdisc_dir, ds_name),
                      formats = c("csv", "rds", "xpt"))
  }

  # Session info
  .export_session_info(output_dir)
}
