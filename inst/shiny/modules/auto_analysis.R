#' Auto-Analysis Module
#'
#' Generates an R script from the current app settings (or uploaded settings)
#' using get_code(), executes it over the loaded data, and exports the
#' results as a ZIP file.

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

    observeEvent(session$userData$auto_analysis_trigger(), {
      req(session$userData$auto_analysis_trigger() > 0)

      raw_data <- tryCatch(raw_data_reactive(), error = function(e) NULL)
      if (is.null(raw_data) || nrow(raw_data) == 0) {
        showNotification("No data loaded. Please upload a dataset first.",
                         type = "error", duration = 5)
        return()
      }

      settings_override <- tryCatch(settings_override_reactive(), error = function(e) NULL)

      loading_popup("Running auto-analysis...")

      tryCatch({
        # 1. Build a settings object for get_code()
        setts_obj <- .build_settings_object(raw_data, settings_override)

        # 2. Save raw data to temp file so the generated script can read it
        data_tmp <- file.path(tempdir(), "auto_input_data.rds")
        saveRDS(raw_data, data_tmp)

        # 3. Generate the R script from the template
        script_path <- file.path(tempdir(), "auto_analysis_script.R")
        template_path <- system.file(
          "www/templates/auto_analysis_template.R", package = "aNCA"
        )
        get_code(
          setts_obj = setts_obj,
          output_path = script_path,
          template_path = template_path
        )

        # 4. Source the script in a dedicated environment
        log_info("Auto-analysis: executing generated script...")
        run_env <- new.env(parent = globalenv())
        run_env$data_path <- data_tmp
        source(script_path, local = run_env)

        # 5. Extract results
        results <- run_env$auto_results

        # 6. Save to ZIP
        output_dir <- file.path(tempdir(), "auto_analysis_output")
        unlink(output_dir, recursive = TRUE)
        dir.create(output_dir, recursive = TRUE)

        save_output(
          output = results,
          output_path = output_dir,
          table_formats = c("csv", "rds", "xpt")
        )
        .export_session_info(output_dir)

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

        # Trigger the hidden download button
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

#' Build a plain settings list for get_code() from uploaded settings or defaults.
#'
#' Mirrors the structure that get_code() expects (settings_list$...),
#' using uploaded settings when available, otherwise auto-detecting from data.
#'
#' @param raw_data The raw data frame.
#' @param settings_override Uploaded settings list (or NULL).
#' @return A plain list compatible with get_code(setts_obj = ...).
#' @noRd
.build_settings_object <- function(raw_data, settings_override = NULL) {
  col_names <- names(raw_data)
  has_override <- !is.null(settings_override)
  sett <- if (has_override) settings_override$settings else NULL

  # Mapping: from uploaded settings or auto-detected
  mapping <- if (has_override && !is.null(settings_override$mapping)) {
    settings_override$mapping
  } else {
    .build_auto_mapping(col_names)
  }

  # Filters: from uploaded settings or none
  applied_filters <- if (has_override) settings_override$filters else NULL

  # NCA settings: from uploaded or defaults
  method <- sett$method %||% "linear"
  start_impute <- sett$data_imputation$impute_c0 %||% TRUE
  blq_rule <- if (!is.null(sett)) sett$data_imputation$blq_imputation_rule else NULL
  bioavailability <- if (!is.null(sett)) sett$bioavailability else NULL
  general_exclusions <- if (!is.null(sett)) sett$general_exclusions else NULL
  int_parameters <- if (!is.null(sett)) sett$int_parameters else NULL
  param_selections <- if (!is.null(sett) && !is.null(sett$parameters)) sett$parameters$selections else NULL

  # Analyte/profile/pcspec: from settings or will be derived from data after mapping.
  # We need to apply mapping to know what columns exist, so we do a quick mapping pass.
  mapped_data <- tryCatch({
    raw_data %>%
      apply_mapping(mapping, silent = TRUE) %>%
      create_metabfl(mapping$Metabolites) %>%
      adjust_class_and_length(metadata_nca_variables, adjust_length = FALSE)
  }, error = function(e) NULL)

  if (!is.null(mapped_data)) {
    analyte <- sett$analyte %||% unique(mapped_data[["PARAM"]])
    profile <- sett$profile %||% unique(mapped_data[["ATPTREF"]])
    pcspec <- sett$pcspec %||% unique(mapped_data[["PCSPEC"]])
  } else {
    analyte <- sett$analyte
    profile <- sett$profile
    pcspec <- sett$pcspec
  }

  # Flag rules: from settings or defaults (all unchecked)
  flags <- if (!is.null(sett) && !is.null(sett$flags)) {
    sett$flags
  } else {
    list(
      R2ADJ = list(is.checked = FALSE, threshold = 0),
      R2 = list(is.checked = FALSE, threshold = 0),
      AUCPEO = list(is.checked = FALSE, threshold = 0),
      AUCPEP = list(is.checked = FALSE, threshold = 0),
      LAMZSPN = list(is.checked = FALSE, threshold = 0)
    )
  }

  # Slope rules, ratio table, units, extra vars
  slope_rules <- if (has_override) settings_override$slope_rules else NULL
  ratio_table <- if (!is.null(sett)) sett$ratio_table else NULL
  units_table <- if (!is.null(sett)) sett$units else NULL

  # Grouping variables from mapping
  extra_vars <- mapping$Grouping_Variables
  extra_vars <- if (is.null(extra_vars) || all(extra_vars == "")) character(0) else extra_vars

  list(
    mapping = mapping,
    applied_filters = applied_filters,
    time_duplicate_rows = NULL,
    settings = list(
      method = method,
      analyte = analyte,
      profile = profile,
      pcspec = pcspec,
      data_imputation = list(
        impute_c0 = start_impute,
        blq_imputation_rule = blq_rule
      ),
      bioavailability = bioavailability,
      general_exclusions = general_exclusions,
      int_parameters = int_parameters,
      parameters = list(selections = param_selections),
      flags = flags
    ),
    slope_rules = slope_rules,
    ratio_table = ratio_table,
    units_table = units_table,
    extra_vars_to_keep = extra_vars
  )
}

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
