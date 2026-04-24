# ratio_calculations_table.R
# Module for dynamic ratio calculation table in Shiny

ratios_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 10,
        actionButton(ns("add_row"), "(+) Add Row", class = "btn-success"),
        actionButton(ns("remove_row"), "(-) Remove Row/s", class = "btn-warning")
      ),
      column(width = 2, .ratio_help_dropdown())
    ),
    uiOutput(ns("ratio_cards"))
  )
}

# Help dropdown for the ratio calculations panel.
.ratio_help_dropdown <- function() {
  dropdown(
    div(
      tags$h2("Ratio Calculations Help"),
      p("
        This section is to perform ratio calculations within the allowed parameters
        that you previously selected. Add a new row for each ratio calculation to
        compute. You can also select and remove rows.
      "),
      p("For each ratio you need to specify:"),
      tags$ul(
        tags$li(
          tags$b("PPTESTCD"),
          ": Code name for the ratio. By default, unique
          CDISC style names are generated."
        ),
        tags$li(
          tags$b("TestParameter"),
          ": The parameter to use for the test (numerator)."
        ),
        tags$li(
          tags$b("TestGroups"),
          ": The level/value to use as test (numerator). If you select 'all other levels,'
          the ratio is calculated using the reference level (e.g., Group = A) against all
          other values of the variable (e.g., Groups = B, C, D)"
        ),
        tags$li(
          tags$b("Aggregate Subject"),
          ": Shown as \u03a3 in the denominator. `yes` (\u03a3) aggregates reference
          values using the mean of all subjects, `no` (\u2014) does not, and
          `if-needed` (\u03a3?) only when ratios cannot be performed within the same subject."
        ),
        tags$li(
          tags$b("RefParameter"),
          ": The parameter to use for the reference (denominator)."
        ),
        tags$li(
          tags$b("RefGroups"),
          ": The level/value to use as reference (denominator)."
        ),
        tags$li(
          tags$b("Adjusting Factor"),
          ": Factor to multiply the ratio with i.e, for molecular weight ratios
          (MW_ref / MW_test)."
        )
      ),
      tags$div(
        withMathJax("$$\\text(Parameter_{test} / Parameter_{reference(s)}) * AdjFactor$$")
      )
    ),
    style = "unite",
    right = TRUE,
    icon = icon("question"),
    status = "primary",
    width = "500px"
  )
}

#' Validate imported ratio rows against available options.
#' @param ratio_df Data.frame of imported ratio rows.
#' @param param_options Character vector of valid parameter codes.
#' @param ref_options Character vector of valid group reference strings.
#' @returns List with `valid` (data.frame) and `skipped` (character vector of reasons).
#' @noRd
ratios_table_server <- function(
    id, adnca_data, extra_group_vars, imported_ratios, int_parameters) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper: get group vars and group values
    ratio_groups <- reactive({
      adnca_data()$intervals %>%
        # Only consider main intervals for ratios
        filter(type_interval == "main") %>%
        # Remove interval parameter columns
        select(
          -any_of(
            c(
              names(PKNCA::get.interval.cols()), "impute", "type_interval", "VOLUME"
            )
          )
        ) %>%
        # Filter out the columns with one one unique value (no ratio possible!)
        select(where(~ length(unique(.)) > 1)) %>%
        select(-any_of(adnca_data()$conc$columns$subject))
    })

    ratio_reference_options <- reactive({
      .build_ratio_reference_options(ratio_groups())
    })

    ratio_param_options <- reactive({
      main_params <- adnca_data()$intervals %>%
        # Only consider main intervals for ratios
        filter(type_interval == "main") %>%
        select(
          any_of(setdiff(names(PKNCA::get.interval.cols()), c("start", "end")))
        ) %>%
        # For logical columns transform all FALSE to NA
        mutate(across(where(is.logical), ~ ifelse(. == FALSE, NA, .))) %>%
        # Select only columns where all values are not NA
        select(where(~ !all(is.na(.)))) %>%
        names() %>%
        purrr::keep(~ grepl("^(auc[itl\\.]|cmax|ae)", ., ignore.case = TRUE)) %>%
        translate_terms("PKNCA", "PPTESTCD")

      # Append interval parameters with range suffix (e.g. AUCINT_0-20)
      interval_params <- .build_interval_param_options(int_parameters())
      unique(c(main_params, interval_params))
    })

    # Store table data
    ratio_table <- reactiveVal({
      data.frame(
        TestParameter = character(),
        RefParameter = character(),
        RefGroups = character(),
        TestGroups = character(),
        AggregateSubject = character(),
        AdjustingFactor = numeric(),
        PPTESTCD = character(),
        stringsAsFactors = FALSE
      )
    })

    # Restore ratios from uploaded settings (append to existing rows).
    # pending_ratios stores the import until param/group options are available.
    pending_ratios <- reactiveVal(NULL)

    observeEvent(imported_ratios(), {
      req(imported_ratios())
      pending_ratios(imported_ratios())
    })

    observe({
      req(pending_ratios(), ratio_param_options(), ratio_reference_options())
      ratio_df <- isolate(pending_ratios())
      pending_ratios(NULL)
      .import_ratio_table(
        ratio_df, ratio_param_options(), ratio_reference_options(),
        ratio_table, refresh_cards
      )
    })

    # Add row
    observeEvent(input$add_row, {
      .add_ratio_row(
        ratio_param_options(), ratio_reference_options(),
        adnca_data(), ratio_table, refresh_cards
      )
    })

    # Remove selected rows
    observeEvent(input$remove_row, {
      .remove_selected_ratio_rows(input, ratio_table, refresh_cards)
    })

    # Render formula cards
    refresh_cards <- reactiveVal(1)
    output$ratio_cards <- renderUI({
      refresh_cards()
      tbl <- ratio_table()
      if (nrow(tbl) == 0) return(NULL)

      param_opts <- ratio_param_options()
      ref_opts <- ratio_reference_options()
      group_opts <- c(ref_opts, "(all other levels)")

      card_list <- lapply(seq_len(nrow(tbl)), function(i) {
        row <- tbl[i, ]
        .ratio_formula_card(ns, i, row, param_opts, ref_opts, group_opts)
      })
      tagList(card_list)
    })

    # Update table when any card input changes
    observe({
      tbl <- ratio_table()
      if (nrow(tbl) == 0) return()
      for (i in seq_len(nrow(tbl))) {
        .observe_ratio_row(i, input, ratio_table, adnca_data, refresh_cards)
      }
    })

    # Return the table as a reactive
    ratio_table
  })
}

# Remove rows selected via checkboxes.
.remove_selected_ratio_rows <- function(input, ratio_table, refresh_cards) {
  tbl <- ratio_table()
  req(nrow(tbl) > 0)
  selected <- which(vapply(seq_len(nrow(tbl)), function(i) {
    isTRUE(input[[paste0("select_row_", i)]])
  }, logical(1)))
  req(length(selected) > 0)
  ratio_table(tbl[-selected, , drop = FALSE])
  refresh_cards(refresh_cards() + 1)
}

# Add a new ratio row with default values.
.add_ratio_row <- function(param_opts, ref_opts, adnca_data,
                           ratio_table, refresh_cards) {
  if (length(param_opts) == 0 || length(ref_opts) == 0) {
    showNotification(
      "No parameters or group variables available to add a row.",
      type = "warning"
    )
    return()
  }
  new_row <- data.frame(
    TestParameter = param_opts[1],
    RefParameter = param_opts[1],
    RefGroups = ref_opts[1],
    TestGroups = "(all other levels)",
    AggregateSubject = "no",
    AdjustingFactor = 1,
    PPTESTCD = "",
    stringsAsFactors = FALSE
  ) %>%
    .generate_pptestcd_for_ratios(adnca_data = adnca_data)
  ratio_table(rbind(ratio_table(), new_row))
  refresh_cards(refresh_cards() + 1)
}

# Import and validate a ratio table from settings.
.import_ratio_table <- function(ratio_df, param_opts, ref_opts,
                                ratio_table, refresh_cards) {
  ratio_df <- .coerce_ratio_df(ratio_df)
  if (is.null(ratio_df) || nrow(ratio_df) == 0) return()

  if (!.has_required_ratio_cols(ratio_df)) {
    showNotification("Skipped ratio import: missing required columns",
                     type = "warning", duration = 10)
    return()
  }

  result <- .validate_ratio_table(ratio_df, param_opts, ref_opts)

  if (length(result$skipped) > 0) {
    showNotification(
      paste0("Skipped ratio rows:\n", paste(result$skipped, collapse = "\n")),
      type = "warning", duration = 10
    )
  }

  if (any(result$keep)) {
    ratio_table(ratio_df[result$keep, , drop = FALSE])
    refresh_cards(refresh_cards() + 1)
  }
}

# Register input observers for a single ratio row.
# Handles select fields, numeric AdjustingFactor, and text PPTESTCD.
.observe_ratio_row <- function(idx, input, ratio_table, adnca_data, refresh_cards) {
  local({
    i <- idx
    # Select inputs (dropdowns)
    for (field in c("TestParameter", "RefParameter", "RefGroups",
                    "TestGroups", "AggregateSubject")) {
      local({
        fld <- field
        observeEvent(input[[paste0(fld, "_", i)]], {
          val <- input[[paste0(fld, "_", i)]]
          current <- ratio_table()
          if (i <= nrow(current) && !identical(current[i, fld], val)) {
            current[i, fld] <- val
            if (fld %in% c("TestParameter", "RefGroups", "TestGroups")) {
              current <- .generate_pptestcd_for_ratios(
                current, adnca_data = adnca_data()
              )
            }
            ratio_table(current)
            if (fld != "AggregateSubject") {
              refresh_cards(refresh_cards() + 1)
            }
          }
        }, ignoreInit = TRUE)
      })
    }
    # Numeric input
    observeEvent(input[[paste0("AdjustingFactor_", i)]], {
      val <- input[[paste0("AdjustingFactor_", i)]]
      current <- ratio_table()
      if (i <= nrow(current) && !identical(current[i, "AdjustingFactor"], val)) {
        current[i, "AdjustingFactor"] <- val
        ratio_table(current)
      }
    }, ignoreInit = TRUE)
    # Text input
    observeEvent(input[[paste0("PPTESTCD_", i)]], {
      val <- input[[paste0("PPTESTCD_", i)]]
      current <- ratio_table()
      if (i <= nrow(current) && !identical(current[i, "PPTESTCD"], val)) {
        current[i, "PPTESTCD"] <- val
        ratio_table(current)
      }
    }, ignoreInit = TRUE)
  })
}

# Build a single formula card for ratio row i.
# Fraction layout:
#                     TestParam [TestGroup]                 #nolint
#  ☐  PPTESTCD  =  ─────────────────────────  ×  AdjFactor
#                  Σ  RefParam [RefGroup]
.ratio_formula_card <- function(ns, i, row, param_opts, ref_opts, group_opts) {
  # Map aggregate values to symbol labels for the dropdown
  agg_choices <- c(
    "\u2014"  = "no",
    "\u03a3"  = "yes",
    "\u03a3?" = "if-needed"
  )

  tags$div(
    class = "ratio-formula-card",
    # Left side: checkbox + PPTESTCD + equals
    tags$div(
      class = "ratio-left",
      tags$div(
        class = "ratio-select",
        checkboxInput(ns(paste0("select_row_", i)), label = NULL, value = FALSE)
      ),
      tags$div(
        class = "ratio-pptestcd-input",
        title = "PPTESTCD (editable)",
        textInput(
          ns(paste0("PPTESTCD_", i)), label = NULL,
          value = row$PPTESTCD, width = "110px"
        )
      ),
      tags$span(class = "ratio-eq", "=")
    ),
    tags$div(
      class = "ratio-fraction",
      # Numerator
      tags$div(
        class = "ratio-numerator",
        tags$div(
          class = "ratio-input-wrap",
          title = "Test Parameter (numerator)",
          selectInput(
            ns(paste0("TestParameter_", i)), label = NULL,
            choices = param_opts, selected = row$TestParameter, width = "130px"
          )
        ),
        tags$span(class = "ratio-bracket", "["),
        tags$div(
          class = "ratio-input-wrap",
          title = "Test Group (numerator level)",
          selectInput(
            ns(paste0("TestGroups_", i)), label = NULL,
            choices = group_opts, selected = row$TestGroups, width = "180px"
          )
        ),
        tags$span(class = "ratio-bracket", "]")
      ),
      # Fraction line
      tags$hr(class = "ratio-line"),
      # Denominator
      tags$div(
        class = "ratio-denominator",
        tags$div(
          class = "ratio-sigma",
          title = "Aggregate Subject: \u2014 = no, \u03a3 = yes, \u03a3? = if-needed",
          selectInput(
            ns(paste0("AggregateSubject_", i)), label = NULL,
            choices = agg_choices, selected = row$AggregateSubject,
            width = "55px"
          )
        ),
        tags$div(
          class = "ratio-input-wrap",
          title = "Reference Parameter (denominator)",
          selectInput(
            ns(paste0("RefParameter_", i)), label = NULL,
            choices = param_opts, selected = row$RefParameter, width = "130px"
          )
        ),
        tags$span(class = "ratio-bracket", "["),
        tags$div(
          class = "ratio-input-wrap",
          title = "Reference Group (denominator level)",
          selectInput(
            ns(paste0("RefGroups_", i)), label = NULL,
            choices = ref_opts, selected = row$RefGroups, width = "180px"
          )
        ),
        tags$span(class = "ratio-bracket", "]")
      )
    ),
    # Right side: multiply + adjusting factor
    tags$div(
      class = "ratio-right",
      tags$span(class = "ratio-mult", "\u00d7"),
      tags$div(
        class = "ratio-adj-factor",
        title = "Adjusting Factor (multiplier)",
        numericInput(
          ns(paste0("AdjustingFactor_", i)), label = NULL,
          value = row$AdjustingFactor, width = "70px", step = 0.1
        )
      )
    )
  )
}

# Build interval parameter options with range suffix from int_parameters table.
# Rows with NA start/end are excluded since they represent incomplete definitions.
# Returns e.g. c("AUCINT_0-20", "AUCINT_0-30").
.build_interval_param_options <- function(int_params) {
  if (is.null(int_params) || nrow(int_params) == 0) {
    return(character(0))
  }
  complete <- !is.na(int_params$start_auc) & !is.na(int_params$end_auc)
  if (!any(complete)) {
    return(character(0))
  }
  int_params <- int_params[complete, , drop = FALSE]
  paste0(int_params$parameter, "_", int_params$start_auc, "-", int_params$end_auc)
}

# Build reference options from ratio group columns.
.build_ratio_reference_options <- function(groups_df) {
  if (ncol(groups_df) == 0) {
    return(NULL)
  }
  groups_df %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(cols = everything()) %>%
    mutate(input_name = paste0(name, ": ", value)) %>%
    pull(input_name) %>%
    unique() %>%
    sort()
}

.generate_pptestcd_for_ratios <- function(tbl, adnca_data) {
  analyte_col <- adnca_data$conc$columns$groups$group_analyte
  profile_col <- "ATPTREF"
  pcspec_col <- "PCSPEC"
  raw_route_col <- "ROUTE"
  duration_col <- adnca_data$dose$columns$duration
  route_col <- adnca_data$dose$columns$route

  tbl %>%
    mutate(
      PPTESTCD = case_when(
        startsWith(RefGroups, analyte_col) ~ paste0("MR", TestParameter),
        startsWith(RefGroups, profile_col) ~ paste0("AR", TestParameter),
        startsWith(TestGroups, paste0(pcspec_col, ": URINE")) &
          startsWith(RefGroups, pcspec_col) &
          !endsWith(RefGroups, "URINE") &
          startsWith(TestParameter, "RCAMINT") &
          startsWith(RefParameter, "AUC") ~ "RENALCL",
        startsWith(
          toupper(RefGroups),
          paste0(toupper(route_col), ": INTRAVASCULAR")
        ) ~ "FABS",
        startsWith(toupper(RefGroups), paste0(toupper(raw_route_col), ": INTRA")) ~ "FABS",
        startsWith(RefGroups, paste0(route_col)) ~ "FREL",
        startsWith(RefGroups, raw_route_col) ~ "FREL",
        TRUE ~ paste0("RA", TestParameter)
      ),
      PPTESTCD = make.unique(PPTESTCD, sep = "")
    )
}

# Coerce imported ratio data to a data frame, handling YAML empty arrays.
.coerce_ratio_df <- function(ratio_df) {
  if (!is.data.frame(ratio_df)) {
    ratio_df <- tryCatch(as.data.frame(ratio_df, stringsAsFactors = FALSE),
                         error = function(e) NULL)
  }
  ratio_df
}

# Check that a ratio data frame has all required columns.
.has_required_ratio_cols <- function(ratio_df) {
  required_cols <- c(
    "TestParameter", "RefParameter", "RefGroups", "TestGroups",
    "AggregateSubject", "AdjustingFactor", "PPTESTCD"
  )
  all(required_cols %in% names(ratio_df))
}

# Validate all rows in a ratio table. Returns list(keep, skipped).
.validate_ratio_table <- function(ratio_df, param_options, ref_options) {
  all_group_options <- c(ref_options, "(all other levels)")
  valid_agg <- c("yes", "no", "if-needed")
  skipped <- character()
  keep <- logical(nrow(ratio_df))

  for (i in seq_len(nrow(ratio_df))) {
    reasons <- .validate_ratio_row(
      ratio_df[i, ], param_options, ref_options, all_group_options, valid_agg
    )
    if (length(reasons) == 0) {
      keep[i] <- TRUE
    } else {
      skipped <- c(skipped, paste0("Row ", i, " (", ratio_df$PPTESTCD[i], "): ",
                                   paste(reasons, collapse = "; ")))
    }
  }
  list(keep = keep, skipped = skipped)
}

# Validate a single ratio row against available options.
# Returns a character vector of reasons (empty if valid).
.validate_ratio_row <- function(row, param_options, ref_options,
                                all_group_options, valid_agg) {
  reasons <- character()
  if (!row$TestParameter %in% param_options)
    reasons <- c(reasons, paste0("TestParameter '", row$TestParameter, "' not available"))
  if (!row$RefParameter %in% param_options)
    reasons <- c(reasons, paste0("RefParameter '", row$RefParameter, "' not available"))
  if (!row$RefGroups %in% ref_options)
    reasons <- c(reasons, paste0("RefGroups '", row$RefGroups, "' not available"))
  if (!row$TestGroups %in% all_group_options)
    reasons <- c(reasons, paste0("TestGroups '", row$TestGroups, "' not available"))
  if (!tolower(row$AggregateSubject) %in% valid_agg)
    reasons <- c(reasons, paste0("AggregateSubject '", row$AggregateSubject, "' invalid"))
  if (!is.numeric(row$AdjustingFactor) || length(row$AdjustingFactor) != 1 ||
        is.na(row$AdjustingFactor))
    reasons <- c(reasons, paste0(
      "AdjustingFactor '", row$AdjustingFactor, "' must be a single numeric value"
    ))
  reasons
}
