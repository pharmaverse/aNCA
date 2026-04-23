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
      column(
        width = 2,
        dropdown(
          div(
            tags$h2("Ratio Calculations Help"),
            p(
              "Add a ratio calculation for each comparison you need.",
              "Each entry is displayed as a formula:"
            ),
            tags$div(
              class = "ratio-formula-example",
              tags$span(class = "ratio-pptestcd", "PPTESTCD"),
              tags$span(class = "ratio-eq", "="),
              tags$span(class = "ratio-numerator", "Parameter [Test Group]"),
              tags$span(class = "ratio-div", "/"),
              tags$span(class = "ratio-denominator", "Parameter [Ref Group]"),
              tags$span(class = "ratio-mult", "\u00d7"),
              tags$span("AdjFactor")
            ),
            tags$hr(),
            tags$ul(
              tags$li(
                tags$b("Test / Ref Parameter"),
                ": The PK parameter for numerator / denominator."
              ),
              tags$li(
                tags$b("Test / Ref Groups"),
                ": The group level for numerator / denominator.",
                "'(all other levels)' computes ratios against all non-reference levels."
              ),
              tags$li(
                tags$b("Aggregate"),
                ": 'yes' averages reference across subjects,",
                "'no' matches within subject, 'if-needed' falls back to aggregation."
              ),
              tags$li(
                tags$b("Adj. Factor"),
                ": Multiplier (e.g. MW ratio). Default 1."
              ),
              tags$li(
                tags$b("PPTESTCD"),
                ": Auto-generated CDISC code. Editable."
              )
            )
          ),
          style = "unite",
          right = TRUE,
          icon = icon("question"),
          status = "primary",
          width = "500px"
        )
      )
    ),
    uiOutput(ns("ratio_cards"))
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

      ratio_df <- .coerce_ratio_df(ratio_df)
      if (is.null(ratio_df) || nrow(ratio_df) == 0) return()

      if (!.has_required_ratio_cols(ratio_df)) {
        showNotification("Skipped ratio import: missing required columns",
                         type = "warning", duration = 10)
        return()
      }

      result <- .validate_ratio_table(
        ratio_df, ratio_param_options(), ratio_reference_options()
      )

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
    })

    # Add row
    observeEvent(input$add_row, {
      if (length(ratio_param_options()) == 0 || length(ratio_reference_options()) == 0) {
        showNotification(
          "No parameters or group variables available to add a row.",
          type = "warning"
        )
        return()
      }

      # Add a new row with default values
      new_row <- data.frame(
        TestParameter = ratio_param_options()[1],
        RefParameter = ratio_param_options()[1],
        RefGroups = ratio_reference_options()[1],
        TestGroups = "(all other levels)",
        AggregateSubject = "no",
        AdjustingFactor = 1,
        PPTESTCD = "",
        stringsAsFactors = FALSE
      ) %>%
        .generate_pptestcd_for_ratios(adnca_data = adnca_data())

      updated <- rbind(ratio_table(), new_row)
      ratio_table(updated)
      refresh_cards(refresh_cards() + 1)
    })

    # Remove selected rows
    observeEvent(input$remove_row, {
      tbl <- ratio_table()
      req(nrow(tbl) > 0)
      selected <- which(vapply(seq_len(nrow(tbl)), function(i) {
        isTRUE(input[[paste0("select_row_", i)]])
      }, logical(1)))
      req(length(selected) > 0)
      ratio_table(tbl[-selected, , drop = FALSE])
      refresh_cards(refresh_cards() + 1)
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
        local({
          idx <- i
          # Observe select inputs for this row
          for (field in c("TestParameter", "RefParameter", "RefGroups",
                          "TestGroups")) {
            local({
              fld <- field
              input_id <- paste0(fld, "_", idx)
              observeEvent(input[[input_id]], {
                val <- input[[input_id]]
                current <- ratio_table()
                if (idx <= nrow(current) && !identical(current[idx, fld], val)) {
                  current[idx, fld] <- val
                  if (fld %in% c("TestParameter", "RefGroups", "TestGroups")) {
                    current <- .generate_pptestcd_for_ratios(
                      current, adnca_data = adnca_data()
                    )
                  }
                  ratio_table(current)
                  refresh_cards(refresh_cards() + 1)
                }
              }, ignoreInit = TRUE)
            })
          }
          # Sigma toggle: cycle through no -> yes -> if-needed -> no
          observeEvent(input[[paste0("toggle_agg_", idx)]], {
            current <- ratio_table()
            if (idx <= nrow(current)) {
              current[idx, "AggregateSubject"] <- switch(
                current[idx, "AggregateSubject"],
                "no" = "yes",
                "yes" = "if-needed",
                "no"
              )
              ratio_table(current)
              refresh_cards(refresh_cards() + 1)
            }
          }, ignoreInit = TRUE)
          # Numeric: AdjustingFactor
          observeEvent(input[[paste0("AdjustingFactor_", idx)]], {
            val <- input[[paste0("AdjustingFactor_", idx)]]
            current <- ratio_table()
            if (idx <= nrow(current) && !identical(current[idx, "AdjustingFactor"], val)) {
              current[idx, "AdjustingFactor"] <- val
              ratio_table(current)
            }
          }, ignoreInit = TRUE)
          # Text: PPTESTCD
          observeEvent(input[[paste0("PPTESTCD_", idx)]], {
            val <- input[[paste0("PPTESTCD_", idx)]]
            current <- ratio_table()
            if (idx <= nrow(current) && !identical(current[idx, "PPTESTCD"], val)) {
              current[idx, "PPTESTCD"] <- val
              ratio_table(current)
            }
          }, ignoreInit = TRUE)
        })
      }
    })

    # Return the table as a reactive
    ratio_table
  })
}

# Build a single formula card for ratio row i.
# Layout: [checkbox] PPTESTCD = TestParam [TestGroup] / RefParam [RefGroup] x AdjFactor
#         Aggregate: [dropdown]
# Build a single formula card for ratio row i.
# Fraction layout:
#                     TestParam [TestGroup]
#  ☐  PPTESTCD  =  ─────────────────────────  ×  AdjFactor
#                  Σ  RefParam [RefGroup]
.ratio_formula_card <- function(ns, i, row, param_opts, ref_opts, group_opts) {
  # Sigma label for aggregate mode
  agg_label <- switch(row$AggregateSubject,
    "yes" = "\u03a3",
    "if-needed" = "\u03a3?",
    "\u2014"
  )
  agg_title <- switch(row$AggregateSubject,
    "yes" = "Aggregate: yes (mean of reference subjects)",
    "if-needed" = "Aggregate: if-needed (fallback to mean)",
    "Aggregate: no (within-subject)"
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
    # Center: fraction (numerator / line / denominator)
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
          title = agg_title,
          actionLink(
            ns(paste0("toggle_agg_", i)),
            label = agg_label,
            class = "ratio-sigma-btn"
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
