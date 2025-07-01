# ratio_calculations_table.R
# Module for dynamic ratio calculation table in Shiny

ratios_table_ui <- function(id) {
  ns <- NS(id)
  fluidRow(

    # Main widgets for the ratio table
    div(
      class = "plot-widget-group",
      actionButton(ns("add_row"), "(+) Add Row", class = "btn-success"),
      actionButton(ns("remove_row"), "(-) Remove Row/s", class = "btn-warning"),
      # Help button
      dropdown(
        div(
          tags$h1("Ratio calculations guide"),
          p("
            This section is to perform ratio calculations within the allowed parameters
            that you previously selected. Add a new row for each ratio calculation to
            compute. You can also select and remove rows.
            "
          ),
          p("For each ratio you need to specify:"),
          tags$ul(
            tags$li(
              tags$b("Parameter"),
              ": The parameter you want to calculate the ratio for."
            ),
            tags$li(
              tags$b("Reference"),
              ": The level/value to use as reference (denominator)."
            ),
            tags$li(
              tags$b("Test"),
              ": The level/value to use as test (numerator). If you select '(all other levels)'
              will use all other levels of the Reference variable."
            ),
            tags$li(
              tags$b("Aggregate Subject"),
              ": `yes` aggregates reference values using the mean of all subjects, 
              `no` does not, and 
              `if-needed` only when ratios cannot be performed within the same subject."
            ),
            tags$li(
              tags$b("Adjusting Factor"),
              ": Factor to multiply the ratio with i.e, for molecular weight ratios 
              (MW_ref / MW_test)."
            ),
            tags$li(
              tags$b("PPTESTCD"),
              ": Code name for the ratio. By default, unique
              CDISC style names are generated."
            )
          ),
          tags$div(
            withMathJax("$$\\text(Parameter_{test} / Parameter_{reference(s)}) * AdjFactor$$")
          )
        ),
        style = "unite",
        right = TRUE,
        icon = icon("question"),
        status = "primary"
      ),
    ),
    fluidRow(
      reactableOutput(ns("ratio_calculations"))
    )
  )
}

ratios_table_server <- function(
  id, adnca_data
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper: get group vars and group values
    ratio_groups <- reactive({

      adnca_data()$intervals %>%
        # Only consider main intervals for ratios
        filter(type_interval == "main") %>%
        select(
          -any_of(c(names(PKNCA::get.interval.cols()), "impute", "type_interval"))
        ) %>%
        select(
          any_of(
            c(group_vars(adnca_data()$conc), "ROUTE", "NCA_PROFILE")
          )
        ) %>%
        # Filter out the columns with one one unique value (no ratio possible!)
        select(where(~ length(unique(.)) > 1)) %>%
        select(-any_of(adnca_data()$conc$columns$subject))
    })

    ratio_reference_options <- reactive({
      # We paste the column name and value to use as a specified input
      if (ncol(ratio_groups()) > 0) {
        ratio_groups() %>%
          # Convert all columns to character
          mutate(across(everything(), as.character)) %>%
          pivot_longer(cols = everything()) %>%
          mutate(input_name = paste0(name, ": ", value)) %>%
          pull(input_name) %>%
          unique() %>%
          sort()
      } else {
        NULL
      }
    })

    ratio_param_options <- reactive({
      adnca_data()$intervals %>%
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
        purrr::keep(~ grepl("^(auc[itl\\.]|cmax)", ., ignore.case = TRUE)) %>%
        translate_terms("PKNCA", "PPTESTCD")
    })

    # Table columns
    table_columns <- c(
      "Parameter", "Reference", "Test", "AggregateSubject", "AdjustingFactor", "PPTESTCD"
    )

    # Store table data
    ratio_table <- reactiveVal({
      data.frame(
        Parameter = character(),
        Reference = character(),
        Test = character(),
        AggregateSubject = character(),
        AdjustingFactor = numeric(),
        stringsAsFactors = FALSE
      )
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
      new_row <- data.frame(
        Parameter = ratio_param_options()[1],
        Reference = ratio_reference_options()[1],
        Test = "(all other levels)",
        AggregateSubject = "no",
        AdjustingFactor = 1,
        PPTESTCD = "",
        stringsAsFactors = FALSE
      )
      updated <- rbind(ratio_table(), new_row)
      ratio_table(updated)
      reset_reactable_memory()
      refresh_reactable(refresh_reactable() + 1)
    })

    # Remove selected rows
    observeEvent(input$remove_row, {
      selected <- getReactableState("ratio_calculations", "selected")
      req(selected)
      updated <- ratio_table()[-selected, ]
      ratio_table(updated)
      reset_reactable_memory()
      refresh_reactable(refresh_reactable() + 1)
    })

    # Render table
    refresh_reactable <- reactiveVal(1)
    output$ratio_calculations <- renderReactable({

      # Update column names for display in the UI
      col_defs <- list(
        Parameter = colDef(
          name = "Parameter",
          cell = dropdown_extra(
            id = ns("edit_Parameter"),
            choices = ratio_param_options(),
            class = "dropdown-extra"
          ),
          width = 180
        ),
        Reference = colDef(
          name = "Reference",
          cell = dropdown_extra(
            id = ns("edit_Reference"),
            choices = ratio_reference_options(),
            class = "dropdown-extra"
          ),
          width = 180
        ),
        Test = colDef(
          name = "Test",
          cell = dropdown_extra(
            id = ns("edit_Test"),
            choices = c(ratio_reference_options(), "(all other levels)"),
            class = "dropdown-extra"
          ),
          width = 180
        ),
        AggregateSubject = colDef(
          name = "Aggregate Subject?",
          cell = dropdown_extra(
            id = ns("edit_AggregateSubject"),
            choices = c("yes", "no", "if-needed"),
            class = "dropdown-extra"
          ),
          width = 120
        ),
        AdjustingFactor = colDef(
          name = "Adj. Factor",
          cell = text_extra(
            id = ns("edit_AdjustingFactor")
          ),
          width = 120
        ),
        PPTESTCD = colDef(
          name = "PPTESTCD",
          cell = text_extra(
            id = ns("edit_PPTESTCD")
          ),
          width = 140
        )
      )
      reactable(
        data = ratio_table(),
        defaultColDef = colDef(align = "center"),
        columns = col_defs,
        selection = "multiple",
        defaultExpanded = TRUE,
        borderless = TRUE,
        theme = reactableTheme(
          rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
        )
      )
    }) %>%
      shiny::bindEvent(refresh_reactable())

    # Update table on edit
    observe({
      purrr::walk(table_columns, function(colname) {
        observeEvent(input[[paste0("edit_", colname)]], {
          edit <- input[[paste0("edit_", colname)]]
          tbl <- ratio_table()
          tbl[edit$row, edit$column] <- edit$value

          # If Parameter or Reference changed, update PPTESTCD for that row
          if (colname %in% c("Parameter", "Reference")) {
            analyte_col <- adnca_data()$conc$columns$groups$group_analyte
            profile_col <- "NCA_PROFILE"
            pcspec_col <- "PCSPEC"
            raw_route_col <- "ROUTE"
            duration_col <- adnca_data()$dose$columns$duration
            route_col <- adnca_data()$dose$columns$route
            ref <- tbl[edit$row, "Reference"]
            param <- tbl[edit$row, "Parameter"]
            tbl <- tbl %>% mutate(
              PPTESTCD = case_when(
                startsWith(Reference, analyte_col) ~ paste0("MR", Parameter),
                startsWith(Reference, profile_col) ~ paste0("AR", Parameter),
                startsWith(
                  toupper(Reference),
                  paste0(toupper(route_col), ": INTRAVASCULAR")
                ) ~ "FABS",
                startsWith(toupper(Reference), paste0(toupper(raw_route_col), ": INTRA")) ~ "FABS",
                startsWith(Reference, paste0(route_col)) ~ "FREL",
                startsWith(Reference, raw_route_col) ~ "FREL",
                TRUE ~ paste0("RA", Parameter)
              ),
              PPTESTCD = make.unique(PPTESTCD, sep = "")
            )
          }
          ratio_table(tbl)
          refresh_reactable(refresh_reactable() + 1)
        })
      })
      # ReferenceValue per row
      observe({
        nrows <- nrow(ratio_table())
        if (nrows > 0) {
          for (i in seq_len(nrows)) {
            observeEvent(input[[paste0("edit_Reference_", i)]], {
              edit <- input[[paste0("edit_Reference_", i)]]
              tbl <- ratio_table()
              tbl[edit$row, edit$column] <- edit$value
              ratio_table(tbl)
            }, ignoreInit = TRUE)
          }
        }
      })

      # Add special names for certain ratios
      observe({
        analyte_col <- adnca_data()$conc$columns$groups$group_analyte
        profile_col <- "NCA_PROFILE"
        pcspec_col <- "PCSPEC"
        raw_route_col <- "ROUTE"
        route_col <- adnca_data()$dose$columns$route

        tbl <- ratio_table() %>% mutate(
          PPTESTCD = case_when(
            startsWith(Reference, analyte_col) ~ paste0("MR", Parameter),
            startsWith(Reference, profile_col) ~ paste0("AR", Parameter),
            startsWith(toupper(Reference), paste0(toupper(route_col), ": INTRAVASCULAR")) ~ "FABS",
            startsWith(toupper(Reference), paste0(toupper(raw_route_col), ": INTRA")) ~ "FABS",
            startsWith(Reference, paste0(route_col)) ~ "FREL",
            startsWith(Reference, raw_route_col) ~ "FREL",
            TRUE ~ paste0("RA", Parameter)
          ),
          PPTESTCD = make.unique(PPTESTCD, sep = "")
        )

        ratio_table(tbl)
      })
    })

    # Return the table as a reactive
    return(ratio_table)
  })
}
