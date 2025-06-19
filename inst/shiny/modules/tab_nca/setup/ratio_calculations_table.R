# ratio_calculations_table.R
# Module for dynamic ratio calculation table in Shiny

ratio_calculations_table_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    div(
      class = "plot-widget-group",
      actionButton(ns("add_row"), "+ Add Ratio Row", class = "btn-success")
    ),
    div(
      class = "plot-widget-group",
      actionButton(ns("remove_row"), "- Remove selected rows", class = "btn-warning")
    ),
    fluidRow(
      reactableOutput(ns("ratio_calculations"))
    )
  )
}

ratio_calculations_table_server <- function(
    id, adnca_data
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper: get group vars and group values
    ratio_groups <- reactive({
      adnca_data()$intervals %>%
        # Only consider main intervals for ratios
        dplyr::filter(type_interval == "main") %>%
        dplyr::select(
          -any_of(c(names(PKNCA::get.interval.cols()), "impute", "type_interval"))
        ) %>%
        # Join route information (bioavailability ratios)
        left_join(
          dplyr::select(
            adnca_data()$dose$data,
            any_of(c(dplyr::group_vars(adnca_data()$dose), adnca_data()$dose$columns$route))
          ) %>%
            unique(),
          by = dplyr::group_vars(adnca_data()$dose)
        ) %>%
        dplyr::select(any_of(
          c(dplyr::group_vars(adnca_data()$conc),
            adnca_data()$dose$columns$route,
            "NCA_PROFILE"))
        ) %>%
        # Filter out the columns with one one unique value (no ratio possible!)
        dplyr::select(dplyr::where(~ length(unique(.)) > 1)) %>%
        dplyr::select(-any_of(adnca_data()$conc$columns$subject))
    })

    ratio_reference_options <- reactive({
      # We paste the column name and value to use as a specified input
      if (ncol(ratio_groups()) > 0) {
        ratio_groups() %>%
          # Convert all columns to character
          dplyr::mutate(across(everything(), as.character)) %>%
          pivot_longer(cols = everything()) %>%
          mutate(input_name = paste0(name, ": ", value)) %>%
          pull(input_name) %>%
          unique() %>%
          sort()
      } else NULL
    })

    ratio_param_options <- reactive({
      adnca_data()$intervals %>%
        # Only consider main intervals for ratios
        dplyr::filter(type_interval == "main") %>%
        dplyr::select(
          any_of(setdiff(names(PKNCA::get.interval.cols()), c("start", "end")))
        ) %>%
        # For logical columns transform all FALSE to NA
        dplyr::mutate(across(where(is.logical), ~ ifelse(. == FALSE, NA, .))) %>%
        # Select only columns where all values are not NA
        dplyr::select(dplyr::where(~ !all(is.na(.)))) %>%
        names() %>%
        purrr::keep(~ grepl("^(auc[it\\.]|cmax)", ., ignore.case = TRUE)) %>%
        translate_terms("PKNCA", "PPTESTCD")
    })

    # Table columns
    table_columns <- c("Parameter", "Reference", "Numerator", "AggregateSubject", "AdjustingFactor")

    # Store table data
    ratio_table <- reactiveVal({
      data.frame(
        Parameter = character(),
        Reference = character(),
        Numerator = character(),
        AggregateSubject = character(),
        AdjustingFactor = numeric(),
        stringsAsFactors = FALSE
      )
    })

    # Add row
    observeEvent(input$add_row, {

      if (length(ratio_param_options()) == 0 || length(ratio_reference_options()) == 0) {
        showNotification("No parameters or group variables available to add a row.", type = "error")
        return()
      }
      new_row <- data.frame(
        Parameter = ratio_param_options()[1],
        Reference = ratio_reference_options()[1],
        Numerator = "(all)",
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
        Numerator = colDef(
          name = "Numerator",
          cell = dropdown_extra(
              id = ns("edit_Numerator"),
              choices = c(ratio_reference_options(), "(all)"),
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
        )
      )
      reactable(
        data = dplyr::select(ratio_table(), -PPTESTCD),
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
          ratio_table(tbl)
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
        route_col <- adnca_data()$dose$columns$route

        tbl <- ratio_table() %>% mutate(
          PPTESTCD = case_when(
            startsWith(Reference, analyte_col) ~ paste0("MR", Parameter),
            startsWith(Reference, profile_col) ~ paste0("AR", Parameter),
            startsWith(Reference, pcspec_col) ~ paste0("PR", Parameter),
            startsWith(Reference, paste0(route_col, ": intravascular")) ~ "FABS",
            TRUE ~ paste0("RA", Parameter)
          )
        )

        ratio_table(tbl)
      })
    })

    # Return the table as a reactive
    return(list(
      ratio_table = ratio_table
    ))
  })
}
