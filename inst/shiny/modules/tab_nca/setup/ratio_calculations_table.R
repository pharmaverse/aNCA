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
    id, adnca_data, nca_params, select_nca_profiles, select_analytes, select_pcspec
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper: get group vars and group values
    group_vars <- reactive({
      c(
        adnca_data()$conc$columns$groups$group_analyte,
        dplyr::group_vars(adnca_data()$conc),
        dplyr::group_vars(adnca_data()$dose),
        adnca_data()$dose$columns$route
      ) |>
        unique()
    })

    group_values <- reactive({
      nca_profile_col <- "NCA_PROFILE"
      analyte_col <- adnca_data()$conc$columns$groups$group_analyte
      pcspec_col <- "PCSPEC"

      left_join(adnca_data()$conc$data %>%
                  select(any_of(c(group_vars(), "NCA_PROFILE"))) %>%
                  unique(),
                adnca_data()$dose$data %>%
                  select(any_of(group_vars())) %>%
                  unique(),
                by = c(intersect(names(adnca_data()$conc$data), group_vars()))
      ) %>%
        filter(
          .[[nca_profile_col]] %in% select_nca_profiles,
          .[[analyte_col]] %in% select_analytes,
          .[[pcspec_col]] %in% select_pcspec
        )
    })

    ratio_params <- reactive({
      # Get all nca_params() starting with auc or with cmax
      nca_params() |>
        purrr::keep(~ grepl("^(auc[li\\.]|cmax)", ., ignore.case = TRUE)) |>
        unique()
    })

    # Table columns
    table_columns <- c("Parameter", "ContrastVar", "ReferenceValue", "AggregateSubject", "AdjustingFactor")

    # Store table data
    ratio_table <- reactiveVal({
      data.frame(
        Parameter = character(),
        ContrastVar = character(),
        ReferenceValue = character(),
        AggregateSubject = character(),
        AdjustingFactor = numeric(),
        stringsAsFactors = FALSE
      )
    })

    # Add row
    observeEvent(input$add_row, {

      if (length(nca_params()) == 0 || nrow(group_values()) == 0) {
        showNotification("No parameters or group variables available to add a row.", type = "error")
        return()
      }
      new_row <- data.frame(
        Parameter = ratio_params()[1],
        ContrastVar = group_vars()[1],
        ReferenceValue = group_values()[[group_vars()[1]]][1],
        AggregateSubject = "no",
        AdjustingFactor = 1,
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

      # Column definitions
      col_defs <- list(
        Parameter = colDef(
          cell = dropdown_extra(
            id = ns("edit_Parameter"),
            choices = ratio_params(),
            class = "dropdown-extra"
          ),
          width = 180
        ),
        ContrastVar = colDef(
          cell = dropdown_extra(
            id = ns("edit_ContrastVar"),
            choices = group_vars(),
            class = "dropdown-extra"
          ),
          width = 180
        ),
        ReferenceValue = colDef(
          cell = dropdown_extra(
              id = ns(paste0("edit_ReferenceValue")),
              choices = group_values()[[group_vars()[1]]],,
              class = "dropdown-extra"
            ),
          width = 180
        ),
        AggregateSubject = colDef(
          cell = dropdown_extra(
            id = ns("edit_AggregateSubject"),
            choices = c("yes", "no", "if-needed"),
            class = "dropdown-extra"
          ),
          width = 120
        ),
        AdjustingFactor = colDef(
          cell = text_extra(
            id = ns("edit_AdjustingFactor")
          ),
          width = 120
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
          # If ContrastVar changed, reset ReferenceValue for that row
          if (colname == "ContrastVar") {
            tbl[edit$row, "ReferenceValue"] <- ""
          }
          ratio_table(tbl)
        })
      })
      # ReferenceValue per row
      observe({
        nrows <- nrow(ratio_table())
        if (nrows > 0) {
          for (i in seq_len(nrows)) {
            observeEvent(input[[paste0("edit_ReferenceValue_", i)]], {
              edit <- input[[paste0("edit_ReferenceValue_", i)]]
              tbl <- ratio_table()
              tbl[edit$row, edit$column] <- edit$value
              ratio_table(tbl)
            }, ignoreInit = TRUE)
          }
        }
      })
    })

    # Return the table as a reactive
    return(list(
      ratio_table = ratio_table,
      refresh_reactable = refresh_reactable
    ))
  })
}
