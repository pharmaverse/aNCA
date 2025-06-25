#' TODO: add proper documentation
#'
#' @details
#' Module requires session-wide object for storing units table: `session$userData$units_table`.
#' All copies of the module will modify this single source of truth. This is so that the units
#' selection is respected across the whole application, regardless of where the user decides
#' to set the units.
#'
units_table_ui <- function(id) {
  ns <- NS(id)
  # Button to open a module message with the parameter units table #
  tagList(
    actionButton(
      ns("open_units_table"),
      icon = icon("scale-balanced"),
      label = "Parameter Units",
      disabled = FALSE
    )
  )
}

units_table_server <- function(id, mydata) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Define the modal message displayed with the parameter units table #
    modal_units_table <- reactiveVal(NULL)
    observeEvent(input$open_units_table, {

      # Make a reactive variable from the units table
      if (!is.null(session$userData$units_table())) {
        modal_units_table(session$userData$units_table())
      } else {
        # If the user has not set any custom units table, use the default one
        modal_units_table(mydata()$units)
      }

      # Show the modal message with the units table and an analyte selector
      showModal(modalDialog(
        title = tagList(
          span("Units of NCA parameter results")
        ),
        tagList(
          modalButton("Close"),
          actionButton(ns("save_units_table"), "Save Units Table")
        ),
        DTOutput(ns("modal_units_table")),
        footer = tagList(
          modalButton("Close"),
          actionButton(ns("save_units_table"), "Save Units Table")
        ),
        size = "xl"
      ))
    })

    # Define rows from units table not of interest for the user
    rows_to_hide_units_table <- reactive({

      group_cols <- intersect(
        names(PKNCA::getGroups(mydata()$conc)), names(mydata()$units)
      )
      groups_to_keep <- select(mydata()$intervals, any_of(group_cols))
      params_to_keep <- names(purrr::keep(mydata()$intervals, ~ is.logical(.x) && any(.x)))

      rows_to_keep <- mydata()$units %>%
        mutate(nrow = row_number()) %>%
        filter(PPTESTCD %in% params_to_keep)
      if (ncol(groups_to_keep) > 0) {
        rows_to_keep <- inner_join(
          rows_to_keep, unique(groups_to_keep),
          by = intersect(names(rows_to_keep), names(groups_to_keep))
        )
      }

      rows_to_hide <- setdiff(seq_len(nrow(mydata()$units)), rows_to_keep$nrow)
      paste0("[", paste(rows_to_hide, collapse = ", "), "]")
    })

    #' Rendering the modal units table
    output$modal_units_table <- DT::renderDT({
      req(modal_units_table())
      req(rows_to_hide_units_table())

      datatable(
        data = modal_units_table() %>%
          mutate(
            PPTESTCD = translate_terms(PPTESTCD, "PKNCA", "PPTEST"),
            across(where(is.character), as.factor),
            nrow = row_number()
          ),
        class = "cell-border compact striped",
        escape = FALSE,
        filter = "top",
        selection = list(mode = "single", target = "cell"),
        colnames = c(
          "Parameter" = "PPTESTCD",
          "Default unit" = "PPORRESU",
          "Conversion Factor" = "conversion_factor",
          "Custom unit" = "PPSTRESU"
        ),
        rownames = FALSE,
        editable = list(
          target = "cell",
          disable = list(
            columns = which(
              !names(modal_units_table()) %in% c("PPSTRESU", "conversion_factor")
            ) - 1
          )
        ),
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          searching = TRUE,
          autoWidth = TRUE,
          dom = "t",
          # Display only rows with the parameters to run for the NCA
          rowCallback = htmlwidgets::JS(
            paste0(
              "
              function(row, data, index) {
                var rowsToHide =", rows_to_hide_units_table(),
              ";
                if (rowsToHide.includes(data[", ncol(modal_units_table()), "])) {
                  $(row).hide();
                }
              }
              "
            )
          ),
          columnDefs = list(
            list(
              visible = FALSE,
              targets = ncol(modal_units_table())
            ),
            list(className = "dt-center", targets = "_all")
          )
        )
      ) %>%
        # Format the conversion factor column with scientific notation
        DT::formatSignif(columns = c("Conversion Factor"), digits = 2)
    })

    # Accept user modifications in the modal units table
    observeEvent(input$modal_units_table_cell_edit, {

      info <- input$modal_units_table_cell_edit
      modal_units_table <- modal_units_table()
      col_conv_factor <- which(names(modal_units_table) == "conversion_factor")
      col_custom_unit <- which(names(modal_units_table) == "PPSTRESU")

      # If the edited cell is in the 'Conversion Factor' only accept numeric values
      if ((info$col + 1) == col_conv_factor && !is.numeric(info$value)) {
        # Report the user the expected numeric format
        showNotification(
          "Please enter a valid numeric value for the Conversion Factor.",
          type = "error",
          duration = 5
        )

        # Elude further actions
        return()
      }

      # Make the edition in the units table
      modal_units_table[info$row, info$col + 1] <- info$value

      # If the custom unit was changed recalculate the conversion factor
      if ((info$col + 1) == col_custom_unit) {
        def_unit <- modal_units_table[info$row, "PPORRESU"]
        cust_unit <- modal_units_table[info$row, "PPSTRESU"]
        conversion_factor_value <- get_conversion_factor(def_unit, cust_unit)

        # If the modification lead to an unexpected conversion factor notify the user
        if (any(is.na(conversion_factor_value))) {
          showNotification(
            paste0(
              "Unrecognised conversion: ", def_unit, " > ", cust_unit,
              ". Either make sure both units are defined in the UNIDATA
              library or impute a conversion factor yourself"
            ),
            type = "error",
            duration = 12.5
          )
        }

        modal_units_table[info$row, "conversion_factor"] <- conversion_factor_value
      }

      # Update the server table
      modal_units_table(modal_units_table)
    })

    # When save button is pressed substitute the original units table based on the modal one
    observeEvent(input$save_units_table, {
      # Make sure there are no missing entries (no NA in conversion factor)
      if (any(is.na(modal_units_table()$conversion_factor))) {
        id_cols <- setdiff(names(mydata()$units),
                           c("PPTESTCD", "PPORRESU", "PPSTRESU", "conversion_factor"))
        invalid_entries <- modal_units_table() %>%
          filter(is.na(`conversion_factor`)) %>%
          mutate(
            entry = paste0(PPTESTCD, " (", paste(!!!syms(id_cols), sep = ", "), ")")
          ) %>%
          pull(entry)

        showNotification(
          paste0(
            "Please, make sure to use only recognised convertible units in `Custom Unit` ",
            "(i.e, day, h, min, sec, g/L).",
            " If not, introduce yourself the corresponding `Conversion Factor` value in: ",
            paste(invalid_entries, collapse = ", ")
          ),
          duration = NULL,
          closeButton = TRUE,
          type = "warning"
        )
        return()
      }

      log_trace("Applying custom units specification.")
      modal_units_table() %>%
        session$userData$units_table()

      # Close the modal message window for the user
      removeModal()
    })

    #' Update local `modal_units_table()` is the global value changes.
    observeEvent(session$userData$units_table(), {
      session$userData$units_table() %>%
        modal_units_table()
    })
  })
}
