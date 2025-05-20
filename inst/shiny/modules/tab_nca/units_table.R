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
      disabled = TRUE
    )
  )
}

units_table_server <- function(id, mydata) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #' Allow user to open the units table when data is available
    observeEvent(mydata(), {
      updateActionButton(
        session = session,
        inputId = "open_units_table",
        disabled = FALSE
      )
    })

    # Define the modal message displayed with the parameter units table #
    modal_units_table <- reactiveVal(NULL)
    observeEvent(input$open_units_table, {
      
      # Make a reactive variable from the units table
      modal_units_table(mydata()$units)

      # Keep in a variable all analytes available
      group_cols_for_units <- mydata()$units %>%
        select(-any_of(c("PPTESTCD", "PPORRESU", "PPSTRESU", "conversion_factor"))) %>%
        colnames()
      groups_for_units <- mydata()$intervals %>%
        select(any_of(group_cols_for_units)) %>%
        unique()
      
      groups_as_filter_ops <- apply(groups_for_units, 1, function(x) {
        paste0(names(x), " == '", x, "'") %>%
          paste(collapse = " | ") %>%
          paste0("(", ., ")")
      })
      names(groups_as_filter_ops) <- apply(groups_for_units, 1, function(x) {
        paste0(names(x), ": ", x) %>%
          paste(collapse = ", ")
      })
      

      groups_for_units_input <- lapply(
        colnames(groups_for_units),
        function(varname) {
          vals <- unique(groups_for_units[[varname]])
          setNames(
            paste0(varname, " == '", vals, "'"),
            vals
          )
        }
      )
      names(groups_for_units_input) <- colnames(groups_for_units)

      # Show the modal message with the units table and an analyte selector
      showModal(modalDialog(
        title = tagList(
          span("Units of NCA parameter results")
        ),
        DTOutput(ns("modal_units_table")),
        footer = tagList(
          modalButton("Close"),
          actionButton(ns("save_units_table"), "Save Units Table")
        ),
        size = "l"
      ))
    })

    # Define the parameter units table and how is displayed to the user #
    modal_units_table <- reactiveVal(NULL)
    observeEvent(mydata()$units, {
      mydata()$units
    })

    # Define which parameters where choosen by the user
    params_to_calculate <- reactive({
      names(purrr::keep(mydata()$intervals, ~ is.logical(.x) && any(.x))) %>%
        translate_terms("PKNCA", "PPTESTCD")
    })

    params_to_calculate_array_str <- reactive({
      paste0("['", paste(params_to_calculate(), collapse = "','"), "']")
    })
    
    group_cols_for_units <- reactive({
      mydata()$units %>%
        select(-any_of(c("PPTESTCD", "PPORRESU", "PPSTRESU", "conversion_factor"))) %>%
        colnames()
    })

    #' Rendering the modal units table
    output$modal_units_table <- DT::renderDT({
      req(modal_units_table())

      datatable(
        data = modal_units_table() %>%
          rename(
            `Parameter` = PPTESTCD,
            `Default unit` = PPORRESU,
            `Conversion Factor` = conversion_factor,
            `Custom unit` = PPSTRESU
          ) %>%
          mutate(Parameter = translate_terms(Parameter, "PKNCA", "PPTESTCD"),
                 across(where(is.character), as.factor)
          ),
        escape = FALSE,
        filter = "top",
        selection = list(mode = "single", target = "cell"),
        class = "table table-striped table-bordered",
        rownames = FALSE,
        editable = list(
          target = "cell",
          disable = list(
            columns = c(0, 1, 2)
          )
        ),
        options = list(
          order = list(2, "desc"),
          paging = FALSE,
          searching = TRUE,
          autoWidth = TRUE,
          dom = "ft",
          # Display only rows with the parameters to run for the NCA
          ### TODO: Not working
          # rowCallback = htmlwidgets::JS(
          #   paste0(
          #     "
          #     function(row, data, index) {
          #     var paramsToCalculate = ", params_to_calculate_array_str(),
          #     ";
          #     if (paramsToCalculate.indexOf(data[1]) === -1) {
          #     $(row).hide();
          #     }
          #     }"
          #   )
          # ),
          columnDefs = list(
            list(
              searchable = FALSE,
              targets = which(
                c("PPORRES", "PPSTRES") %in% names(mydata()$units)
              )
            )
          )
        )
      )
    })

    # Accept user modifications in the modal units table
    observeEvent(input$modal_units_table_cell_edit, {
      browser()
      info <- input$modal_units_table_cell_edit
      modal_units_table <- modal_units_table()
      col_conv_factor <- which(names(modal_units_table) == "conversion_factor")
      col_default_unit <- which(names(modal_units_table) == "PPORRES")
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

        modal_units_table[rows_to_change, "Conversion Factor"] <- conversion_factor_value
      }

      # Update the server table
      modal_units_table(modal_units_table)
    })

    # When save button is pressed substitute the original units table based on the modal one
    observeEvent(input$save_units_table, {
      # Make sure there are no missing entries (no NA in conversion factor)
      if (any(is.na(modal_units_table()$`Conversion Factor`))) {
        invalid_entries <- modal_units_table() %>%
          filter(is.na(`Conversion Factor`)) %>%
          mutate(entry = paste0(Parameter, " (", Analytes, ")")) %>%
          pull(entry)

        showNotification(
          paste0(
            "Please, make sure to use only recognised convertible units in `Custom Unit` ",
            "(i.e, day, hr, min, sec, g/L).",
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
        rename(
          PARAM = `Analytes`,
          PPTESTCD = `Parameter`,
          PPORRESU = `Default unit`,
          PPSTRESU = `Custom unit`,
          conversion_factor = `Conversion Factor`
        ) %>%
        session$userData$units_table()

      # Close the modal message window for the user
      removeModal()
    })

    #' Update local `modal_units_table()` is the global value changes.
    observeEvent(session$userData$units_table(), {
      session$userData$units_table() %>%
        rename(
          `Analytes` = PARAM,
          `Parameter` = PPTESTCD,
          `Default unit` = PPORRESU,
          `Custom unit` = PPSTRESU,
          `Conversion Factor` = conversion_factor
        ) %>%
        modal_units_table()
    })
  })
}

# Create a function to provide a clean display of the modal units table
.clean_display_units_table <- function(modal_units_table, sel_filter_operations) {

  modal_units_table #%>%
    # mutate(`Conversion Factor` = signif(`Conversion Factor`, 3)) %>%
    # filter(`Analytes` %in% selected_analytes) %>%
    # group_by(Parameter, `Default unit`, `Conversion Factor`, `Custom unit`) %>%
    # mutate(Analytes = paste(Analytes, collapse = ", ")) %>%
    # ungroup() %>%
    # unique()
}

#' Check if units table already exists.
#' If it does, check if parameters and their default units are the same as pulled
#' from the data. If they are, there is no need to update the table and we wish to keep
#' the previously established units.
#' If the tables differ in content (e.g. when new data is uploaded), then overwrite existing
#' units table.
#' @param current Tibble with current units table, or NULL if non-existant.
#' @param new      Tibble with new units table to replace the current one.
#' @returns Boolean, TRUE if current table is still valid, FALSE if it is not.
.validate_current_table <- function(current, new) {
  !is.null(current) &&
    all(sort(current$`Parameter`) == sort(new$`Parameter`)) &&
    all(sort(current$`Default unit`) == sort(new$`Default unit`))
}
