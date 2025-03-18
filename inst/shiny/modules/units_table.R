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

units_table_server <- function(id, mydata, res_nca = reactiveVal(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #' Allow user to open the units table when data is available
    observeEvent(mydata(), {
      updateActionButton(session = session,
                         inputId = "open_units_table",
                         disabled = FALSE)
    })

    # Define the modal message displayed with the parameter units table #
    observeEvent(input$open_units_table, {

      # Keep in a variable all analytes available
      analyte_column <- mydata()$conc$columns$groups$group_analyte
      analyte_choices <- unique(mydata()$units[[analyte_column]])

      # Show the modal message with the units table and an analyte selector
      showModal(modalDialog(
        title = tagList(
          span("Units of NCA parameter results")
        ),
        selectInput(
          inputId = ns("select_unitstable_analyte"),
          multiple = TRUE,
          label = "Select Analytes to modify:",
          choices = analyte_choices,
          selected = analyte_choices
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
    observeEvent(mydata(), {
      req(mydata()$units)
      analyte_column <- mydata()$conc$columns$groups$group_analyte

      modal_units_table_data <- mydata()$units %>%
        rename(
          `Parameter` = PPTESTCD,
          `Default unit` = PPORRESU,
          `Conversion Factor` = conversion_factor,
          `Custom unit` = PPSTRESU,
          `Analytes` = analyte_column
        ) %>%
        select(`Analytes`, `Parameter`, `Default unit`, `Custom unit`, `Conversion Factor`)

      modal_units_table(modal_units_table_data)
    })

    # Define which parameters where choosen by the user
    params_to_calculate <- reactive({
      names(purrr::keep(mydata()$intervals, ~ is.logical(.x) && any(.x)))
    })

    params_to_calculate_array_str <- reactive({
      paste0("['", paste(params_to_calculate(), collapse = "','"), "']")
    })

    #' Rendering the modal units table
    output$modal_units_table <- DT::renderDT({
      datatable(
        data = .clean_display_units_table(modal_units_table(),
                                          input$select_unitstable_analyte),
        escape = FALSE,
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
          rowCallback = htmlwidgets::JS(
            paste0("
              function(row, data, index) {
              var paramsToCalculate = ", params_to_calculate_array_str(),
              ";
              if (paramsToCalculate.indexOf(data[1]) === -1) {
              $(row).hide();
              }
              }"
            )
          ),
          columnDefs = list(
            list(
              visible = FALSE,
              targets = c()
            )
          )
        )
      )
    })

    # Accept user modifications in the modal units table
    observeEvent(input$modal_units_table_cell_edit, {

      info <- input$modal_units_table_cell_edit
      modal_units_table <- modal_units_table()

      analytes <- input$select_unitstable_analyte
      param <- .clean_display_units_table(modal_units_table,
                                          input$select_unitstable_analyte) %>%
        slice(info$row) %>%
        pull(Parameter)
      rows_to_change <- with(modal_units_table,
                             which(Analytes %in% analytes & Parameter %in% param))
      col_to_change <- names(modal_units_table)[info$col + 1]

      # If the edited cell is in the 'Conversion Factor' only accept numeric values
      if (col_to_change == "Conversion Factor" && !is.numeric(info$value)) {

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
      modal_units_table[rows_to_change, col_to_change] <- info$value

      # If the custom unit was changed recalculate the conversion factor
      if (col_to_change == "Custom unit") {
        def_unit <- modal_units_table[rows_to_change, ][["Default unit"]]
        cust_unit <- modal_units_table[rows_to_change, ][["Custom unit"]]
        conversion_factor_value <- get_conversion_factor(def_unit, cust_unit)

        # If the modification lead to an unexpected conversion factor notify the user
        if (any(is.na(conversion_factor_value))) {
          showNotification(
            paste0(
              "Unrecognised conversion: ",  def_unit, " > ", cust_unit,
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
          paste0("Please, make sure to use only recognised convertible units in `Custom Unit` ",
                 "(i.e, day, hr, min, sec, g/L).",
                 " If not, introduce yourself the corresponding `Conversion Factor` value in: ",
                 paste(invalid_entries, collapse = ", ")),
          duration = NULL,
          closeButton = TRUE,
          type = "warning"
        )
        return()
      }

      # Tranforms the modal units table back to the original one
      analyte_column <- mydata()$conc$columns$groups$group_analyte
      modal_units_table <- modal_units_table() %>%
        rename(PARAM = `Analytes`,
               PPTESTCD = `Parameter`,
               PPORRESU = `Default unit`,
               PPSTRESU = `Custom unit`,
               conversion_factor = `Conversion Factor`)

      # Close the modal message window for the user
      removeModal()

      # Updates units table of mydata and res_nca according to the user's changes
      mydata <- mydata()
      mydata$units <- modal_units_table
      mydata(mydata)

      # If there are already results produced, make sure they are also adapted
      if (!is.null(res_nca())) {
        res_nca <- res_nca()
        res_nca$data$units <- modal_units_table
        res_nca$result <- res_nca$result %>%
          select(-PPSTRESU, -PPSTRES) %>%
          left_join(
            modal_units_table,
            by = intersect(names(.), names(modal_units_table))
          ) %>%
          mutate(PPSTRES = PPORRES * conversion_factor) %>%
          select(-conversion_factor)
        res_nca(res_nca)

      }

    })

  })
}

# Create a function to provide a clean display of the modal units table
.clean_display_units_table <- function(modal_units_table, selected_analytes) {
  modal_units_table %>%
    mutate(`Conversion Factor` = signif(`Conversion Factor`, 3)) %>%
    filter(`Analytes` %in% selected_analytes) %>%
    group_by(Parameter, `Default unit`, `Conversion Factor`, `Custom unit`) %>%
    mutate(Analytes = paste(Analytes, collapse = ", ")) %>%
    ungroup() %>%
    unique()
}