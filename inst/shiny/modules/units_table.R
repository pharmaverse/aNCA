# UI function for the units table module
units_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      ns("open_units_table"),
      icon = shiny::icon("scale-balanced"),
      label = "Parameter Units",
      disabled = TRUE
    )
  )
}

# Server function for the units table module
units_table_server <- function(id, mydata, res_nca = reactive(NULL), params_to_calculate) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Enable the units table button when data is available
    observeEvent(mydata(),{
      updateActionButton(session = session, 
                         inputId = "open_units_table",
                         disabled = FALSE)
    })
    
    # Open modal message when units table button is pressed
    observeEvent(input$open_units_table, {
      showModal(modalDialog(
        title = tagList(
          span("Units of NCA parameter results"),
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-label` = "Close",
            span(`aria-hidden` = "true", HTML("&times;"))
          )
        ),
        selectInput(
          inputId = ns("select_unitstable_analyte"),
          multiple = TRUE,
          label = "Select Analyte:",
          choices = mydata()$conc$data[[mydata()$conc$columns$groups$group_analyte]] %>% unique(),
          selected = mydata()$conc$data[[mydata()$conc$columns$groups$group_analyte]] %>% unique()
        ),
        DTOutput(ns("modal_units_table")),
        footer = tagList(
          modalButton("Close"),
          actionButton(ns("save_units_table"), "Save Units Table")
        ),
        size = "l"
      ))
    })
    
    # Reformat how the units table is displayed to the user
    modal_units_table <- reactiveVal(NULL)
    observeEvent(list(mydata(), input$select_unitstable_analyte), {
      req(mydata())
      req(input$select_unitstable_analyte)
      modal_units_table_data <- mydata()$units %>%
        dplyr::group_by(PPTESTCD, PPORRESU, PPSTRESU, conversion_factor) %>%
        dplyr::filter(!!sym(mydata()$conc$columns$groups$group_analyte) == input$select_unitstable_analyte) %>%
        dplyr::rename(`Parameter` = PPTESTCD,
                      `Default unit` = PPORRESU,
                      `Conversion Factor` = conversion_factor,
                      `Custom unit` = PPSTRESU) %>%
        dplyr::mutate(Analytes = paste(!!sym(mydata()$conc$columns$groups$group_analyte), collapse = ", ")) %>%
        dplyr::ungroup() %>%
        dplyr::select(`Analytes`, `Parameter`, `Default unit`, `Custom unit`, `Conversion Factor`)
      modal_units_table(modal_units_table_data)
    })
    
    # Define which parameters where choosen by the user
    params_to_calculate <- reactiveVal(NULL)
    observeEvent(mydata()$intervals, {
      params_to_calculate(names(purrr::keep(mydata()$intervals, 
                                            ~ is.logical(.x) && any(.x)))
                          )
    })

    # Render the modal units table to the user
    output$modal_units_table <- DT::renderDT({
      datatable(
        data = modal_units_table() %>%
          dplyr::mutate(`Conversion Factor` = signif(`Conversion Factor`, 3)),
        escape = FALSE,
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
          rowCallback = JS(
            paste0(
              "function(row, data, index) {",
              "  var paramsToCalculate = ", paste0("['", paste(params_to_calculate(), collapse = "','"), "']"), ";",
              "  if (paramsToCalculate.indexOf(data[1]) === -1) {",
              "    $(row).hide();",
              "  }",
              "}"
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
      modal_units_table[info$row, info$col + 1] <- info$value

      if (names(modal_units_table)[info$col + 1] == "Custom unit") {
        default_unit <- modal_units_table[info$row, "Default unit"]
        custom_unit <- modal_units_table[info$row, "Custom unit"]
        modal_units_table[info$row, "Conversion Factor"] <- transform_unit(default_unit, custom_unit)
      }

      modal_units_table(modal_units_table)
    })

    # When save button is pressed substitute the original units table based on the modal one
    new_res_nca <- reactiveVal(NULL)
    observeEvent(input$save_units_table, {

      # Make sure there are no missing entries (no NA in conversion factor)
      if (any(is.na(modal_units_table()$`Conversion Factor`))) {

        invalid_entries <- modal_units_table() %>%
          filter(is.na(`Conversion Factor`)) %>%
          mutate(entry = paste0(Parameter, " (", Analytes, ")")) %>%
          pull(entry)

        showNotification(
          paste0("Please, make sure to use only recognised convertible units in `Custom Unit` (i.e, day, hr, min, sec, g/L).",
                 " If not, introduce yourself the corresponding `Conversion Factor` value in: ",
                 paste(invalid_entries, collapse = ", ")),
          duration = NULL,
          closeButton = TRUE,
          type = "warning"
        )
        return()
      }

      # Tranform the modal units table back to the original one
      modal_units_table <- modal_units_table() %>%
        dplyr::mutate(Analytes = strsplit(Analytes, ", ")) %>%
        tidyr::unnest(Analytes) %>%
        dplyr::rename(ANALYTE = `Analytes`,
                      PPTESTCD = `Parameter`,
                      PPORRESU = `Default unit`,
                      PPSTRESU = `Custom unit`,
                      conversion_factor = `Conversion Factor`)

      # Save the modified units table in my data object
      mydata <- mydata()
      mydata$units <- modal_units_table
      mydata(mydata)
      
      # If there are already results produced, make sure they are also adapted
      if (!is.null(res_nca())) {
        res_nca <- res_nca()
        res_nca$data$units <- modal_units_table
        res_nca$result <- res_nca$result %>%
          dplyr::select(-PPSTRESU, -PPSTRES) %>%
          dplyr::left_join(
            modal_units_table,
            by=intersect(names(.), names(modal_units_table))
          ) %>%
          dplyr::mutate(PPSTRES = PPORRES * conversion_factor) %>% 
          dplyr::select(-conversion_factor)
 
        new_res_nca(res_nca)
      }
      # Close the module window once all saving actions are done
      removeModal()

    })
    return(reactive({new_res_nca()}))
  })
}