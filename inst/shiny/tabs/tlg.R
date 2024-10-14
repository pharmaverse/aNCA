# SERVER LOGIC OF NAVBAR TLG TAB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Order details: Input definition for Tables, Listings and Graphs to generate in the other tabs
# Tables:
# Listings:
# Graphs:

# TABSET: Order details =============================================

# Make available the CSV file with the TLG list
tlg_order <- reactiveVal(
  read.csv("www/data/TLG_order_details.csv")
)

# Render the TLG list for the user's inspection
output$tlg_order <- DT::renderDT({

  # Load reactive value and include link to the TLG reference
  tlg_order_data <- tlg_order() %>%
    mutate(PKid = paste0("<a href='", Catalog_Link, "'>", PKid, "</a>"))

  # Render the editable DT table
  datatable(
    data = tlg_order_data,
    editable = TRUE,
    rownames = TRUE,
    escape = FALSE,

    # Server/Client processing of selected rows
    selection = list(
      mode = "multiple",
      selected = which(as.logical(tlg_order_data$Selection))
    ),
    extensions = c("RowGroup", "Select"),
    options = list(
      paging = FALSE,  # Disable pagination
      searching = TRUE,
      autoWidth = TRUE,
      dom = "ft",

      # Columns to hide from the user / UI
      columnDefs = list(
        list(width = "150px", targets = "_all"),
        list(className = "dt-center", targets = "_all"),
        list(targets = 0, orderable = FALSE, className = "select-checkbox"),
        list(visible = FALSE,
             targets = which(names(tlg_order_data) %in% c("Selection", "Catalog_Link")))
      ),

      # Aesthetics to create a checkbox column for selected rows
      select = list(
        style = "multiple",
        selector = "td:first-child",
        selection = "none",
        server = FALSE
      ),

      # Aesthetics to define the row distribution in groups (Tables, Listing, Graphs)
      rowGroup = list(dataSrc = which(names(tlg_order_data) %in% c("Type", "Dataset")))
    ),
    class = "table table-striped table-bordered"
  ) %>%
    # Aesthetics for the font
    formatStyle(
      columns = colnames(tlg_order_data),
      fontSize = "14px",
      fontFamily = "Arial"
    )
}, server = FALSE)

# Save table changes from the UI into the server
observeEvent(input$tlg_order_cell_edit, {
  info <- input$tlg_order_cell_edit

  # Update the reactive data frame with the new value
  tlg_order_data <- tlg_order()
  tlg_order_data[info$row, info$col] <- info$value
  tlg_order(tlg_order_data)
})


# Based on the TLG list conditions for data() define the preselected rows in $Selection
observeEvent(data(), {
  req(data())
  req(tlg_order())

  # Take the column name to later test if it is present in the data
  column_of_conditions <- gsub("([=<>!].*)", "", tlg_order()$Condition)

  # If there is any condition assess if it is true at least for one case
  tlg_order <- tlg_order() %>%
    mutate(
      Selection = case_when(
        Condition == "" | is.na(Condition) ~ Selection,
        ! column_of_conditions %in% names(data()) ~ FALSE,
        sum(eval(parse(text = Condition), envir = data())) > 0 ~ TRUE,
        TRUE ~ FALSE
      )
    )

  # Based on the data return a new preselected set of TLGs
  tlg_order(tlg_order)
})


# When the user submits the TLG order...
observeEvent(input$submit_tlg_order, {

  # Filter only the rows requested by the user
  tlg_order_filt <- tlg_order()[input$tlg_order_rows_selected, ]
  print(tlg_order_filt)
})