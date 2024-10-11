# SERVER LOGIC OF NAVBAR TLG TAB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Order details: Input definition for Tables, Listings and Graphs to generate
# Tables:
# Listings:
# Graps:

# TABSET: General Individual Plots =============================================
#
# Order details: Displays specifications file for the TLGs and allows user to inspect and their details and make certain changes


# Make available the file defined in UI DTOutput("TLG_order_details"), an editable table with certain columns not editable 
TLG_order <- reactiveVal(
  # Read the excel file
  read.csv("www/data/TLG_order_details.csv")  
)

output$TLG_order <- DT::renderDT({
  # Load reactive value
  TLG_order <- TLG_order() 
  
  # Generate a color palette based on those
  colors <- colorRampPalette(c("#f8fbfd", '#f9f8fd', '#f2f3ff', '#fbf3ff'))(length(unique(TLG_order$type)))
  
  # Render the editable DT table
  datatable(data = TLG_order, 
            editable = TRUE, 
            rownames = TRUE,
            extensions = c('Select', 'RowGroup'),
            options = list(
              paging = FALSE,  # Disable pagination
              autoWidth = TRUE,
              dom = 't',  # Hide length menu
              columnDefs = list(
                list(width = '150px', targets = '_all'),
                list(className = 'dt-center', targets = '_all'),
                list(targets = 0, orderable = FALSE, className = "select-checkbox"),
                list(visible = FALSE, 
                     targets = which(names(TLG_order) %in% c("row_group", "Selection")))  # Hide the row_group and Selection columns
              ),
              select = list(
                style = "os", 
                selector = "td:first-child",  # Only select by the first cell (checkbox)
                selected = which(as.logical(TLG_order$Selection))
              ),
              rowGroup = list(dataSrc = which(names(TLG_order) %in% c('Type', 'Dataset')))
            ),
            class = 'table table-striped table-bordered'
  ) %>% 
    formatStyle(
      columns = colnames(TLG_order),
      fontSize = '14px',
      fontFamily = 'Arial'
    ) 
})



# Save user table changes from the UI into the server
observeEvent(input$TLG_order_cell_edit, {
  info <- input$TLG_order_cell_edit
  
  # Update the reactive data frame with the new value
  TLG_order <- TLG_order() 
  TLG_order[info$row, (info$col + 1)] <- info$value
  TLG_order(TLG_order)
})


# When the user submits the TLG order...
observeEvent(input$submit_TLG_order, {

  # Filter the rows requested by the user
  TLG_order = TLG_order()[input$TLG_order_rows_selected,]
  
  ## FOR LOOOP OR LAPPLY
  print(TLG_order)
  
  
  # Take each TLG function 
  #func <- get(func_name)
  
  ## ANOTHER LOOP 
  
  # Apply each function over each group set of records
  
})



