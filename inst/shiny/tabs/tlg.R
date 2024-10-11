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
  readxl::read_excel("www/data/TLG_order_details.xlsx")  
)

output$TLG_order <- DT::renderDT({

    # Differentiate rows by a defined color column
    TLG_order <- TLG_order() %>% 
      arrange(dplyr::desc(Type), Dataset)  %>% 
      mutate(row_colour = as.factor(interaction(Type, Dataset)))
    
    # Generate a color palette based on those
    colors <- colorRampPalette(c("#f8fbfd", '#f9f8fd', '#f2f3ff', '#fbf3ff'))(length(unique(TLG_order$row_colour)))
    
    # Render the editable DT table
    datatable(data = TLG_order, 
              editable = TRUE, 
              rownames = FALSE,
              options = list(
                paging = FALSE,  # Disable pagination
                autoWidth = TRUE,
                dom = 't',  # Hide length menu
                columnDefs = list(
                  list(width = '150px', targets = '_all'),
                  list(className = 'dt-center', targets = '_all'),
                  list(targets = 0, render = JS(
                    "function(data, type, row, meta) {",
                    "  return type === 'display' ? '<input type=\"checkbox\" ' + (data ? 'checked' : '') + '>' : data;",
                    "}"
                  )), 
                  list(visible = FALSE, 
                       targets = which(colnames(TLG_order) == "row_colour") - 1)  # Hide the row_colour column
                ),
                headerCallback = JS(
                  "function(thead, data, start, end, display) {",
                  "  $(thead).css('background-color', '#eaf0f3');",  # Set header background color
                  "  $(thead).css('color', '#333');",  # Set header text color
                  "}"
                )
              ),
              class = 'table table-striped table-bordered'
    ) %>% 
      formatStyle(
        columns = colnames(TLG_order),
        fontSize = '14px',
        fontFamily = 'Arial'
      ) %>%
      formatStyle(
        'row_colour',  # Apply distinct row coloring based on row_colour
        target = 'row',
        backgroundColor = styleEqual(
          unique(TLG_order$row_colour),
          colors
        )
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

  ## FOR LOOOP OR LAPPLY
  print(TLG_order())
  
  
  # Take each TLG function 
  #func <- get(func_name)
  
  ## ANOTHER LOOP 
  
  # Apply each function over each group set of records
  
})
