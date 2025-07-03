# Define UI for the combined application
ui <- fluidPage(
      variable_browser_ui(
        dataname = "ADPC"
      )
)

# Define server logic for the combined application
server <- function(input, output, session) {
  
  # Call the server function for the variable browser
  data_for_vb <- reactive({
    
    # Ensure column labels exist, as the helper functions rely on them.
    for (col_name in names(adpc_data)) {
      current_label <- attr(adpc_data[[col_name]], "label")
      if (is.null(current_label) || current_label == "") {
        attr(adpc_data[[col_name]], "label") <- col_name
      }
    }
    
    # Create an empty join_keys object since there are none for this single dataset.
    jks <- teal.data::join_keys()
    
    # Use the constructor to create the final S4 teal_data object.
    data_list <- teal.data::teal_data(
      ADPC = adpc_data,
      join_keys = jks
    )
    
    data_list
  })
  
  # Pass the reactive expression directly
  variable_browser_server(
    input = input,
    output = output,
    session = session,
    data_list_reactive = data_for_vb,
    dataname_param = "ADPC",
    parent_dataname_param = "ADPC"
  )
}