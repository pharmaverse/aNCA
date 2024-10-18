tab_tlg_ui <- function(id) {
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel("Order details",
             actionButton(ns("add_tlg"), "Add TLG"),
             actionButton(ns("remove_tlg"), "Remove TLG"),
             actionButton(ns("submit_tlg_order"), "Submit Order Details"),
             
             DTOutput(ns("selected_tlg_table")),
             actionButton(ns("submit_tlg_order"), "Submit Order Details")
    ),
    tabPanel("Tables",
             fluidRow(
               column(2,  # Left column for plot selection
                      radioButtons(inputId = ns("buttons_Tables"), label = "Choose Table\n", choices = "")
               ),
               column(6,  # Middle column for plot output
                      h4("Table to display"),
                      plotOutput(ns("plot_Tables"))
               ),
               column(2,  # Right column for plot customization inputs
                      h4("Inputs with selected vals linked to downloadable object (i.e, tlg_order())"),
                      textInput(ns("footnote_Tables"), label = 'Footnote')
               )
             )
    ),
    tabPanel("Listings",
             fluidRow(
               column(2,  # Left column for plot selection
                      radioButtons(inputId = ns("buttons_Listings"), label = "Choose List\n", choices = "")
               ),
               column(6,  # Middle column for plot output
                      h4("Listing to display"),
                      plotOutput(ns("plot_Listings"))
               ),
               column(2,  # Right column for plot customization inputs
                      h4("Inputs with selected vals linked to downloadable object (i.e, tlg_order())"),
                      textInput(ns("footnote_Listings"), label = 'Footnote')
               )
             )
    ),
    tabPanel("Graphs",
             fluidRow(
               column(2,  # Left column for plot selection
                      radioButtons(inputId = ns("buttons_Graphs"), label = "Choose Graph\n", choices = "")
               ),
               column(6,  # Middle column for plot output
                      h4("Graph to display"),
                      plotOutput(ns("plot_Graphs"))
               ),
               column(2,  # Right column for plot customization inputs
                      h4("Inputs with selected vals linked to downloadable object (i.e, tlg_order())"),
                      textInput(ns("footnote_Graphs"), label = 'Footnote')
               )
             )
    )
  )
}

tab_tlg_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # SERVER LOGIC OF NAVBAR TLG TAB ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Order details: Input definition for Tables, Listings and Graphs to generate in the other tabs
    # Tables:
    # Listings:
    # Graphs:
    
    # TABSET: Order details =============================================
    
    # Make available the CSV file with the TLG list and available links to NEST
    tlg_order <- reactiveVal(
      # TODO(mateusz): migrate this to shiny/data folder
      read.csv(file.path(assets, "data/TLG_order_details.csv")) %>%
        mutate(PKid = paste0("<a href='", Catalog_Link, "' target='_blank'>", PKid, "</a>"))
    )
    
    # Based on the TLG list conditions for data() define the preselected rows in $Selection
    observeEvent(list(tlg_order(), data()), {
      
      # Take the column name to later test if it is present in the data
      column_of_conditions <- gsub("([=<>!].*)", "", tlg_order()$Condition)
      
      # If there is any condition assess if it is true at least for one case
      new_tlg_order <- tlg_order() %>%
        mutate(
          Selection = case_when(
            Condition == "" | is.na(Condition) ~ Selection,
            ! column_of_conditions %in% names(data()) ~ FALSE,
            sum(eval(parse(text = Condition), envir = data())) > 0 ~ TRUE,
            TRUE ~ FALSE
          )
        )
      
      # Return TLG list with preselections
      tlg_order(new_tlg_order)
    })
    
    
    # Render the TLG list for the user's inspection
    output$selected_tlg_table <- DT::renderDT({
      
      # Render the editable DT table
      datatable(
        elementId = "selected_tlg_datatable",
        class = "tlg-order-datatable",
        data = dplyr::filter(tlg_order(), Selection),
        editable = list(
          target = "cell",
          disable = list(
            columns = which(!names(tlg_order()) %in% c("Footnote", "Stratification", "Comment"))
          )
        ),
        rownames = TRUE,
        escape = FALSE,
        # Server/Client processing of selected rows
        selection = list(
          mode = "multiple"
        ),
        extensions = c("RowGroup"),
        options = list(
          paging = FALSE,
          searching = TRUE,
          autoWidth = TRUE,
          dom = "ft",
          # Columns to hide from the user / UI
          columnDefs = list(
            list(width = "150px", targets = "_all"),
            list(className = "dt-center", targets = "_all"),
            list(
              visible = FALSE,
              targets = c(
                0,
                which(!names(tlg_order()) %in% c(
                  "Type", "Dataset", "PKid", "Label", "Footnote", "Stratification", "Comment"
                ))
              )
            )
          ),
          
          # Aesthetics to define the row distribution in groups (Tables, Listing, Graphs)
          rowGroup = list(dataSrc = which(names(tlg_order()) %in% c("Type", "Dataset")))
        ),
        class = "table table-striped table-bordered"
      ) %>%
        # Aesthetics for the font
        formatStyle(
          columns = colnames(tlg_order()),
          fontSize = "14px",
          fontFamily = "Arial"
        )
    }, server = FALSE)
    
    # Save table changes from the UI into the server
    observeEvent(input$selected_tlg_table_cell_edit, {
      info <- input$selected_tlg_table_cell_edit
      
      # Update the reactive data frame with the new value
      new_tlg_order <- tlg_order()
      new_tlg_order[new_tlg_order$Selection, ][info$row, info$col] <- info$value
      tlg_order(new_tlg_order)
    })
    
    
    # Show modal when the add_tlg button is pressed
    observeEvent(input$add_tlg, {
      showModal(modalDialog(
        title = "Add TLGs to Order",
        DTOutput(session$ns("modal_tlg_table")),
        footer = tagList(
          modalButton("Close"),
          actionButton(session$ns("confirm_add_tlg"), "Add TLGs to Order")
        ),
        size = "l"
      ))
    })
    
    # Render the DT table in the modal
    output$modal_tlg_table <- DT::renderDT({
      datatable(
        data = dplyr::filter(tlg_order(), !Selection),
        selection = list(mode = "multiple"),
        escape = FALSE,
        editable = FALSE,
        class = "table table-striped table-bordered",
        extensions = c("RowGroup", "Select"),
        options = list(
          paging = FALSE,
          searching = TRUE,
          autoWidth = TRUE,
          dom = "ft",
          columnDefs = list(
            list(
              visible = FALSE,
              targets = which(!names(tlg_order()) %in% c("Type", "Dataset", "PKid", "Label"))
            ),
            list(targets = 0, orderable = FALSE, className = "select-checkbox")
          ),
          
          # Aesthetics to create a checkbox column for selected rows
          select = list(
            style = "multiple",
            selector = "td:first-child",
            selection = "none",
            server = FALSE
          ),
          
          # Aesthetics to define the row distribution in groups (Tables, Listing, Graphs)
          rowGroup = list(dataSrc = which(names(tlg_order()) %in% c("Type", "Dataset")))
        )
      )
    })
    
    # Update the Selection column when the confirm_add_tlg button is pressed
    observeEvent(input$confirm_add_tlg, {
      print("confirm_add_tlg")
      selected_rows <- input$modal_tlg_table_rows_selected
      if (length(selected_rows) > 0) {
        print("action")
        print(selected_rows)
        tlg_order_data <- tlg_order()
        tlg_order_data$Selection[!tlg_order_data$Selection][selected_rows] <- TRUE
        tlg_order(tlg_order_data)
      }
      removeModal()
    })
    
    
    # Update the Selection column when the confirm_add_tlg button is pressed
    observeEvent(input$remove_tlg, {
      selected_rows <- input$selected_tlg_table_rows_selected
      
      if (length(selected_rows) > 0) {
        tlg_order_data <- tlg_order()
        tlg_order_data$Selection[tlg_order_data$Selection][selected_rows] <- FALSE
        tlg_order(tlg_order_data)
      }
    })
    
    
    # When the user submits the TLG order...
    observeEvent(input$submit_tlg_order, {
      
      # Filter only the rows requested by the user
      tlg_order_filt <- tlg_order()[tlg_order()$Selection, ]
      
      ## possible TODO(Gerardo): func_plot = get(tlg_order_filt$Function[i])
      ## possible TODO(Gerardo): Generate all plots, with id based on $Label and save them in outputs 
      
      
      # Update TLG options to display in each tab
      if (sum(tlg_order_filt$Type == "Table") > 0) {
        updateRadioButtons(session = session, inputId = "buttons_Tables", label = "Table to display", 
                           choices = tlg_order_filt$Label[tlg_order_filt$Type == "Table"] )
      } else {
        updateRadioButtons(session = session, inputId = "buttons_Tables", label = "", choices = "")
      }
      
      if (sum(tlg_order_filt$Type == "Listing") > 0) {
        updateRadioButtons(session = session, inputId = "buttons_Listings", label = "Listing to display", 
                           choices = tlg_order_filt$Label[tlg_order_filt$Type == "Listing"])
      } else {
        updateRadioButtons(session = session, inputId = "buttons_Listings", label = "", choices = "")
      }
      
      if (sum(tlg_order_filt$Type == "Graph") > 0) {
        updateRadioButtons(session = session, inputId = "buttons_Graphs", label = "Graph to display", 
                           choices = tlg_order_filt$Label[tlg_order_filt$Type == "Graph"])
      } else {
        updateRadioButtons(session = session, inputId = "buttons_Graphs", label = "", choices = "")
      }
      
    })
    
    # Render the outputs to display dynamically
    
    
    
  })
}