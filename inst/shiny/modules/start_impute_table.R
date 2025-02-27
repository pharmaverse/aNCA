start_impute_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      ns("open_start_impute_table"),
      icon = icon("table"),
      label = "Start Impute Table",
      disabled = TRUE
    )
  )
}

start_impute_table_server <- function(id, mydata) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(mydata(), {
      updateActionButton(session, "open_start_impute_table", disabled = FALSE)
    })
    
    observeEvent(input$open_start_impute_table, {
      showModal(modalDialog(
        title = tagList(span("Start Impute Table")),
        reactableOutput(ns("modal_start_impute_table")),
        footer = modalButton("Close"),
        size = "l"
      ))
    })
    
    intervals_df <- reactive({
      req(mydata())
      mydata$intervals %>%
        mutate(DOSNOs = ifelse(DOSNO > 1, ">1", 1))
    })
    
    output$modal_intervals_start <- renderReactable({
      req(mydata())
      browser()

      group_cols <- unname(unlist(mydata()$conc$columns$groups)) |>
        append(unname(unlist(mydata()$dose$columns$groups))) |>
        setdiff(mydata()$conc$columns$subject) |>
        unique()

      reactable(
        intervals_df %>%
          select(group_cols, DOSNOs, impute) %>%
          unique(),
        columns = list(
          start = colDef(name = "Start"),
          end = colDef(name = "End"),
          DOSNO = colDef(name = "Dose Number"),
          ANALYTE = colDef(name = "Analyte"),
          select = colDef(
            name = "Select",
            cell = function(value, index) {
              selectInput(
                ns(paste0("select_", index)),
                label = NULL,
                choices = c("a", "b", "c"),
                selected = value,
                width = "100%"
              )
            }
          )
        ),
        searchable = TRUE,
        sortable = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        resizable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE,
        bordered = TRUE,
        height = "60vh"
      )
    })
    
    # Accept user modifications in the modal units table
    observeEvent(input$modal_units_table_cell_edit, {
      
      info <- input$modal_units_table_cell_edit
      intervals_df <- mydata()$intervals

      rows_to_change <- semi_join(intervals_df %>%
                                    select(group_cols, DOSNOs, impute) %>%
                                    unique() %>%
                                    slice(info$row),
                                  intervals_df)
      
      


      
      # Update the server table
      modal_units_table(modal_units_table)
    })
    
  })
}
