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
        reactableOutput(ns("modal_intervals_start")),
        footer = modalButton("Close"),
        size = "l"
      ))
    })
    
    intervals_df <- reactive({
      req(mydata())
      duration_col <- mydata()$dose$columns$duration
      route_col <- mydata()$dose$columns$route
      drug_col <- "DRUG"
      analyte_col <- mydata()$conc$columns$groups$group_analyte
      if (is.null(analyte_col)) analyte_col <- drug_col
      dosno_col <- "DOSNO"
      mydata()$intervals %>%
        mutate(is.first.dose = ifelse(!!sym(dosno_col) > 1, ">1", "1"),
               is.iv.bolus = tolower(.[[route_col]]) == "intravascular" & .[[duration_col]] == 0,
               is.analyte.drug = !!sym(analyte_col) == !!sym(drug_col)
               )
    })

    output$modal_intervals_start <- renderReactable({
      req(mydata())

      group_cols <- unname(unlist(mydata()$conc$columns$groups)) |>
        append(unname(unlist(mydata()$dose$columns$groups))) |>
        setdiff(mydata()$conc$columns$subject) |>
        unique()
      all_cols <- c(group_cols, "is.iv.bolus", "is.analyte.drug", "is.first.dose")
      list_coldef <- setNames(lapply(all_cols, function(col) colDef(name = col)), all_cols)
browser()
      reactable(
        intervals_df(),
        columns = c(
          list_coldef,
          list(
            select = colDef(
              name = "Select",
              cell = function(value, index) {
                selectInput(
                  ns(paste0("select_", index)),
                  label = "aaa",
                  choices = c("a", "b", "c"),
                  selected = value,
                  width = "100%"
                )
              }
            )
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
