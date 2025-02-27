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

      param_choices <- setdiff(names(get.interval.cols()), c("start", "end"))
      param_choices <- sapply(param_choices, \(param) {
        vals <- mydata()$intervals[[param]]
        replace(vals, is.na(vals), FALSE)
        any(mydata()$intervals[[param]])
      })
      param_choices <- names(param_choices)[param_choices]

      showModal(modalDialog(
        title = tagList(span("Start Impute Table")),
        pickerInput(
          inputId = ns("select_imputetable_params"),
          multiple = TRUE,
          label = "Parameters to impute:",
          choices = param_choices,
          selected = setdiff(param_choices, c("cmax", "tmax")),
          options = list(`actions-box` = TRUE)
        ),
        reactableOutput(ns("modal_intervals_start")),
        footer = modalButton("Close"),
        size = "l"
      ))
    })

    intervals_df <- reactive({
      req(mydata())

      # Column names from the main object
      duration_col <- mydata()$dose$columns$duration
      route_col <- mydata()$dose$columns$route
      drug_col <- "DRUG"
      analyte_col <- mydata()$conc$columns$groups$group_analyte
      if (is.null(analyte_col)) analyte_col <- drug_col
      conc_group_cols <- unname(unlist(mydata()$conc$columns$groups))
      dosno_col <- "DOSNO"
      conc_col <- unname(unlist(mydata()$conc$columns$concentration))
      time_col <- unname(unlist(mydata()$conc$columns$time))
      group_cols <- unname(unlist(mydata()$conc$columns$groups)) |>
        append(unname(unlist(mydata()$dose$columns$groups))) |>
        setdiff(mydata()$conc$columns$subject) |>
        unique()
      group_cols_non_redundant <- group_cols[apply(mydata()$intervals[,group_cols],
                                                   2,
                                                   \(x) length(unique(x)) > 1)]
      cols_to_keep <- c(group_cols_non_redundant, "is.iv.bolus",
                        "is.analyte.drug", "is.first.dose", "impute")

      # Return the intervals table with some additional informative columns
      mydata()$intervals %>%
        # Take needed variables from concentration data to choose a good imputation strategy
        left_join(mydata()$conc$data %>%
                    group_by(!!!syms(c(conc_group_cols, dosno_col))) %>%
                    arrange(.[[time_col]]) %>%
                    slice(1:2) %>%
                    mutate(is.c1c2.decay = all(diff(.[[conc_col]]) > 0)) %>%
                    slice(1) %>%
                    ungroup()
                  ) %>%
        select(names(mydata()$intervals), is.c1c2.decay, route_col, duration_col) %>%
        # Create for the user clear columns that inform about the characteristics of each interval
        mutate(is.first.dose = ifelse(!!sym(dosno_col) > 1, ">1", "1"),
               is.iv.bolus = !!sym(route_col) == "intravascular" & !!sym(duration_col) == 0,
               is.analyte.drug = !!sym(analyte_col) == !!sym(drug_col)
               ) %>%
        select(cols_to_keep) %>%
        unique()
    })

    output$modal_intervals_start <- renderReactable({
      req(mydata())
      list_coldef <- setNames(lapply(names(intervals_df()),
                                     function(col) colDef(name = col)),
                              nm = names(intervals_df()))
      list_coldef <- list_coldef[3:length(list_coldef)]

      reactable(
        intervals_df(),
        columns = 
          list(
            impute = colDef(
              cell = reactable.extras::dropdown_extra(id = "select_start_modal",
                                                      choices = c("start_conc0", "start_predose", "start_cmax",
                                                                  "start_c1", "start_logslope", ""))
            )
          ),
        sortable = TRUE,
        highlight = TRUE,
        wrap = FALSE,
        resizable = TRUE,
        showPageSizeOptions = TRUE,
        striped = TRUE
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
