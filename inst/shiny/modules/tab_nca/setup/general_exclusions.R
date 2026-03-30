#' General Exclusions Shiny Module
#'
#' UI and server logic for managing and displaying NCA and TLG exclusions.
#' Allows users to select rows from a concentration table, choose exclusion
#' type(s), provide a reason, and manage exclusions with color-coded feedback.
#'
#' Row color scheme:
#' - Red:    NCA exclusion (from data or manual)
#' - Yellow: TLG exclusion only
#' - Orange: NCA + TLG exclusion

general_exclusions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Input row: checkboxes, reason text, add button, help
    div(
      style = "display: flex; gap: 8px; align-items: center; margin-bottom: 16px;",
      div(
        style = paste(
          "display: flex; flex-direction: column;",
          "gap: 0; margin-right: 4px; min-width: 170px;"
        ),
        checkboxInput(
          ns("cb_manual_nca"), "Manual NCA exclusion",
          value = FALSE, width = "100%"
        ),
        checkboxInput(
          ns("cb_tlg"), "TLG exclusion",
          value = FALSE, width = "100%"
        )
      ),
      textInput(
        ns("exclusion_reason"),
        label = NULL,
        placeholder = "Enter exclusion reason"
      ),
      actionButton(
        ns("add_exclusion_reason"),
        label = "Add",
        class = "btn btn-primary btn-sm"
      ),
      # Help button (dropdown)
      dropdown(
        div(
          style = "min-width:340px; max-width:480px;",
          tags$h2(
            "NCA Exclusions Help",
            style = "font-size:1.2em; margin-bottom:8px;"
          ),
          p(
            "Records excluded here can be removed from NCA PK",
            "calculations, from mean plots and summary tables",
            "(TLGs), or both."
          ),
          tags$ul(
            tags$li(
              tags$b("NCA exclusion"),
              ": excluded from NCA PK calculations"
            ),
            tags$li(
              tags$b("TLG exclusion"),
              ": excluded from mean plots and summary tables"
            )
          ),
          tags$h3(
            "Row Colors",
            style = "font-size:1.05em; margin:10px 0 4px;"
          ),
          tags$ul(
            tags$li(tags$b("Red"), ": NCA exclusion"),
            tags$li(tags$b("Yellow"), ": TLG exclusion only"),
            tags$li(tags$b("Orange"), ": NCA + TLG exclusion")
          ),
          p(
            "Select rows and add a reason to exclude.",
            "Remove exclusions anytime."
          )
        ),
        style = "unite",
        right = TRUE,
        icon = icon("question"),
        status = "primary"
      )
    ),
    # Table of current exclusions (compact, below input)
    uiOutput(ns("exclusion_list_ui")),
    # Color legend for the exclusions table
    div(
      class = "results-legend",
      style = paste(
        "display:flex; gap:12px; align-items:center;",
        "margin:8px 0; flex-wrap:wrap;"
      ),
      div(
        style = "font-weight:600; font-size:0.95em; margin-right:8px;",
        "Row Colors:"
      ),
      .legend_swatch(EXCL_COLOR_NCA, "NCA exclusion"),
      .legend_swatch(EXCL_COLOR_TLG, "TLG exclusion"),
      .legend_swatch(EXCL_COLOR_BOTH, "NCA + TLG exclusion")
    ),
    # Main concentration data table with row selection and color coding
    card(reactable_ui(ns("conc_table")), class = "border-0 shadow-none")
  )
}

general_exclusions_server <- function(
  id, processed_pknca_data, general_exclusions_override
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Store the list of exclusions and a counter for unique button IDs
    exclusion_list <- reactiveVal(list())
    xbtn_counter <- reactiveVal(0)

    # Initialise settings override if available
    observeEvent(general_exclusions_override(), {
      overrides <- general_exclusions_override()

      if (!is.null(overrides) && length(overrides) > 0) {
        new_ids <- seq_along(overrides) + xbtn_counter()
        rehydrated_list <- purrr::map2(
          overrides, new_ids, function(item, id) {
            item$xbtn_id <- paste0("remove_exclusion_reason_", id)
            # Ensure fields exist for older saved settings
            if (is.null(item$exclude_nca)) item$exclude_nca <- TRUE
            if (is.null(item$exclude_tlg)) item$exclude_tlg <- FALSE
            item
          }
        )

        xbtn_counter(max(new_ids))
        exclusion_list(rehydrated_list)
      }
    })

    # Reactive for the concentration data table rendered
    conc_data <- reactive({
      req(processed_pknca_data())
      processed_pknca_data()$conc$data
    })

    # Render the reactable with row coloring for exclusions
    reactable_server(
      "conc_table",
      conc_data,
      selection = "multiple",
      onClick = "select",
      borderless = TRUE,
      rowStyle = function(x) {
        function(index) {
          .exclusion_row_color(index, x, exclusion_list())
        }
      }
    )

    # Add a new exclusion when the Add button is pressed
    observeEvent(input$add_exclusion_reason, {
      .handle_add_exclusion(
        input, session, exclusion_list, xbtn_counter
      )
    })

    # Dynamically observe all remove buttons for exclusion reasons
    observe({
      lst <- exclusion_list()
      lapply(lst, function(item) {
        xbtn_id <- item$xbtn_id
        observeEvent(input[[xbtn_id]], {
          current <- exclusion_list()
          exclusion_list(
            Filter(function(x) x$xbtn_id != xbtn_id, current)
          )
        }, ignoreInit = TRUE, once = TRUE)
      })
    })

    # Render the exclusions table (not shown if empty)
    output$exclusion_list_ui <- renderUI({
      tbl <- .render_exclusion_list_table(exclusion_list(), ns)
      if (is.null(tbl)) return(NULL)
      tagList(
        tbl,
        tags$script("setTimeout(function(){ Shiny.bindAll(); }, 100);")
      )
    })

    # Prepare exclusion list for return (without xbtn_id)
    exclusion_list_for_return <- reactive({
      lapply(exclusion_list(), function(x) {
        x[setdiff(names(x), "xbtn_id")]
      })
    })

    exclusion_list_for_return
  })
}
