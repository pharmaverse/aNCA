#' Saved Outputs Table UI
#'
#' Renders a table of saved exploration plots below the "Add to Exports" button.
#' Shows name, type, timestamp, an Open link (plotly preview), and a Remove button.
#'
#' @param id Module namespace ID.
#'
#' @return A `uiOutput` placeholder for the saved outputs table.
saved_outputs_ui <- function(id) {

  ns <- NS(id)
  uiOutput(ns("saved_outputs_table"))
}

#' Saved Outputs Table Server
#'
#' @param id Module namespace ID.
#' @param saved_plots_metadata A reactive returning a data.frame with columns:
#'   name, type, timestamp (character).
#' @param on_remove A callback function(plot_name) called when the user removes a plot.
#' @param on_open A callback function(plot_name) called when the user opens a plot.
saved_outputs_server <- function(id, saved_plots_metadata, on_remove, on_open) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render the table UI with JS-driven buttons that set Shiny input values
    output$saved_outputs_table <- renderUI({
      meta <- saved_plots_metadata()
      if (is.null(meta) || nrow(meta) == 0) {
        return(NULL)
      }

      rows <- lapply(seq_len(nrow(meta)), function(i) {
        row <- meta[i, ]
        plot_name <- row$name
        # Use Shiny.setInputValue to communicate clicks back to the server.
        # The timestamp ensures each click is treated as a new event.
        open_js <- sprintf(
          "Shiny.setInputValue('%s', {name: '%s', ts: Date.now()});",
          ns("open_plot"), plot_name
        )
        remove_js <- sprintf(
          "Shiny.setInputValue('%s', {name: '%s', ts: Date.now()});",
          ns("remove_plot"), plot_name
        )
        tags$tr(
          tags$td(row$name, style = "padding: 4px 6px; word-break: break-all;"),
          tags$td(row$type, style = "padding: 4px 6px;"),
          tags$td(row$timestamp, style = "padding: 4px 6px;"),
          tags$td(
            tags$a(
              "Open", href = "#",
              onclick = paste0(open_js, "return false;"),
              style = "cursor: pointer;"
            ),
            style = "padding: 4px 6px;"
          ),
          tags$td(
            tags$a(
              href = "#",
              onclick = paste0(remove_js, "return false;"),
              style = "cursor: pointer; color: #dc3545;",
              icon("trash")
            ),
            style = "padding: 4px 6px;"
          )
        )
      })

      tagList(
        tags$hr(style = "margin: 10px 0;"),
        tags$strong("Saved Outputs", style = "font-size: 0.85em;"),
        tags$div(
          style = "max-height: 250px; overflow-y: auto; margin-top: 6px;",
          tags$table(
            class = "table table-sm table-hover",
            style = "font-size: 0.8em; margin-bottom: 0;",
            tags$thead(
              tags$tr(
                tags$th("Name", style = "padding: 4px 6px;"),
                tags$th("Type", style = "padding: 4px 6px;"),
                tags$th("Time", style = "padding: 4px 6px;"),
                tags$th("", style = "padding: 4px 6px;"),
                tags$th("", style = "padding: 4px 6px;")
              )
            ),
            tags$tbody(rows)
          )
        )
      )
    })

    # Handle Open click — input$open_plot is set via JS with {name, ts}
    observeEvent(input$open_plot, {
      req(input$open_plot$name)
      on_open(input$open_plot$name)
    }, ignoreInit = TRUE)

    # Handle Remove click — input$remove_plot is set via JS with {name, ts}
    observeEvent(input$remove_plot, {
      req(input$remove_plot$name)
      on_remove(input$remove_plot$name)
    }, ignoreInit = TRUE)
  })
}
