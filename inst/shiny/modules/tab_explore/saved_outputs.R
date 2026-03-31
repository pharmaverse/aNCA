#' Saved Outputs Button UI
#'
#' Renders a "View Exports" button below "Add to Exports".
#'
#' @param id Module namespace ID.
#'
#' @return An `actionButton` UI element.
saved_outputs_ui <- function(id) {
  ns <- NS(id)
  actionButton(
    ns("view_exports"),
    label = "View Exports",
    icon = icon("eye"),
    class = "btn btn-outline-secondary btn-sm",
    width = "100%"
  )
}

#' Saved Outputs Server
#'
#' Opens a modal with a reactable listing saved exploration plots.
#' Each row has an Open link (plotly preview) and a Remove button.
#'
#' @param id Module namespace ID.
#' @param saved_plots_metadata A reactive returning a data.frame with columns:
#'   name, type, timestamp (character).
#' @param on_remove A callback function(plot_name) called when the user removes a plot.
#' @param on_open A callback function(plot_name) called when the user opens a plot.
saved_outputs_server <- function(id, saved_plots_metadata, on_remove, on_open) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Open the modal when "View Exports" is clicked
    observeEvent(input$view_exports, {
      showModal(modalDialog(
        title = "Saved Outputs",
        uiOutput(ns("saved_outputs_table")),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })

    # Render the reactable inside the modal
    output$saved_outputs_table <- renderUI({
      meta <- saved_plots_metadata()
      if (is.null(meta) || nrow(meta) == 0) {
        return(tags$p(
          "No plots saved yet. Use ",
          tags$strong("Add to Exports"),
          " to save plots.",
          style = "color: #888; font-style: italic; margin: 16px 0;"
        ))
      }

      # Build a data.frame with button IDs for Open and Remove
      df <- data.frame(
        Name = meta$name,
        Type = meta$type,
        Time = meta$timestamp,
        open_id = paste0("open_", meta$name),
        remove_id = paste0("remove_", meta$name),
        stringsAsFactors = FALSE
      )

      tagList(
        reactable::reactable(
          df,
          compact = TRUE,
          bordered = TRUE,
          highlight = TRUE,
          pagination = FALSE,
          defaultPageSize = nrow(df),
          theme = reactable::reactableTheme(
            headerStyle = list(background = "#e9e9e9")
          ),
          columns = list(
            Name = reactable::colDef(name = "Name"),
            Type = reactable::colDef(name = "Type", width = 100),
            Time = reactable::colDef(name = "Time", width = 90),
            open_id = reactable::colDef(
              name = "",
              width = 70,
              sortable = FALSE,
              cell = function(value) {
                as.character(
                  actionButton(
                    ns(value),
                    label = "Open",
                    class = "btn btn-link btn-sm",
                    style = "padding: 2px 6px;"
                  )
                )
              },
              html = TRUE
            ),
            remove_id = reactable::colDef(
              name = "",
              width = 50,
              sortable = FALSE,
              cell = function(value) {
                as.character(
                  actionButton(
                    ns(value),
                    label = NULL,
                    icon = shiny::icon("times"),
                    class = "btn btn-link btn-sm",
                    style = "padding: 2px 6px; color: #dc3545;"
                  )
                )
              },
              html = TRUE
            )
          )
        ),
        # Bind dynamically rendered buttons
        tags$script("setTimeout(function(){ Shiny.bindAll(); }, 100);")
      )
    })

    # Observe Open and Remove clicks for each plot in the current metadata
    observe({
      meta <- saved_plots_metadata()
      req(meta, nrow(meta) > 0)

      lapply(meta$name, function(plot_name) {
        open_id <- paste0("open_", plot_name)
        remove_id <- paste0("remove_", plot_name)

        observeEvent(input[[open_id]], {
          on_open(plot_name)
        }, ignoreInit = TRUE, once = FALSE)

        observeEvent(input[[remove_id]], {
          on_remove(plot_name)
        }, ignoreInit = TRUE, once = FALSE)
      })
    })
  })
}
