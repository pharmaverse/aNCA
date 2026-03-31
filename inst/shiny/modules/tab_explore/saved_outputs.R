# --- Pure helper functions for metadata management ---

#' Create an empty saved-plots metadata data.frame
#'
#' @return A zero-row data.frame with columns: name, type, timestamp.
empty_saved_plots_metadata <- function() {
  data.frame(
    name = character(0), type = character(0),
    timestamp = character(0), stringsAsFactors = FALSE
  )
}

#' Add or update a row in saved-plots metadata
#'
#' If `plot_name` already exists, its timestamp and type are updated.
#' Otherwise a new row is appended.
#'
#' @param meta A data.frame as returned by `empty_saved_plots_metadata()`.
#' @param plot_name Character; the plot name.
#' @param type_label Character; display label ("Individual", "Mean", "QC").
#' @param timestamp Character; formatted timestamp string.
#'
#' @return Updated data.frame.
upsert_saved_plot <- function(meta, plot_name, type_label, timestamp) {
  if (plot_name %in% meta$name) {
    meta$timestamp[meta$name == plot_name] <- timestamp
    meta$type[meta$name == plot_name] <- type_label
  } else {
    meta <- rbind(meta, data.frame(
      name = plot_name, type = type_label, timestamp = timestamp,
      stringsAsFactors = FALSE
    ))
  }
  meta
}

#' Remove a plot from saved-plots metadata
#'
#' @param meta A data.frame as returned by `empty_saved_plots_metadata()`.
#' @param plot_name Character; the plot name to remove.
#'
#' @return Updated data.frame with the row removed.
remove_saved_plot <- function(meta, plot_name) {
  meta[meta$name != plot_name, , drop = FALSE]
}

# --- Shiny module ---

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
    class = "btn btn-primary btn-sm",
    width = "100%"
  )
}

#' Saved Outputs Server
#'
#' Opens a modal with a reactable listing saved exploration plots.
#' Each row has an Open link (plotly preview) and a Remove button.
#' Button clicks use `Shiny.setInputValue` via JS to avoid observer
#' accumulation — a single `observeEvent` per action type handles all rows.
#'
#' @param id Module namespace ID.
#' @param saved_plots_metadata A reactive returning a data.frame with columns:
#'   name, type, timestamp (character).
#' @param on_remove A callback function(plot_name) called when the user removes a plot.
#' @param on_open A callback function(plot_name, reopen) called when the user
#'   opens a plot. `reopen` is a function that re-opens this modal.
saved_outputs_server <- function(id, saved_plots_metadata, on_remove, on_open) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reusable function to show the Saved Outputs modal
    show_modal <- function() {
      showModal(modalDialog(
        title = "Saved Outputs",
        uiOutput(ns("saved_outputs_table")),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }

    # Open the modal when "View Exports" is clicked
    observeEvent(input$view_exports, {
      show_modal()
    })

    # Render the reactable inside the modal.
    # Buttons use onclick JS to set namespaced Shiny inputs with a timestamp,
    # so each click is treated as a new event without creating new observers.
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

      # Build a data.frame with plot names for Open and Remove columns
      df <- data.frame(
        Name = meta$name,
        Type = meta$type,
        DateTime = meta$timestamp,
        open_name = meta$name,
        remove_name = meta$name,
        stringsAsFactors = FALSE
      )

      open_input_id <- ns("open_plot")
      remove_input_id <- ns("remove_plot")

      # Escape single quotes in plot names to prevent JS injection
      escape_js <- function(x) gsub("'", "\\\\'", x)

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
            Name = reactable::colDef(name = "Name", width = 200),
            Type = reactable::colDef(name = "Type", width = 200),
            DateTime = reactable::colDef(name = "DateTime", width = 200),
            open_name = reactable::colDef(
              name = "",
              width = 70,
              sortable = FALSE,
              cell = function(value) {
                js <- sprintf(
                  paste0(
                    "Shiny.setInputValue('%s',",
                    " {name:'%s', ts:Date.now()});"
                  ),
                  open_input_id, escape_js(value)
                )
                as.character(tags$a(
                  "Open", href = "#",
                  onclick = paste0(js, "return false;"),
                  style = "cursor:pointer;"
                ))
              },
              html = TRUE
            ),
            remove_name = reactable::colDef(
              name = "",
              width = 50,
              sortable = FALSE,
              cell = function(value) {
                js <- sprintf(
                  paste0(
                    "Shiny.setInputValue('%s',",
                    " {name:'%s', ts:Date.now()});"
                  ),
                  remove_input_id, escape_js(value)
                )
                as.character(tags$a(
                  href = "#",
                  onclick = paste0(js, "return false;"),
                  style = "cursor:pointer; color:#dc3545;",
                  as.character(icon("times"))
                ))
              },
              html = TRUE
            )
          )
        )
      )
    })

    # Single observer for Open — input$open_plot is set via JS with {name, ts}
    observeEvent(input$open_plot, {
      req(input$open_plot$name)
      on_open(input$open_plot$name, reopen = show_modal)
    }, ignoreInit = TRUE)

    # Single observer for Remove — input$remove_plot is set via JS with {name, ts}
    observeEvent(input$remove_plot, {
      req(input$remove_plot$name)
      on_remove(input$remove_plot$name)
    }, ignoreInit = TRUE)
  })
}
