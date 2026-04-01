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
#' Opens a single modal displaying all saved exploration plots vertically.
#' Each plot is rendered inline with a header (name, type, timestamp) and
#' a remove button. The user scrolls to compare plots at a glance.
#'
#' @param id Module namespace ID.
#' @param saved_plots_metadata A reactive returning a data.frame with columns:
#'   name, type, timestamp (character).
#' @param get_plot_obj A function(plot_name) that returns the ggplot object
#'   for the given plot name, or NULL if not found.
#' @param on_remove A callback function(plot_name) called when the user
#'   removes a plot.
saved_outputs_server <- function(id, saved_plots_metadata, get_plot_obj,
                                 on_remove) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    show_modal <- function() {
      showModal(modalDialog(
        title = "Saved Outputs",
        uiOutput(ns("saved_outputs_gallery")),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }

    observeEvent(input$view_exports, {
      show_modal()
    })

    # Render a vertical gallery of all saved plots inside the modal.
    # Each plot gets its own plotlyOutput and a header with metadata.
    # Remove buttons use Shiny.setInputValue via JS onclick to avoid
    # observer accumulation.
    output$saved_outputs_gallery <- renderUI({
      meta <- saved_plots_metadata()
      if (is.null(meta) || nrow(meta) == 0) {
        return(tags$p(
          "No plots saved yet. Use ",
          tags$strong("Add to Exports"),
          " to save plots.",
          style = "color: #888; font-style: italic; margin: 16px 0;"
        ))
      }

      remove_input_id <- ns("remove_plot")
      escape_js <- function(x) gsub("'", "\\\\'", x)

      plot_cards <- lapply(seq_len(nrow(meta)), function(i) {
        plot_name <- meta$name[i]
        plot_type <- meta$type[i]
        plot_ts <- meta$timestamp[i]
        output_id <- ns(paste0("gallery_plot_", i))

        remove_js <- sprintf(
          "Shiny.setInputValue('%s', {name:'%s', ts:Date.now()});",
          remove_input_id, escape_js(plot_name)
        )

        output[[paste0("gallery_plot_", i)]] <- renderPlotly({
          plot_obj <- get_plot_obj(plot_name)
          req(plot_obj)
          ggplotly(plot_obj)
        })

        tags$div(
          style = paste0(
            "border: 1px solid #dee2e6; border-radius: 6px; ",
            "padding: 12px; margin-bottom: 16px; background: #fff;"
          ),
          tags$div(
            style = paste0(
              "display: flex; justify-content: space-between; ",
              "align-items: center; margin-bottom: 8px;"
            ),
            tags$div(
              tags$strong(plot_name, style = "font-size: 1.1em;"),
              tags$span(
                paste0(" \u2014 ", plot_type, " \u00b7 ", plot_ts),
                style = "color: #666; font-size: 0.9em;"
              )
            ),
            tags$a(
              href = "#",
              onclick = paste0(remove_js, "return false;"),
              style = paste0(
                "color: #dc3545; font-size: 1.2em; ",
                "text-decoration: none;"
              ),
              title = "Remove from exports",
              icon("times")
            )
          ),
          plotlyOutput(output_id, width = "800px", height = "800px")
        )
      })

      tags$div(
        style = "display: flex; flex-direction: column;",
        plot_cards
      )
    })

    observeEvent(input$remove_plot, {
      req(input$remove_plot$name)
      on_remove(input$remove_plot$name)
    }, ignoreInit = TRUE)
  })
}
