#' General Exclusions Shiny Module
#'
#' UI and server logic for managing and displaying NCA and TLG exclusions.
#' Allows users to select rows from a concentration table, choose exclusion
#' type(s), provide a reason, and manage exclusions with color-coded feedback.
#'
#' Row color scheme:
#' - Blue:       Default NCA exclusion (from data)
#' - Red:        Manual NCA exclusion only
#' - Yellow:     TLG exclusion only
#' - Orange:     Manual NCA + TLG exclusion
#' - Teal:       Default NCA + TLG exclusion

# Color constants for exclusion row highlighting
EXCL_COLOR_DEFAULT     <- "#CCE5FF"  # blue — default NCA exclusion
EXCL_COLOR_MANUAL_NCA  <- "#FFCCCC"  # red — manual NCA exclusion only
EXCL_COLOR_TLG         <- "#FFFFCC"  # yellow — TLG exclusion only
EXCL_COLOR_BOTH        <- "#FFD9B3"  # orange — manual NCA + TLG
EXCL_COLOR_DEFAULT_TLG <- "#B3E0E6"  # teal — default NCA + TLG exclusion

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
          value = TRUE, width = "100%"
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
              tags$b("Manual NCA exclusion"),
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
            tags$li(tags$b("Blue"), ": Default NCA exclusion (from data)"),
            tags$li(tags$b("Red"), ": Manual NCA exclusion only"),
            tags$li(tags$b("Yellow"), ": TLG exclusion only"),
            tags$li(tags$b("Orange"), ": Manual NCA + TLG exclusion"),
            tags$li(tags$b("Teal"), ": Default NCA + TLG exclusion")
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
      .legend_swatch(EXCL_COLOR_DEFAULT, "Default NCA exclusion"),
      .legend_swatch(EXCL_COLOR_MANUAL_NCA, "Manual NCA exclusion"),
      .legend_swatch(EXCL_COLOR_TLG, "TLG exclusion"),
      .legend_swatch(EXCL_COLOR_BOTH, "Manual NCA + TLG"),
      .legend_swatch(EXCL_COLOR_DEFAULT_TLG, "Default NCA + TLG")
    ),
    # Main concentration data table with row selection and color coding
    card(reactable_ui(ns("conc_table")), class = "border-0 shadow-none")
  )
}

#' Build a legend swatch (color box + label)
#' @noRd
.legend_swatch <- function(color, label) {
  div(
    style = "display:flex; align-items:center; gap:6px;",
    div(style = paste0(
      "width:14px; height:14px; background:", color,
      "; border:1px solid #ddd;"
    )),
    span(label, style = "font-size:0.9em;")
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
          lst <- exclusion_list()
          # Determine manual exclusion type for this row
          row_nca <- FALSE
          row_tlg <- FALSE
          for (excl in lst) {
            if (index %in% excl$rows) {
              if (isTRUE(excl$exclude_nca)) row_nca <- TRUE
              if (isTRUE(excl$exclude_tlg)) row_tlg <- TRUE
            }
          }

          # Check default NCA exclusion from data
          row_default <- FALSE
          row_data <- x[index, ]
          if (!is.null(row_data$nca_exclude) &&
                nzchar(row_data$nca_exclude)) {
            row_default <- TRUE
          }

          # Determine color based on combination
          color <- if (row_nca && row_tlg) {
            EXCL_COLOR_BOTH
          } else if (row_nca) {
            EXCL_COLOR_MANUAL_NCA
          } else if (row_default && row_tlg) {
            EXCL_COLOR_DEFAULT_TLG
          } else if (row_tlg) {
            EXCL_COLOR_TLG
          } else if (row_default) {
            EXCL_COLOR_DEFAULT
          } else {
            NULL
          }

          if (!is.null(color)) list(background = color) else NULL
        }
      }
    )

    # Add a new exclusion when the Add button is pressed
    observeEvent(input$add_exclusion_reason, {
      rows_sel <- getReactableState("conc_table-table", "selected")
      reason <- input$exclusion_reason
      nca_checked <- isTRUE(input$cb_manual_nca)
      tlg_checked <- isTRUE(input$cb_tlg)

      if (length(rows_sel) > 0 && nzchar(reason) &&
            (nca_checked || tlg_checked)) {
        current <- exclusion_list()
        xbtn_id <- paste0(
          "remove_exclusion_reason_", xbtn_counter() + 1
        )
        xbtn_counter(xbtn_counter() + 1)
        list_new_reason <- list(list(
          reason = reason, rows = rows_sel,
          xbtn_id = xbtn_id,
          exclude_nca = nca_checked,
          exclude_tlg = tlg_checked
        ))
        exclusion_list(append(current, list_new_reason))
        # Clear selected rows and reason input
        updateTextInput(session, "exclusion_reason", value = "")
        updateReactable("conc_table-table", selected = NA)
      }
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
      lst <- exclusion_list()
      if (length(lst) == 0) return(NULL)
      tags$table(
        style = paste(
          "width:100%",
          "background:#f9f9f9",
          "font-size:0.95em",
          "margin-bottom:12px",
          "border-radius:4px",
          "border-collapse:separate",
          "border-spacing:0",
          sep = "; "
        ),
        tags$thead(
          tags$tr(
            tags$th(
              "Rows", style = "font-weight:600; padding:4px 8px;"
            ),
            tags$th(
              "Reason", style = "font-weight:600; padding:4px 8px;"
            ),
            tags$th(
              "Type", style = "font-weight:600; padding:4px 8px;"
            ),
            tags$th("", style = "width:36px;")
          )
        ),
        tags$tbody(
          lapply(lst, function(item) {
            type_label <- .exclusion_type_label(
              item$exclude_nca, item$exclude_tlg
            )
            tags$tr(
              tags$td(
                paste(item$rows, collapse = ", "),
                style = "padding:4px 8px;"
              ),
              tags$td(item$reason, style = "padding:4px 8px;"),
              tags$td(type_label, style = "padding:4px 8px;"),
              tags$td(
                actionButton(
                  ns(item$xbtn_id),
                  label = NULL,
                  icon = shiny::icon("times"),
                  class = "btn btn-link btn-sm",
                  style = "padding:2px 6px;"
                )
              )
            )
          })
        )
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

#' Format exclusion type as a readable label
#' @noRd
.exclusion_type_label <- function(nca, tlg) {
  parts <- c()
  if (isTRUE(nca)) parts <- c(parts, "NCA")
  if (isTRUE(tlg)) parts <- c(parts, "TLG")
  paste(parts, collapse = " + ")
}
