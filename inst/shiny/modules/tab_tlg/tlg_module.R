#' Module handling specific Tables, Lists and Graphs.
#'
#' @details
#' Takes in the definition of a TLG, as defined in `tlg.yaml` file. Generates a paginated interface
#' that allows viewing of the TLG with default values. In addition, generates editing widgets in a
#' sidebar that allow the user to specify parameters passed into the rendering function.
#'
#' To read more check out documentation for each function of the module and the contributing
#' guidelines.

#' Filter out rows excluded from TLG summaries by a single dataset flag.
#'
#' Removes rows flagged for exclusion from summary tables using the flag that
#' belongs to that dataset only:
#' - ADNCA data: filter by `PKSUM1F` (`"Y"` == excluded).
#' - ADPP data: filter by `PPSUMFL` (`"Y"` == excluded).
#'
#' Only the flag named by `flag` is applied; the other dataset's flag is
#' intentionally ignored even when both columns are present. A record may be
#' excluded from the PK-parameter summary (`PPSUMFL == "Y"`) while still being
#' wanted in the concentration representations, and vice-versa, so scoping each
#' flag to its own dataset avoids dropping such records from the other TLGs.
#'
#' @param data A data frame (ADNCA or ADPP).
#' @param flag Name of the exclusion-flag column to apply
#'   (`"PKSUM1F"` for ADNCA, `"PPSUMFL"` for ADPP). Absent columns are a no-op.
#' @return The filtered data frame.
#' @noRd
filter_tlg_excluded <- function(data, flag) {
  if (flag %in% names(data)) {
    data <- data[is.na(data[[flag]]) | data[[flag]] != "Y", , drop = FALSE]
  }
  data
}

#' Data-source key for a TLG module.
#'
#' `PKSUM1F` / `PPSUMFL == "Y"` flag rows excluded from *summary tables and mean
#' plots* — not from individual listings.  Listings therefore consume the raw,
#' unfiltered `"<dataset>_all"` source, while tables and graphs use the
#' summary-filtered source keyed by dataset name.
#'
#' @param type    TLG type: `"table"`, `"graph"`, or `"listing"`.
#' @param dataset Source dataset name, `"ADNCA"` or `"ADPP"`.
#' @return A character key naming the data reactive the module should use.
#' @noRd
tlg_data_key <- function(type, dataset) {
  if (identical(type, "listing")) paste0(dataset, "_all") else dataset
}

#' Wire up per-plot plotly outputs for a graph TLG module.
#'
#' Renders each graph through its own `plotlyOutput`/`renderPlotly` pair rather
#' than returning raw plotly widgets from a single `renderUI`. Raw widgets did
#' not redraw in place when an option changed (e.g. a custom title on combined
#' pkcg02 plots) until the figure was hidden and shown again (issue #1336);
#' letting plotly own each output binding makes edits apply immediately.
#'
#' @param output             Module `output` object.
#' @param session            Module `session` object.
#' @param current_page_items Reactive returning the items shown on the current
#'                           page (plotly widgets, ggplots, or character errors).
#' @noRd
render_graph_outputs <- function(output, session, current_page_items) {
  output$tlg_output <- renderUI({
    items <- current_page_items()
    tagList(purrr::imap(items, function(item, i) {
      if (is.character(item)) {
        tags$pre(item)
      } else {
        # preserve the height baked into the plotly object (set via ggplotly)
        height <- if (!is.null(item$height)) paste0(item$height, "px") else "500px"
        plotly::plotlyOutput(session$ns(paste0("plot_", i)), height = height)
      }
    }))
  })

  # Register a renderPlotly binding for each plot slot exactly once (tracked by
  # high-water mark). The bodies read current_page_items() reactively, so option
  # edits re-run them and plotly redraws the existing widget in place.
  n_registered <- 0L
  observe({
    n <- length(current_page_items())
    if (n > n_registered) {
      for (i in seq.int(n_registered + 1L, n)) local({
        my_i <- i
        output[[paste0("plot_", my_i)]] <- plotly::renderPlotly({
          items <- current_page_items()
          req(my_i <= length(items))
          item <- items[[my_i]]
          req(!is.character(item))
          # implemented graph functions (g_pkcg*) always return plotly widgets
          item
        })
      })
      n_registered <<- n
    }
  })
}

#' Wire up the table output for a table TLG module.
#'
#' Renders each page item as a `reactable`, prefixed by an `<h4>` group header
#' when the item carries a split key (e.g. `"Drug A / PLASMA"`) so stacked
#' analyte/specimen tables are distinguishable.  `"all"` is the sentinel used by
#' `split_and_apply()` for un-split single tables and gets no header.
#'
#' @param output             Module `output` object.
#' @param current_page_items Reactive returning the (named) items shown on the
#'                           current page.
#' @noRd
render_table_outputs <- function(output, current_page_items) {
  output$tlg_output <- renderUI({
    items <- current_page_items()
    nms <- names(items)
    tagList(lapply(seq_along(items), function(i) {
      df <- items[[i]]
      body <- if (!is.data.frame(df)) {
        tags$pre(as.character(df))
      } else if (ncol(df) == 0) {
        tags$p("No data available for this table.")
      } else {
        reactable::reactable(
          df,
          columns = define_cols(df, header_from_label = TRUE),
          columnGroups = define_col_groups(df)
        )
      }
      nm <- nms[i]
      if (!is.null(nm) && nzchar(nm) && nm != "all") {
        tagList(tags$h4(nm, class = "tlg-table-group-header"), body)
      } else {
        body
      }
    }))
  })
}

#' Function generating UI for a TLG module.
#'
#' @param id      id of the module, preferably with randomly generated part to avoid conflicts
#' @param type    type of the module, either "graph" or "listing", decides the rendering funciton
#' @param options list of options to customize input parameters
tlg_module_ui <- function(id, type, options) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      position = "right",
      div(
        class = "tlg-options-container",
        dropdown(
          div(
            tags$h2(glue::glue("{type} options")),
            tags$p(glue::glue("
              You can specify any {type} customization options that are supported by the specific
              {type} implementation function.
            ")),
            tags$p(glue::glue(
              "Leaving a widget empty will allow default behaviour of the {type} function."
            )),
            tags$p(
              "In text fields, you can reference values / columns in the dataset by using
              the dollar sign (", tags$b("$"), ") and providing column name, e.g. ",
              tags$b("$DOSEU"), "."
            ),
            tags$p("
              You can also reference ", tags$i("label"), " attribute of any column by prefacing the
              column name by exclamation mark (", tags$b("!"), "), e.g. ", tags$b("!DOSEU"), ".
            ")
          ),
          style = "unite",
          right = TRUE,
          icon = icon("question"),
          status = "primary"
        ),
        actionButton(
          inputId = ns("reset_widgets"),
          label = "Reset to defaults"
        ),
        uiOutput(ns("options"), class = "tlg-options-container")
      )
    ),
    div(
      class = "tlg-widgets-container",
      div(
        align = "left",
        tags$span(
          class = "inline-select-input",
          style = "margin-right: 5em;",
          tags$span("Entries per page:"),
          selectInput(
            ns("entries_per_page"),
            "",
            choices = c("All", 1, 2, 4, 6, 8, 10),
            selected = 1
          )
        ),
        shinyjs::disabled(actionButton(ns("previous_page"), "Previous Page", class = "btn-page"))
      ),
      div(
        align = "center",
        tags$span(
          class = "inline-select-input",
          tags$span("Page "),
          selectInput(
            inputId = ns("select_page"),
            label = "",
            choices = ""
          ),
          tags$span(" out of "),
          uiOutput(ns("page_number"), inline = TRUE),
        )
      ),
      div(align = "right", actionButton(ns("next_page"), "Next Page", class = "btn-page"))
    ),
    shinycssloaders::withSpinner(
      switch(
        type,
        graph   = uiOutput(ns("tlg_output")),
        listing = verbatimTextOutput(ns("tlg_output")),
        table   = uiOutput(ns("tlg_output"))
      )
    )
  )
}

#' Function generating a server function for a TLG module.
#' @param id          id of the module, preferably with randomly generated part to avoid conflicts
#' @param data        adnca data object used for processing the TLG
#' @param type        type of the module, either "graph" or "listing",
#'                    decides the rendering funciton
#' @param render_list function that renders the list of entries, actual implementation of the TLG
#' @param options     list of options to customize input parameters
#'
tlg_module_server <- function(id, data, type, render_list, options = NULL) { # nolint: cyclocomp_linter
  moduleServer(id, function(input, output, session) {
    current_page <- reactiveVal(1)

    #' updating current page based on user input
    observeEvent(input$next_page, current_page(current_page() + 1))
    observeEvent(input$previous_page, current_page(current_page() - 1))
    observeEvent(input$select_page, {
      if (input$select_page == "") return(NULL)
      current_page(as.numeric(input$select_page))
    })

    #' hold reactive information about the page layout
    num_pages <- reactive({
      req(tlg_list(), entries_per_page())
      ceiling(length(tlg_list()) / entries_per_page())
    })

    entries_per_page <- reactive({
      if (is.null(input$entries_per_page)) return(NULL)
      if (is.null(tlg_list())) return(NULL)
      if (input$entries_per_page == "All") {
        length(tlg_list())
      } else {
        as.numeric(input$entries_per_page)
      }
    })

    #' updates UI responsible for page change
    observeEvent(list(current_page(), num_pages()), {
      req(num_pages(), current_page())
      shinyjs::toggleState(id = "previous_page", condition = current_page() > 1)
      shinyjs::toggleState(
        id = "next_page",
        condition = num_pages() != 1 && current_page() < num_pages()
      )
      updateSelectInput(session = session, inputId = "select_page", selected = current_page())
    })
    observeEvent(entries_per_page(), {
      req(num_pages(), entries_per_page())
      current_page(1)

      output$page_number <- renderUI(paste0(num_pages(), "."))
      updateSelectInput(inputId = "select_page", choices = seq_len(num_pages()))
    })

    #' keeps list of plots to render, with options gathered from the UI and applied
    tlg_list <- reactive({
      list_options <- purrr::imap(
        reactiveValuesToList(options_values), function(value, name) value()
      )

      if (any(sapply(list_options, is.null))) return(NULL)

      list_options <- purrr::keep(list_options, function(value) all(!value %in% c(NULL, "", 0, NA)))

      tryCatch({
        # Data arrives already exclusion-filtered (per-dataset flag) and
        # label-restored from the tab_tlg boundary (see tlg_data_sources), so
        # it is passed straight through here.  Label restoration matters because
        # the PKNCA/dplyr pipeline strips column `label` attributes, which breaks
        # the `!COLUMN` label-reference syntax in title/subtitle/footnote/axis
        # inputs (resolved via parse_annotation).
        do.call(render_list, purrr::list_modify(list(data = data()), !!!list_options))
      },
      error = function(e) {
        log_error("Error in list rendering:")
        print(e)
        paste0("Error: ", conditionMessage(e))
      })
    }) %>%
      debounce(750)

    #' raw entries shown on the current page (slice of the full TLG list); the
    #' per-type render helpers below turn them into reactables / plotly / prints
    current_page_items <- reactive({
      req(tlg_list(), entries_per_page(), current_page())

      num_plots <- length(tlg_list())
      page_end <- current_page() * entries_per_page()
      page_start <- page_end - entries_per_page() + 1
      if (page_end > num_plots) page_end <- num_plots

      tlg_list()[page_start:page_end]
    })

    if (type == "graph") {
      render_graph_outputs(output, session, current_page_items)
    } else if (type == "table") {
      render_table_outputs(output, current_page_items)
    } else {
      output$tlg_output <- renderPrint({
        for (item in current_page_items()) print(item)
      })
    }

    options_values <- lapply(names(options), function(option) {
      if (is.character(options[[option]])) return(NULL)
      fn <- get(glue::glue("tlg_option_{options[[option]]$type}_server"))
      fn(option, options[[option]], data, reactive(input$reset_widgets))
    }) %>%
      setNames(names(options)) %>%
      purrr::keep(\(x) !is.null(x)) %>%
      do.call(reactiveValues, .)

    #' creates widgets responsible for custimizing the plots
    output$options <- renderUI({
      purrr::imap(options, function(def, id) .tlg_module_edit_widget(session$ns(id), def, data))
    })
  })
}

#' Creates editing widget of appropriate type.
#' @param opt_def Definition of the option
#' @param opt_id  Id of the option
#' @param session Session object for namespacing the widgets
#' @returns Shiny widget with appropriate type, label and options
.tlg_module_edit_widget <- function(opt_id, opt_def, data) {
  if (grepl(".group_label", opt_id)) {
    return(tags$h1(opt_def, class = "tlg-group-label"))
  }
  ui_fn <- get(glue::glue("tlg_option_{opt_def$type}_ui"))
  ui_fn(opt_id, opt_def, data)
}
