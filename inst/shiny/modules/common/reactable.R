#' Custom reactable module for extending functionalities of base reactable,
#' mainly focused on providing common layout of the reactables across the app
#' and ability to download data from the table directly.
#'
#' @param id ID of the module.
#' @param data Reactive containing data to be displayed.
#' @param download_buttons Character vector containing file extensions to be available for the user
#'                         to download. Currently available: `csv`, `xlsx`.
#' @param file_name Character string with file name, or `reactive()` or callback function that
#'                  returns character string.
#' @param on_render JavaScript code to be executed on the table after it is rendered.
#' @param editable Character vector with names of columns that should be editable.
#' @param edit_debounce Time in milliseconds to debounce the edit events. Default is 750ms.
#' @param ... Any other parameters to be passed to `reactable()` call. Can be simple values,
#'            reactives or callback functions accepting the `data()` as argument.
reactable_ui <- function(id) {
  ns <- NS(id)

  div(
    div(
      class = "d-flex gap-1 justify-content-end mb-2",
      hidden(downloadButton(ns("download_csv"), label = "csv")),
      hidden(downloadButton(ns("download_xlsx"), label = "xlsx"))
    ),
    withSpinner(reactableOutput(ns("table")))
  )
}

reactable_server <- function(
  id, data, download_buttons = c(), file_name = NULL, on_render = NULL, editable = NULL,
  edit_debounce = 750, columns = define_cols, ...
) {
  moduleServer(id, function(input, output, session) {
    default_reactable_opts <- list(
      searchable = TRUE,
      sortable = TRUE,
      highlight = TRUE,
      striped = TRUE,
      wrap = FALSE,
      resizable = TRUE,
      defaultPageSize = 25,
      showPageSizeOptions = TRUE,
      compact = TRUE,
      style = list(fontSize = "0.75em",
                   maxHeight = "68vh"),
      class = "reactable-table",
      columns = columns
    )

    args <- list(...)
    reactable_opts <- c(
      default_reactable_opts[!names(default_reactable_opts) %in% names(args)],
      args
    )

    # Show requested download  buttons
    purrr::walk(download_buttons, function(x) shinyjs::show(paste0("download_", x)))

    # Attach observers for editable columns
    table_edit <- reactiveVal(NULL)
    purrr::walk(editable, function(col) {
      observe({
        table_edit(input[[paste0("edit_", col)]])
      })
    })
    table_edit_debounced <- reactive({
      req(table_edit())
      table_edit()
    }) %>%
      debounce(edit_debounce)

    output$table <- renderReactable({
      req(data())
      labeled_data <- apply_labels(data())
      opts <- lapply(reactable_opts, function(x) {
        if (is.reactive(x))  {
          x()
        } else if (is.function(x)) {
          x(labeled_data)
        } else {
          x
        }
      })

      if (!is.null(editable)) {
        col_defs <- lapply(editable, function(col) {
          col_def <- lapply(opts$columns[[col]], function(x) x) # unpack other existing colDef-s
          col_def$cell <- text_extra(id = session$ns(paste0("edit_", col)))

          do.call(colDef, col_def)
        }) %>%
          setNames(editable)

        if (is.null(opts$columns)) {
          opts$columns <- col_defs
        } else {
          purrr::iwalk(col_defs, function(val, name) {
            opts$columns[[name]] <<- val
          })
        }
      }

      do.call(reactable, c(list(data = labeled_data), opts)) %>%
        htmlwidgets::onRender(on_render)
    })

    output$download_csv <- downloadHandler(
      filename = .reactable_file_name(file_name, "csv", id),
      content = function(con) write.csv(labeled_data, con, row.names = FALSE)
    )

    output$download_xlsx <- downloadHandler(
      filename = .reactable_file_name(file_name, "xlsx", id),
      content = function(con) openxlsx2::write_xlsx(labeled_data, con)
    )

    reactive(
      list(
        selected = getReactableState("table", "selected"),
        edit = table_edit_debounced
      )
    ) %>%
      invisible()
  })
}

# Creates file name for export
.reactable_file_name <- function(file_name, ext, id = NULL) {
  f_name <- {
    if (is.character(file_name)) {
      file_name
    } else if (is.reactive(file_name) || is.function(file_name)) {
      file_name()
    } else {
      paste0(Sys.Date(), "_", id)
    }
  }
  paste0(f_name, ".", ext)
}

#' Define Column Definitions for PK/PD reactable Tables
#'
#' This function generates column definitions for a reactable table by merging
#' label-based tooltips with dynamic width estimation based on character length.
#'
#' @param data A data frame containing the data to be displayed in the reactable table.
#' @param max_px Integer. Maximum allowable base width in pixels. Default is 150.
#' @param expand_factor Integer. Multiplier to convert character count to pixels. Default is 8.
#' @param overrides A named list of [reactable::colDef()] objects to override defaults.
#'
#' @return A named list of [reactable::colDef()] objects.
#'
#' @importFrom reactable colDef
#' @importFrom purrr imap
#' @importFrom utils modifyList
#' @importFrom htmltools tags
#'
#' @examples
#' library(reactable)
#' # Prepare data with CDISC labels
#' adpc <- data.frame(
#'   USUBJID = c("STUDY-001-001", "STUDY-001-002"),
#'   AVAL    = c(15.2, 0.45),
#'   PARAMCD = c("DRUG_CONC", "DRUG_CONC")
#' )
#' attr(adpc$USUBJID, "label") <- "Unique Subject Identifier"
#' attr(adpc$AVAL, "label")    <- "Analysis Value (ng/mL)"
#' # Generate definitions with custom overrides
#' col_defs <- define_cols(
#'   data = adpc,
#'   overrides = list(
#'     AVAL = colDef(format = colFormat(digits = 2))
#'   )
#' )
#' # Render table
#' if (interactive()) {
#'   reactable(adpc, columns = col_defs)
#' }
define_cols <- function(data, max_px = 150, expand_factor = 8, overrides = list()) {
  if (is.null(data)) {
    return(NULL)
  }
  defs <- purrr::imap(data, \(values, col_name) {
    # Define width based on max character length
    max_char <- max(nchar(as.character(values)), nchar(col_name), na.rm = TRUE)
    calc_width <- max_char * expand_factor + 20
    # Label for tooltip
    label <- unname(attr(values, "label"))
    min_width <-  min(calc_width, max_px)
    max_width <- 2 * max_px
  
    if (!is.null(label)) {
      reactable::colDef(
        html = TRUE,
        header = htmltools::tags$span(
          col_name,
          `data-toggle` = "tooltip",
          `data-placement` = "top",
          title = label
        ),
        minWidth = min_width,
        maxWidth = max_width,
        resizable = TRUE
      )
    } else {
      reactable::colDef(
        name = col_name,
        minWidth = min_width,
        maxWidth = max_width,
        resizable = TRUE
      )
    }
  })
  if (length(overrides) > 0) {
    defs <- utils::modifyList(defs, overrides)
  }
  defs
}
