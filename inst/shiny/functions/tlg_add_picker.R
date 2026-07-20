#' Helpers for the "Add TLGs to order" modal (issue #1335).
#'
#' The picker was originally a grouped reactable whose Type/Dataset columns were
#' blank on every selectable row.  It is rebuilt as a catalog-style checklist:
#' dataset tabs (PK Concentrations / PK Parameters) over one column per output
#' Type (Tables / Listings / Graphs), with a search + download + select-all
#' toolbar, per-column select-all, and a live count on the confirm button.
#'
#' Client-side behaviour lives in `inst/shiny/www/tlg_add_picker.js`
#' (`window.tlgAdd`); styling lives in
#' `inst/shiny/www/styles/partials/_tlg_add_modal.scss`.  These helpers only
#' build the server-side UI and translate the checked rows back to `tlg_order()`
#' ids, keeping `tab_tlg_server()` small enough to stay under the cyclomatic
#' complexity limit.

# Fixed left-to-right order + icon for the Type columns, and dataset tab order.
.TLG_TYPE_ORDER <- c("Table", "Listing", "Graph")
.TLG_TYPE_ICON  <- c(Table = "table", Listing = "list-ul", Graph = "chart-line")
.TLG_DATASET_ORDER <- c("PK Concentrations", "PK Parameters")

#' Escape a string for safe embedding inside a single-quoted JS literal.
#' @param x Character vector.
#' @returns Character vector wrapped in single quotes with `'` and `\` escaped.
#' @noRd
tlg_js_str <- function(x) paste0("'", gsub("(['\\\\])", "\\\\\\1", x), "'")

#' Build the "Add TLGs" catalog checklist UI.
#'
#' @param avail Tibble of not-yet-selected TLGs (rows of `tlg_order()` with
#'   `Selection == FALSE`); must contain `Type`, `Dataset`, `Description`,
#'   `Link`, and `id`.
#' @param ns Namespace function for the calling module (`session$ns`).
#' @returns A list with `ui` (the modal body tag) and `group_ids` (the
#'   `checkboxGroupInput` ids created, read back on confirm).
#' @noRd
build_add_checklist <- function(avail, ns) {
  present_types <- intersect(.TLG_TYPE_ORDER, unique(avail$Type))
  datasets <- c(intersect(.TLG_DATASET_ORDER, unique(avail$Dataset)),
                setdiff(unique(avail$Dataset), .TLG_DATASET_ORDER))

  pairs <- dplyr::distinct(avail, Type, Dataset)
  pairs <- pairs[order(match(pairs$Type, .TLG_TYPE_ORDER), pairs$Dataset), ]
  pairs$input_id <- paste0("modal_check_", seq_len(nrow(pairs)))

  spec_icon_html <- as.character(icon("circle-info"))

  # One dataset block for a Type column: tagged with data-dataset so the tab bar
  # can show/hide it.  The active tab is applied client-side.
  build_group_ui <- function(type, dataset, input_id) {
    rows <- dplyr::filter(avail, Type == !!type, Dataset == !!dataset)
    choice_names <- purrr::map2(rows$Description, rows$Link, function(desc, link) {
      spec_link <- if (is.na(link)) "" else paste0(
        "<a href='", link, "' target='_blank' onclick='event.stopPropagation()' ",
        "class='tlg-spec' title='View spec'>", spec_icon_html, "</a>"
      )
      HTML(paste0("<span class='tlg-desc'>", htmltools::htmlEscape(desc), "</span>", spec_link))
    })
    div(
      class = "tlg-ds", `data-dataset` = dataset,
      checkboxGroupInput(
        inputId = ns(input_id),
        label = NULL,
        choiceNames = choice_names,
        choiceValues = as.character(rows$id)
      )
    )
  }

  # One flex column per Type (plain flex, not the bootstrap grid, whose negative
  # row margins would misalign the columns against the toolbar).
  type_columns <- purrr::map(present_types, function(tp) {
    tp_pairs <- dplyr::rename(pairs[pairs$Type == tp, ], type = Type, dataset = Dataset)
    div(
      class = "tlg-col",
      div(
        class = "tlg-col-head",
        tags$span(
          class = "tlg-col-title",
          icon(.TLG_TYPE_ICON[[tp]]), paste0(" ", tp, "s"),
          tags$span(sum(avail$Type == tp), class = "tlg-col-count")
        ),
        tags$button(
          type = "button", class = "tlg-col-selall",
          onclick = "window.tlgAdd.colSelect(this)", "Select all"
        )
      ),
      div(
        class = "tlg-col-body",
        purrr::pmap(tp_pairs, build_group_ui),
        div(class = "tlg-col-empty", "None in this view", style = "display: none;")
      )
    )
  })

  # Dataset tab bar; first dataset active by default.  data-total feeds the count
  # badge (restored when the search box is cleared).
  tab_bar <- div(
    class = "tlg-tabs",
    purrr::imap(datasets, function(ds, i) {
      ds_total <- sum(avail$Dataset == ds)
      tags$button(
        type = "button",
        class = paste("tlg-tab", if (i == 1) "active" else ""),
        `data-dataset` = ds, `data-total` = ds_total,
        onclick = paste0("window.tlgAdd.setTab(", tlg_js_str(ds), ", this)"),
        ds, tags$span(ds_total, class = "tlg-tab-count")
      )
    })
  )

  # Per-open initialisation: pick the first dataset tab and render.  Kept inline
  # (not in tlg_add_picker.js) because it depends on the datasets present now.
  init_js <- paste0(
    "window.tlgAdd.tab = ", tlg_js_str(datasets[1]), "; ",
    "window.tlgAdd.q = ''; window.tlgAdd.render();"
  )

  # Shared left inset so toolbar, tabs, column headers and checkbox rows all line
  # up on the same left edge (see --tlg-inset in _tlg_add_modal.scss).
  ui <- div(
    class = "tlg-add-modal",
    div(
      class = "tlg-toolbar",
      tags$input(
        type = "text", class = "form-control tlg-search-input",
        placeholder = "Search outputs…",
        oninput = "window.tlgAdd.setQuery(this.value)"
      ),
      tags$button(type = "button", class = "btn btn-sm btn-default",
                  onclick = "window.tlgAdd.selectAll()", "Select all"),
      tags$button(type = "button", class = "btn btn-sm btn-default",
                  onclick = "window.tlgAdd.clearAll()", "Clear all"),
      div(class = "tlg-toolbar-sep"),
      downloadButton(ns("modal_dl_csv"), "CSV", class = "btn-sm btn-default"),
      downloadButton(ns("modal_dl_xlsx"), "XLSX", class = "btn-sm btn-default")
    ),
    tab_bar,
    div(class = "tlg-add-checklist tlg-cols", type_columns),
    div(class = "tlg-no-matches", "No outputs match your search.", style = "display: none;"),
    tags$script(HTML(init_js))
  )

  list(ui = ui, group_ids = pairs$input_id)
}

#' Ids of the rows checked in the add-picker modal, mapped back to `tlg_order()`.
#'
#' @param input The module `input` object.
#' @param group_ids Character vector of `checkboxGroupInput` ids in the modal.
#' @returns Integer vector of checked `tlg_order()` ids (empty if none).
#' @noRd
checked_tlg_ids <- function(input, group_ids) {
  as.integer(unlist(lapply(group_ids, function(gid) input[[gid]])))
}

#' Available-TLG catalog for the modal's CSV / XLSX download.
#'
#' @param df The available-TLG tibble (`modal_avail()`), or `NULL`.
#' @returns A data frame with `Type`, `Dataset`, `PKid`, `Description`.
#' @noRd
tlg_modal_dl_data <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(Type = character(), Dataset = character(),
                      PKid = character(), Description = character()))
  }
  dplyr::select(df, Type, Dataset, PKid, Description)
}
