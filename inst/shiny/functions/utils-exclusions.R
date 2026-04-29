# Exclusion color constants
EXCL_COLOR_NCA  <- "#FFCCCC"  # red — NCA exclusion
EXCL_COLOR_TLG  <- "#FFFFCC"  # yellow — TLG exclusion only
EXCL_COLOR_BOTH <- "#FFD9B3"  # orange — NCA + TLG

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

#' Check whether a row has manual NCA or TLG exclusions.
#' @param index Row index.
#' @param exclusion_lst List of exclusion entries.
#' @returns Named logical list with `nca` and `tlg`.
#' @noRd
.row_exclusion_flags <- function(index, exclusion_lst) {
  nca <- FALSE
  tlg <- FALSE
  for (excl in exclusion_lst) {
    if (index %in% excl$rows) {
      if (isTRUE(excl$exclude_nca)) nca <- TRUE
      if (isTRUE(excl$exclude_tlg)) tlg <- TRUE
    }
  }
  list(nca = nca, tlg = tlg)
}

#' Determine the background color for a row based on exclusion state.
#' @param index Row index in the table.
#' @param data The data frame rendered in the reactable.
#' @param exclusion_lst Current list of exclusion entries.
#' @returns A list with `background` style or NULL.
#' @noRd
.exclusion_row_color <- function(index, data, exclusion_lst) {
  flags <- .row_exclusion_flags(index, exclusion_lst)

  # Default NCA exclusion from data counts as NCA exclusion
  has_default <- !is.null(data[index, ]$nca_exclude) &&
    nzchar(data[index, ]$nca_exclude)
  is_nca <- flags$nca || has_default

  color <- if (is_nca && flags$tlg) {
    EXCL_COLOR_BOTH
  } else if (is_nca) {
    EXCL_COLOR_NCA
  } else if (flags$tlg) {
    EXCL_COLOR_TLG
  } else {
    NULL
  }

  if (!is.null(color)) list(background = color) else NULL
}

#' Handle adding a new exclusion entry from UI inputs.
#' @param input Shiny input object.
#' @param session Shiny session object.
#' @param exclusion_list reactiveVal holding the exclusion list.
#' @param xbtn_counter reactiveVal holding the button counter.
#' @param table_id ID of the reactable to read selection from.
#' @noRd
.handle_add_exclusion <- function(input, session, exclusion_list, xbtn_counter,
                                  table_id = "conc_table-table") {
  rows_sel <- getReactableState(table_id, "selected")
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
    updateTextInput(session, "exclusion_reason", value = "")
    updateReactable(table_id, selected = NA)

    type_label <- .exclusion_type_label(nca_checked, tlg_checked)
    log_info(
      "Exclusion added: ", length(rows_sel), " rows, type=", type_label,
      ", reason='", reason, "'"
    )
  }
}

#' Render the exclusion list as a reactable.
#' @param lst List of exclusion entries.
#' @param ns Shiny namespace function.
#' @returns A reactable widget or NULL if empty.
#' @noRd
.render_exclusion_list_table <- function(lst, ns) {
  if (length(lst) == 0) return(NULL)

  df <- data.frame(
    Rows = vapply(lst, function(item) {
      paste(item$rows, collapse = ", ")
    }, character(1)),
    Reason = vapply(lst, "[[", character(1), "reason"),
    Type = vapply(lst, function(item) {
      .exclusion_type_label(item$exclude_nca, item$exclude_tlg)
    }, character(1)),
    xbtn_id = vapply(lst, "[[", character(1), "xbtn_id"),
    stringsAsFactors = FALSE
  )

  reactable::reactable(
    df[, c("Rows", "Reason", "Type", "xbtn_id")],
    compact = TRUE,
    bordered = TRUE,
    highlight = TRUE,
    pagination = FALSE,
    defaultPageSize = nrow(df),
    theme = reactable::reactableTheme(
      headerStyle = list(background = "#e9e9e9")
    ),
    columns = list(
      Rows = reactable::colDef(name = "Rows"),
      Reason = reactable::colDef(name = "Reason"),
      Type = reactable::colDef(name = "Type"),
      xbtn_id = reactable::colDef(
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
              style = "padding:2px 6px;"
            )
          )
        },
        html = TRUE
      )
    )
  )
}

#' Format exclusion type as a readable label
#' @noRd
.exclusion_type_label <- function(nca, tlg) {
  parts <- c()
  if (isTRUE(nca)) parts <- c(parts, "NCA")
  if (isTRUE(tlg)) parts <- c(parts, "TLG")
  paste(parts, collapse = " + ")
}
