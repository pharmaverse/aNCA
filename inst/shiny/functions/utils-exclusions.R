# Exclusion color constants
EXCL_COLOR_NCA   <- "#FFCCCC"  # red — NCA exclusion
EXCL_COLOR_TLG   <- "#FFFFCC"  # yellow — TLG exclusion only
EXCL_COLOR_BOTH  <- "#FFD9B3"  # orange — NCA + TLG
EXCL_COLOR_PARAM <- "#FFF3CD"  # yellow — parameter exclusion

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
  }
}

#' Format exclusion type as a readable label
#' @noRd
.exclusion_type_label <- function(nca, tlg) {
  parts <- c()
  if (isTRUE(nca)) parts <- c(parts, "NCA")
  if (isTRUE(tlg)) parts <- c(parts, "TLG")
  paste(parts, collapse = " + ")
}

#' Build a reason-per-index vector from an exclusion list.
#' Concatenates with "; " when multiple exclusions cover the same row.
#' @param lst List of exclusion entries (each with $rows and $reason).
#' @param n_rows Total number of rows in the data. Indices beyond this are ignored.
#' @returns A list with `indices` (integer) and `reasons` (character).
#' @noRd
.build_exclusion_reasons <- function(lst, n_rows = NULL) {
  all_indices <- sort(unique(unlist(lapply(lst, function(x) x$rows))))
  if (!is.null(n_rows)) {
    all_indices <- all_indices[all_indices <= n_rows]
  }
  if (length(all_indices) == 0) {
    return(list(indices = integer(0), reasons = character(0)))
  }
  reasons <- rep(NA_character_, max(all_indices))
  for (entry in lst) {
    for (idx in entry$rows) {
      if (idx <= length(reasons)) {
        reasons[idx] <- if (is.na(reasons[idx])) {
          entry$reason
        } else {
          paste(reasons[idx], entry$reason, sep = "; ")
        }
      }
    }
  }
  list(indices = all_indices, reasons = reasons[all_indices])
}

#' Render an exclusion list as a reactable with remove buttons.
#'
#' Shared renderer for both general and parameter exclusion tables.
#' When `show_type = TRUE`, a "Type" column is included (for general exclusions).
#'
#' @param lst List of exclusion entries.
#' @param ns Shiny namespace function.
#' @param show_type Logical. Include a Type column? Default FALSE.
#' @returns A reactable widget or NULL if empty.
#' @noRd
.render_exclusion_table <- function(lst, ns, show_type = FALSE) {
  if (length(lst) == 0) return(NULL)

  df <- data.frame(
    Rows = vapply(lst, function(item) {
      paste(item$rows, collapse = ", ")
    }, character(1)),
    Reason = vapply(lst, "[[", character(1), "reason"),
    xbtn_id = vapply(lst, "[[", character(1), "xbtn_id"),
    stringsAsFactors = FALSE
  )

  cols <- list(
    Rows = reactable::colDef(name = "Rows"),
    Reason = reactable::colDef(name = "Reason"),
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

  display_cols <- c("Rows", "Reason")

  if (show_type) {
    df$Type <- vapply(lst, function(item) {
      .exclusion_type_label(item$exclude_nca, item$exclude_tlg)
    }, character(1))
    cols$Type <- reactable::colDef(name = "Type")
    display_cols <- c(display_cols, "Type")
  }

  display_cols <- c(display_cols, "xbtn_id")

  reactable::reactable(
    df[, display_cols, drop = FALSE],
    compact = TRUE,
    bordered = TRUE,
    highlight = TRUE,
    pagination = FALSE,
    defaultPageSize = nrow(df),
    theme = reactable::reactableTheme(
      headerStyle = list(background = "#e9e9e9")
    ),
    columns = cols
  )
}

#' Register remove-button observers for exclusion list entries.
#'
#' Tracks which buttons already have observers to avoid duplicates.
#' Used by both general and parameter exclusion modules.
#'
#' @param exclusion_list reactiveVal holding the exclusion list.
#' @param registered_ids reactiveVal holding character vector of registered IDs.
#' @param input Shiny input object.
#' @noRd
.register_remove_observers <- function(exclusion_list, registered_ids, input) {
  lst <- exclusion_list()
  already <- registered_ids()
  new_ids <- setdiff(
    vapply(lst, function(x) x$xbtn_id, character(1)),
    already
  )
  for (xbtn_id in new_ids) {
    local({
      local_id <- xbtn_id
      observeEvent(input[[local_id]], {
        current <- exclusion_list()
        exclusion_list(Filter(function(x) x$xbtn_id != local_id, current))
      }, ignoreInit = TRUE, once = TRUE)
    })
  }
  registered_ids(union(already, new_ids))
}

#' Build tagged and filtered views of NCA results based on parameter exclusions.
#'
#' Tagged view adds .pp_excl/.pp_excl_reason markers for CDISC export.
#' Filtered view removes excluded rows for summary tables and plots.
#'
#' @param res NCA result object (list with `$result` data.frame).
#' @param excl_info List with `$indices` and `$reasons` from `.build_exclusion_reasons`.
#' @returns List with `tagged` and `filtered` result objects.
#' @noRd
.apply_param_exclusions <- function(res, excl_info) {
  excl_indices <- excl_info$indices
  excl_reasons <- excl_info$reasons

  tagged_result <- res$result
  n <- nrow(tagged_result)
  tagged_result$.pp_excl <- seq_len(n) %in% excl_indices
  reason_vec <- rep(NA_character_, n)
  if (length(excl_indices) > 0 && length(excl_indices) == length(excl_reasons)) {
    reason_vec[excl_indices] <- excl_reasons
  }
  tagged_result$.pp_excl_reason <- reason_vec

  res_tagged <- res
  res_tagged$result <- tagged_result

  keep <- !tagged_result$.pp_excl
  filtered_result <- tagged_result[keep, , drop = FALSE]
  filtered_result$.pp_excl <- NULL
  filtered_result$.pp_excl_reason <- NULL

  res_filtered <- res
  res_filtered$result <- filtered_result

  list(tagged = res_tagged, filtered = res_filtered)
}
