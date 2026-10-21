# Shared settings contract for TLG order metadata, rendered labels and sidebar options.
# Catalog keys and option IDs are stable identifiers; row positions are never saved.

# Only user-editable order fields are portable; catalog metadata and row IDs
# always come from the current catalog.
.TLG_ORDER_FIELDS <- c("Selection", "Footnote", "Stratification", "Comment")

.valid_tlg_order_value <- function(value, field) {
  if (field == "Selection") {
    return(is.logical(value) && length(value) == 1L && !is.na(value))
  }
  is.null(value) ||
    (is.atomic(value) && length(value) == 1L && (is.character(value) || is.na(value)))
}

.normalize_tlg_order_row <- function(entry, key) {
  if (!is.list(entry) || is.data.frame(entry)) {
    warning("Ignored invalid TLG order row: ", key, call. = FALSE)
    return(list())
  }
  result <- list()
  for (field in intersect(.TLG_ORDER_FIELDS, names(entry))) {
    value <- entry[[field]]
    if (!.valid_tlg_order_value(value, field)) {
      warning("Ignored invalid TLG order field: ", key, "/", field, call. = FALSE)
      next
    }
    result[field] <- list(if (is.null(value)) NA_character_ else value)
  }
  result
}

#' Validate saved order fields, matching stable catalog keys rather than row IDs.
#' @param saved Named list of saved TLG order rows, or NULL for older settings.
#' @param catalog_ids Names of the current catalog entries.
#' @returns A named list of supported, valid order fields.
#' @noRd
.normalize_tlg_order <- function(saved, catalog_ids) {
  if (is.null(saved) || length(saved) == 0L) return(list())
  ids <- names(saved)
  valid <- c(
    is.list(saved), !is.data.frame(saved), !is.null(ids),
    !anyNA(ids), all(nzchar(ids)), !anyDuplicated(ids)
  )
  if (!all(valid)) {
    warning("Ignored invalid TLG order settings: expected unique catalog names.", call. = FALSE)
    return(list())
  }
  missing <- setdiff(ids, catalog_ids)
  if (length(missing) > 0L) {
    warning(
      "Skipped TLGs no longer in the catalog: ", paste(missing, collapse = ", "), call. = FALSE
    )
  }
  ids <- intersect(ids, catalog_ids)
  setNames(lapply(ids, function(key) .normalize_tlg_order_row(saved[[key]], key)), ids)
}

.restore_tlg_order <- function(default_order, saved, catalog_ids) {
  for (key in names(saved)) {
    i <- match(key, catalog_ids)
    for (field in names(saved[[key]])) default_order[[field]][i] <- saved[[key]][[field]]
  }
  default_order
}

#' Normalize non-text TLG options from either YAML or an in-memory snapshot.
#'
#' An absent option uses its catalog default. Empty selections and zero-row tables
#' are explicit values, including YAML's empty sequence representation (`list()`).
#' @param saved Named map from the `tlg_options` settings key.
#' @param definitions Current resolved TLG catalog.
#' @returns Supported values keyed by catalog name and option ID.
#' @noRd
.normalize_tlg_options <- function(saved, definitions) {
  if (is.null(saved) || identical(saved, list())) return(list())
  if (!.tlg_label_map_valid(saved)) {
    warning("Ignored invalid TLG options: expected unique catalog names.", call. = FALSE)
    return(list())
  }
  unknown <- setdiff(names(saved), names(definitions))
  if (length(unknown)) {
    warning("Skipped TLGs no longer in the catalog: ", paste(unknown, collapse = ", "),
            call. = FALSE)
  }
  result <- list()
  for (key in intersect(names(saved), names(definitions))) {
    values <- .normalize_tlg_option_entry(saved[[key]], definitions[[key]]$options, key)
    if (length(values)) result[[key]] <- values
  }
  result
}

#' @noRd
.normalize_tlg_option_entry <- function(entry, options, key) {
  if (!.tlg_label_map_valid(entry)) {
    warning("Ignored invalid TLG options for ", key, ".", call. = FALSE)
    return(list())
  }
  values <- list()
  for (id in names(entry)) {
    value <- tryCatch(.normalize_tlg_option(entry[[id]], options[[id]]), error = identity)
    if (inherits(value, "error")) {
      warning("Ignored invalid TLG option: ", key, "/", id, ".", call. = FALSE)
    } else {
      values[id] <- list(value)
    }
  }
  values
}

#' @noRd
.normalize_tlg_option <- function(value, definition) {
  if (!is.list(definition)) stop("Unknown option")
  switch(definition$type,
    select = .normalize_tlg_selection(value, definition),
    numeric = .normalize_tlg_number(value),
    table = .normalize_tlg_table(value, definition),
    stop("Unsupported option type")
  )
}

#' @noRd
.normalize_tlg_selection <- function(value, definition) {
  if (is.null(value) || identical(value, list())) return(character())
  valid <- c(is.atomic(value), !anyNA(value), is.character(value) || is.numeric(value))
  if (!all(valid)) stop("Invalid selection")
  if (!isTRUE(definition$multiple) && length(value) > 1L) stop("Multiple selections")
  as.character(value)
}

#' @noRd
.normalize_tlg_number <- function(value) {
  if (is.null(value)) return(NA_real_)
  if (length(value) != 1L || !(is.numeric(value) || is.na(value))) stop("Invalid number")
  as.numeric(value)
}

#' @noRd
.normalize_tlg_table <- function(value, definition) {
  cols <- names(definition$cols)
  if (!is.list(value) || !setequal(names(value), cols)) stop("Invalid table columns")
  value <- lapply(value[cols], function(column) {
    if (identical(column, list())) return(character())
    if (!is.atomic(column)) stop("Invalid table column")
    as.character(column)
  })
  if (length(unique(lengths(value))) > 1L) stop("Unequal table columns")
  as.data.frame(value, stringsAsFactors = FALSE)
}

#' Read one TLG settings snapshot for both standalone and ZIP settings exports.
#' @noRd
.tlg_settings_payload <- function(session) {
  keys <- c("tlg_order", "tlg_labels", "tlg_options")
  setNames(lapply(keys, function(key) {
    value <- session$userData[[key]]
    if (is.function(value)) value() else NULL
  }), keys)
}

#' Collect applied option values, retaining imported values for unopened TLGs.
#' @noRd
.collect_tlg_options <- function(registry, imported) {
  for (key in ls(envir = registry)) {
    current <- get(key, envir = registry)$options()
    if (length(current)) {
      entry <- imported[[key]]
      if (is.null(entry)) entry <- list()
      entry[names(current)] <- current
      imported[[key]] <- entry
    }
  }
  imported
}

#' Keep supported text labels from a settings payload, keyed by catalog name.
#'
#' Unknown catalog entries/options and malformed values cannot replace defaults.
#' Empty strings and annotation tokens are kept verbatim. Older settings omit this key.
#' @param saved Named list from the top-level `tlg_labels` settings key.
#' @param definitions Current catalog definitions.
#' @returns A named list of valid text labels.
#' @noRd
.normalize_tlg_labels <- function(saved, definitions) {
  if (is.null(saved) || identical(saved, list())) return(list())
  if (!.tlg_label_map_valid(saved)) {
    warning("Ignored invalid TLG labels: expected unique catalog names.", call. = FALSE)
    return(list())
  }
  keys <- intersect(names(saved), names(definitions))
  if (length(keys) < length(saved)) {
    warning("Ignored unknown or invalid TLG labels in settings.", call. = FALSE)
  }
  labels <- lapply(keys, function(key) {
    .normalize_tlg_label_entry(saved[[key]], definitions[[key]]$options)
  })
  labels <- setNames(labels, keys)
  labels[lengths(labels) > 0L]
}

#' @noRd
.tlg_label_map_valid <- function(x) {
  all(c(is.list(x), !is.data.frame(x), !is.null(names(x)), !anyNA(names(x)),
        !anyDuplicated(names(x)), all(nzchar(names(x)))))
}

#' @noRd
.normalize_tlg_label_entry <- function(entry, options) {
  if (!.tlg_label_map_valid(entry)) {
    warning("Ignored unknown or invalid TLG labels in settings.", call. = FALSE)
    return(NULL)
  }
  text_ids <- names(options)[vapply(options, function(def) {
    is.list(def) && identical(def$type, "text") && !isTRUE(def$metadata)
  }, logical(1))]
  valid <- vapply(entry, function(value) {
    is.character(value) && length(value) == 1L && !is.na(value)
  }, logical(1)) & names(entry) %in% text_ids
  if (any(!valid)) warning("Ignored unknown or invalid TLG labels in settings.", call. = FALSE)
  entry[valid]
}

#' Collect raw text from registered modules, retaining unopened imported labels.
#' @noRd
.collect_tlg_labels <- function(registry, imported) {
  for (key in ls(envir = registry)) {
    imported[[key]] <- get(key, envir = registry)$labels()
  }
  imported
}


#' Normalize one complete uploaded snapshot against the current catalog.
#' @noRd
.normalize_tlg_settings <- function(saved, definitions) {
  list(
    tlg_order = .normalize_tlg_order(saved$tlg_order, names(definitions)),
    tlg_labels = .normalize_tlg_labels(saved$tlg_labels, definitions),
    tlg_options = .normalize_tlg_options(saved$tlg_options, definitions)
  )
}

#' Collect submitted order metadata and current sidebar comments.
#' @noRd
.collect_tlg_order <- function(order, registry, catalog_ids) {
  for (key in ls(envir = registry)) {
    comment <- get(key, envir = registry)$comment()
    if (!is.null(comment)) order$Comment[match(key, catalog_ids)] <- comment
  }
  rows <- lapply(seq_len(nrow(order)), function(i) as.list(order[i, .TLG_ORDER_FIELDS]))
  setNames(rows, catalog_ids[order$id])
}

#' @noRd
.tlg_order_comment <- function(entry) {
  comment <- entry$Comment
  if (is.null(comment) || is.na(comment)) "" else comment
}
