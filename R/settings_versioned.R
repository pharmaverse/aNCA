#' Create a versioned settings entry
#'
#' Wraps a settings payload with metadata (timestamp, comment, dataset name,
#' aNCA version, active tab) for storage in a versioned settings YAML file.
#'
#' @param settings_data List with the settings payload (mapping, parameters,
#'   NCA setup, filters, slope_rules, etc.).
#' @param comment Optional character string with a user comment.
#' @param dataset Optional character string identifying the dataset used.
#' @param tab Optional character string with the active app tab.
#'
#' @returns A named list representing one version entry.
#' @export
create_settings_version <- function(settings_data,
                                    comment = "",
                                    dataset = "",
                                    tab = "") {
  list(
    comment = comment,
    datetime = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    dataset = dataset,
    anca_version = as.character(utils::packageVersion("aNCA")),
    tab = tab,
    settings = settings_data
  )
}

#' Read a versioned settings YAML file
#'
#' Parses a YAML file that may contain either the legacy flat format
#' (with a top-level `settings` key) or the versioned format (with
#' `current` and optionally `previous` keys). Returns a normalised
#' list with `versions` (a list of version entries, most recent first)
#' and `format` (`"versioned"` or `"legacy"`).
#'
#' @param path Character string with path to the YAML file.
#'
#' @returns A list with elements `versions` and `format`.
#'
#' @importFrom yaml read_yaml
#' @export
read_versioned_settings <- function(path) {
  obj <- yaml::read_yaml(path)

  if ("current" %in% names(obj)) {
    # Versioned format
    versions <- list()
    versions[[1]] <- .parse_version_entry(obj$current)
    if (!is.null(obj$previous) && is.list(obj$previous)) {
      for (entry in obj$previous) {
        versions[[length(versions) + 1]] <- .parse_version_entry(entry)
      }
    }
    # Sort by timestamp descending (newest first) to handle manual edits
    timestamps <- vapply(versions, function(v) v$datetime %||% "", character(1))
    ord <- order(timestamps, decreasing = TRUE)
    versions <- versions[ord]

    list(versions = versions, format = "versioned")
  } else if ("settings" %in% names(obj)) {
    # Legacy flat format — wrap as a single version
    parsed <- .parse_legacy_settings(obj)
    version_entry <- list(
      comment = "",
      datetime = "",
      dataset = "",
      anca_version = "",
      tab = "",
      settings = parsed
    )
    list(versions = list(version_entry), format = "legacy")
  } else {
    stop(
      "The file does not appear to be a valid settings YAML file. ",
      "Expected top-level 'current' or 'settings' key."
    )
  }
}

#' Write a versioned settings YAML file
#'
#' Writes a list of version entries to a YAML file in the versioned
#' format. The first entry becomes `current`, the rest go under
#' `previous`.
#'
#' @param versions A list of version entries (as returned by
#'   [create_settings_version()]).
#' @param path Character string with the output file path.
#'
#' @returns Invisibly returns `path`.
#'
#' @importFrom yaml write_yaml
#' @export
write_versioned_settings <- function(versions, path) {
  if (length(versions) == 0) {
    stop("At least one version entry is required.")
  }

  out <- list(current = versions[[1]])
  if (length(versions) > 1) {
    out$previous <- versions[2:length(versions)]
  }

  yaml::write_yaml(out, path)
  invisible(path)
}

#' Add a new version to an existing versioned settings list
#'
#' Prepends a new version entry so it becomes the current version.
#' The previous current entry moves to the `previous` list.
#'
#' @param versions Existing list of version entries.
#' @param new_version A single version entry (from
#'   [create_settings_version()]).
#'
#' @returns Updated list of version entries with the new entry first.
#' @export
add_settings_version <- function(versions, new_version) {
  c(list(new_version), versions)
}

#' Delete a version from a versioned settings list
#'
#' Removes the version at the given index. Cannot delete the last
#' remaining version.
#'
#' @param versions List of version entries.
#' @param index Integer index of the version to remove.
#'
#' @returns Updated list of version entries.
#' @export
delete_settings_version <- function(versions, index) {
  if (length(versions) <= 1) {
    stop("Cannot delete the last remaining version.")
  }
  if (index < 1 || index > length(versions)) {
    stop("Index out of bounds: ", index)
  }
  versions[-index]
}

#' Build a summary table of version entries
#'
#' Creates a data.frame with one row per version, suitable for
#' display in a modal.
#'
#' @param versions List of version entries.
#'
#' @returns A data.frame with columns: `index`, `comment`, `datetime`,
#'   `dataset`, `anca_version`, `tab`.
#' @export
settings_version_summary <- function(versions) {
  rows <- lapply(seq_along(versions), function(i) {
    v <- versions[[i]]
    data.frame(
      index = i,
      comment = v$comment %||% "",
      datetime = v$datetime %||% "",
      dataset = v$dataset %||% "",
      anca_version = v$anca_version %||% "",
      tab = v$tab %||% "",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Extract the settings payload from a version entry
#'
#' Applies the same post-processing as the legacy `read_settings()`
#' (converting YAML lists to data.frames, etc.).
#'
#' @param version A single version entry.
#'
#' @returns A list with the settings payload ready for use.
#' @export
extract_version_settings <- function(version) {
  s <- version$settings
  if (is.null(s) || length(s) == 0) {
    warning("Version entry has empty settings content.")
    return(NULL)
  }

  # Backward compat: old format nests NCA fields under s$settings;
  # new flat format has them directly on s alongside slope_rules/filters.
  if (is.null(s$settings)) {
    meta_keys <- c("mapping", "slope_rules", "filters")
    nca_fields <- s[setdiff(names(s), meta_keys)]
    s <- list(
      settings = nca_fields,
      mapping = s$mapping,
      slope_rules = s$slope_rules,
      filters = s$filters
    )
  }

  s$slope_rules <- .convert_list_to_df(s$slope_rules)
  s$settings$units <- .convert_list_to_df(s$settings$units)
  s$settings$int_parameters <- .convert_list_to_df(s$settings$int_parameters)
  s$filters <- .convert_filter_values(s$filters)

  s
}

# Internal helpers --------------------------------------------------------

#' Parse a single version entry from YAML, filling in missing metadata.
#' @param entry A list from YAML.
#' @returns A normalised version entry list.
#' @noRd
.parse_version_entry <- function(entry) {
  list(
    comment = entry$comment %||% "",
    datetime = entry$datetime %||% "",
    dataset = entry$dataset %||% "",
    anca_version = entry$anca_version %||% "",
    tab = entry$tab %||% "",
    settings = entry$settings %||% list()
  )
}

#' Convert legacy flat settings to the payload format used inside
#' version entries.
#' @param obj The raw YAML list with top-level `settings`, `slope_rules`,
#'   `filters`.
#' @returns A list matching the `settings` field of a version entry.
#' @noRd
.parse_legacy_settings <- function(obj) {
  c(
    obj$settings,
    list(
      slope_rules = obj$slope_rules,
      filters = obj$filters
    )
  )
}
