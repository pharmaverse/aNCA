# Shared utilities for exclusion list management.
# Used by both general_exclusions.R and parameter_exclusions.R.

#' Rehydrate an exclusion override list with fresh button IDs.
#'
#' Uploaded settings store exclusions without xbtn_id. This function
#' assigns new IDs so the remove buttons work in the current session.
#'
#' @param overrides List of exclusion entries (reason + rows, no xbtn_id).
#' @param exclusion_list ReactiveVal holding the current exclusion list.
#' @param xbtn_counter ReactiveVal holding the button ID counter.
#' @param prefix Character prefix for button IDs.
rehydrate_exclusions <- function(overrides, exclusion_list, xbtn_counter, prefix) {
  new_ids <- seq_along(overrides) + xbtn_counter()
  rehydrated <- purrr::map2(overrides, new_ids, function(item, id) {
    item$xbtn_id <- paste0(prefix, id)
    item
  })
  xbtn_counter(max(new_ids))
  exclusion_list(rehydrated)
}

#' Strip xbtn_id from an exclusion list for persistence.
#'
#' @param lst List of exclusion entries (with xbtn_id).
#' @returns List of exclusion entries without xbtn_id.
clean_exclusion_list <- function(lst) {
  lapply(lst, function(x) x[setdiff(names(x), "xbtn_id")])
}

#' Register remove-button observers for exclusion list entries.
#'
#' Each entry's xbtn_id is observed once. When clicked, the entry is
#' removed from the exclusion list. Already-registered IDs are skipped.
#'
#' @param exclusion_list ReactiveVal holding the current exclusion list.
#' @param registered_ids ReactiveVal(character) tracking registered button IDs.
#' @param input Shiny input object.
observe_remove_buttons <- function(exclusion_list, registered_ids, input) {
  observe({
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
  })
}
