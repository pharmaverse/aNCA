#' Needed to properly reset reactable.extras widgets
#'
#' @details
#' When the data underlying the `reactable` instance is changed (e.g. rows are deleted), and
#' the reactable is re-rendered, the `reactable.extras` widgets still hold their values. This
#' leads to incorrect display, e.g. after removing a middle row, now widgets for removed row 2
#' will be displayed over what used to be row 3. After significanlty changing the data under
#' `reactable`, the memory of `reactable.extras` needs to be reset.
reset_reactable_memory <- function() {
  shinyjs::runjs("memory = {};")
}