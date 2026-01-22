#' Calculate Dynamic Column Widths for reactable
#'
#' Estimates optimal column widths based on the maximum character length of
#' content and headers to prevent text clipping in PK/PD datasets.
#'
#' @param data A data frame.
#' @param max_px Integer. Maximum allowable base width in pixels. Default is 150.
#' @param expand_factor Integer. Multiplier to convert character count to pixels. Default is 8.
#'
#' @return A named list of [reactable::colDef()] objects.
#'
#' @importFrom reactable colDef
#' @importFrom shiny req
#'
#' @examples
#' adsl <- data.frame(USUBJID = "STUDY-001-001", AGE = 25)
#' col_defs <- max_nchar_col(adsl)
#'
#' @export

max_nchar_col <- function(data, max_px = 150, expand_factor = 8) {
  shiny::req(data)
  
  purrr::imap(data, ~ {
    max_char <- max(nchar(as.character(.x)), nchar(.y), na.rm = TRUE)
    calc_width <- max_char * expand_factor + 20
    
    reactable::colDef(
      minWidth = min(calc_width, max_px),
      maxWidth = 2 * max_px,
      resizable = TRUE
    )
  })
}