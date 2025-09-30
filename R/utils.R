#' Evaluates range notation. If provided notation is invalid, returns NA.
#'
#' @param x character string with range notation, e.g. 1:5.
#' @returns numeric vector with specified range of numbers, NA if notation is invalid
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' .eval_range("1:5") # c(1, 2, 3, 4, 5)
#' .eval_range("5,3:1,15") # c(5, 3, 2, 1, 15)
#' }
#'
.eval_range <- function(x) {
  val_range <- try({
    if (!grepl("^[0-9,:]+$", x)) stop("Error: not a valid range notation.")
    eval(parse(text = paste0("c(", x, ")")))
  }, silent = TRUE)
  if (inherits(val_range, "try-error")) NA else val_range
}

#' Compresses a numeric vector into the simplest possible character string that, when evaluated,
#' will create the same numeric vector.
#'
#' @param range_vector numeric vector with numbers to compress into string
#' @returns simplest possible character string representing provided vector
#'
#' @importFrom methods is
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' .compress_range(c(1, 2, 3, 4)) # "1:4"
#' .compress_range(c(15, 1, 11, 4, 5, 10, 2, 12, 3)) # "1:5,10:12,15"
#' }
#'
.compress_range <- function(range_vector) {
  if (!is(range_vector, "numeric")) range_vector <- suppressWarnings(as.numeric(range_vector))
  if (any(is.na(range_vector))) stop("Error: only numeric values allowed")
  if (length(range_vector) == 0) return(NA_integer_)

  range_vector <- sort(unique(range_vector))
  grouped_range <- c(TRUE, diff(range_vector) != 1) %>%
    cumsum() %>%
    split(range_vector, .)

  subranges <- sapply(grouped_range, \(group) {
    if (length(group) > 1) paste0(group[1], ":", group[length(group)]) else as.character(group)
  })

  paste0(subranges, collapse = ",")
}

#' Generate an Empty Plotly Object
#'
#' This function returns a blank Plotly plot with optional annotation text.
#' It ensures that when no valid data is available, a meaningful placeholder plot is displayed
#' instead of causing an error.
#'
#' @param message A character string specifying the text to display in the center of the empty plot.
#'                Defaults to `"No data available"`.
#'
#' @importFrom plotly plot_ly
#'
#' @returns A Plotly object representing an empty plot with hidden axes.
.plotly_empty_plot <- function(message = "No data available") {
  plot_ly() %>%
    layout(
      title = "",
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(
        text = message,
        x = 0.5,
        y = 0.5,
        showarrow = FALSE,
        font = list(size = 18, color = "red"),
        xref = "paper",
        yref = "paper"
      )
    )
}

#' Parses annotations in the context of data. Special characters and syntax are substituted by
#' actual data and/or substituted for format that is better parsed via rendering functions
#' (e.g. plotly).
#'
#' @details
#' * `\n` character is substituted for `<br>` tag in order to add new lines in rendered image.
#' * `$COLNAME` is parsed to provide unique data value from the mentioned column.
#' * `!COLNAME` is parsed to provide `label` attribute for a given column name.
#' If any values are missing from the provided data, they are substituted for `ERR` string.
#'
#' @param data Data frame containing data to reference. Should include columns and labels referenced
#'             in the text string. Referenced variables should be able to produce single unique
#'             result.
#' @param text Character text to parse.
#' @returns Parsed annotation text.
#'
#' @importFrom magrittr `%>%`
#' @importFrom stringr str_glue
#'
#' @export
parse_annotation <- function(data, text) {
  text %>%
    gsub("\n", "<br>", .) %>%
    gsub("\\$(\\w+)", "{unique(data[['\\1']])}", .) %>%
    gsub("!(\\w+)", "{attr(data[['\\1']], 'label')}", .) %>%
    str_glue(.na = "ERR", .null = "ERR")
}

#' Contatenates a list and data frame objects into formatted string for logging.
#'
#' @details
#' Utilitary function for logging a list object (like mapping list or used settings) in
#' a nice format. Parses a list into nice string and logs at DEBUG level. Can also process
#' data frames, which will be converted into a list of rows.
#'
#' @param title Title for the logs.
#' @param l     List object to be parsed into log. Can also be a data.frame.
#' @returns Character string ready for logging.
#'
#' @importFrom purrr imap
#'
#' @noRd
#' @keywords internal
.concatenate_list <- function(title, l) {
  if (is.data.frame(l))
    l <- split(l, seq_len(nrow(l)))

  log_msg <- imap(l, \(v, n) {
    sep <- ", "
    if (is.list(v)) {
      v <- imap(v, \(v2, n2) paste0("\t* ", n2, " -> ", paste0(v2, collapse = ", "))) |>
        paste0(collapse = "\n") %>%
        paste0("\n", .)
    } else {
      v <- paste0(v, collapse = ", ")
    }
    paste0("* ", n, " -> ", v, collapse = "\n")
  })

  paste0(title, "\n", paste0(log_msg, collapse = "\n"))
}
