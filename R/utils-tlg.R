#' Parses annotations in the context of data. Special characters and syntax are substituted by
#' actual data and/or substituted for format that is better parsed via rendering functions
#' (e.g. plotly).
#'
#' @details
#' * `\n` character is substituted for `<br>` tag in order to add new lines in redered image.
#' * `$COLNAME` is parsed to privde unique data value from the mentioned column.
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
#' @importFrom glue glue
#'
#' @export
parse_annotation <- function(data, text) {
  text %>%
    gsub("\n", "<br>", .) %>%
    gsub("\\$(\\w+)", "{unique(data[['\\1']])}", .) %>%
    gsub("!(\\w+)", "{attr(data[['\\1']], 'label')}", .) %>%
    glue::glue(.na = "ERR", .null = "ERR")
}
