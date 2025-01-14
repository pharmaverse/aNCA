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

#' Parses the tlg definitions described in yaml file into a list object.
#' Custom YAML parsing is needed for templating - the user can define templates for a definition.
#' The function detects appropriate keywords and copies information from specified templates.
#'
#' @param path Path to a YAML file containing TLG definitions.
#' @param defs A list of TLG definitions read from a YAML file.
#'
#' @details
#' If both `path` and `defs` are provided, `path` will be used and definitions will be loaded from
#' the file. If neither is given, as a default, the function will load system file in
#' `shiny/tlg.yaml`.
#'
#' @returns A full list of available TLG definitions.
#'
#' @importFrom purrr imap
#' @importFrom yaml read_yaml
#'
#' @export
parse_tlg_definitions <- function(path = NULL, defs = NULL) {
  if (is.null(path) && is.null(defs)) {
    defs <- yaml::read_yaml(system.file("shiny/tlg.yaml", package = "aNCA"))
  }

  if (!is.null(path)) {
    defs <- yaml::read_yaml(path)
  }

  defs <- purrr::imap(defs, \(opt_def, opt_id) {
    if ("template" %in% names(opt_def)) {
      template_def <- defs[[opt_def$template]]

      for (d in names(opt_def)) {
        if (d == "template") next

        if (d == "options") {
          for (o in names(opt_def$options)) {
            template_def$options[[o]] <- opt_def$options[[o]]
          }
        } else {
          template_def[[d]] <- opt_def[[d]]
        }
      }

      opt_def <- template_def
    }

    opt_def
  }) |>
    setNames(names(defs))
}