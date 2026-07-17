#' Identify columns suitable as a group-comparison variable.
#'
#' Restricts the `col_group_var` dropdown (via the `.groupcols` choices token) to
#' plausible grouping variables so users cannot pick nonsensical columns (a
#' continuous value like `AVAL`, a units column, a high-cardinality identifier,
#' or a column with only one real level).  A column qualifies when it is
#' categorical (character/factor/logical, or a numeric with few distinct values)
#' and has between 2 and `max_levels` distinct non-missing, non-blank values.
#'
#' @param df A data frame.
#' @param max_levels Maximum number of distinct values a column may have.
#' @return Character vector of qualifying column names.
.sensible_group_cols <- function(df, max_levels = 10) {
  keep <- vapply(names(df), function(cn) {
    x  <- df[[cn]]
    xv <- x[!is.na(x) & trimws(as.character(x)) != ""]
    n  <- length(unique(xv))
    categorical <- is.character(x) || is.factor(x) || is.logical(x) ||
      (is.numeric(x) && n <= max_levels)
    categorical && n >= 2 && n <= max_levels
  }, logical(1))
  names(df)[keep]
}

#' Function generating an input widget for TLG option.
#' @param id      id of the input widget
#' @param opt_def definition of the option, as specified in the `yaml` file
#' @param data    data object used for parsing labels, strings, inferring placeholder values or
#'                choices etc.
tlg_option_select_ui <- function(id, opt_def, data) {
  ns <- NS(id)

  label <- if (is.null(opt_def$label)) sub(".*-(.*)", "\\1", id) else opt_def$label

  conc_df <- if (is.data.frame(data())) data() else data()$conc$data
  choices <- {
    if (isTRUE(opt_def$choices == ".colnames")) {
      names(conc_df)
    } else if (isTRUE(opt_def$choices == ".groupcols")) {
      .sensible_group_cols(conc_df)
    } else if (isTRUE(opt_def$choices == ".stats")) {
      # Named vector: names are the readable labels shown in the dropdown, values
      # are the terse statistic names passed to the `stats` function argument.
      labels <- aNCA:::.STAT_LABELS
      setNames(names(labels), unname(labels))
    } else if (length(opt_def$choices) == 1 && grepl("^\\$", opt_def$choices)) {
      unique(conc_df[, sub("^\\$", "", opt_def$choices)])
    } else {
      opt_def$choices
    }
  }

  selected <- {
    if (!is.null(opt_def$default)) {
      if (isTRUE(opt_def$default == ".all")) {
        choices
      } else {
        opt_def$default
      }
    } else {
      ""
    }
  }

  selectInput(
    ns("select"),
    label = label,
    selected = selected,
    choices = c("", choices),
    multiple = isTRUE(opt_def$multiple)
  )
}

#' Function generating an input widget server for TLG option.
#' @param id            id of the input widget
#' @param opt_def       definition of the option, as specified in the `yaml` file
#' @param data          data object used for parsing labels, strings, inferring placeholder
#'                      values or choices etc.
#' @param reset_trigger a reactive expression on which the module will restore its returned value
#'                      to the default one.
#' @returns a reactive with the input value
tlg_option_select_server <- function(id, opt_def, data, reset_trigger) {
  moduleServer(id, function(input, output, session) {
    #' Reset the input to default value upon reset_trigger
    observeEvent(reset_trigger(), shinyjs::reset("select"))

    reactive({
      # A `multiple` selectInput with nothing selected returns NULL (a single
      # select returns ""). Coerce NULL to "" so an unset optional widget flows
      # through the option filter as "use the function default" rather than
      # tripping the is-null guard in tlg_module_server that halts the whole
      # render (which blanked every table carrying an empty multi-select).
      input$select %||% ""
    })
  })
}
