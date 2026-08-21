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

#' Specimen values that look like urine.
#'
#' Restricts the specimen filter on urine-only TLGs (via the `.urinespecs` choices
#' token) to values matching "urin", so a urine table or listing cannot be pointed at
#' serum or plasma.  Matching on the value rather than hardcoding `"URINE"` keeps
#' non-standard labels such as `"Urine - void"` usable.  The specimen column is named
#' `PCSPEC` in ADNCA and `PPSPEC` in ADPP, so whichever the dataset carries is used.
#'
#' @param df A data frame.
#' @return Character vector of urine specimen values, sorted; empty if none.
.urine_spec_values <- function(df) {
  spec_col <- intersect(c("PCSPEC", "PPSPEC"), names(df))
  if (length(spec_col) == 0) return(character(0))
  values <- unique(df[[spec_col[1]]])
  values <- as.character(values[!is.na(values)])
  sort(values[grepl("urin", values, ignore.case = TRUE)])
}

#' Function generating an input widget for TLG option.
#' @param id      id of the input widget
#' @param opt_def definition of the option, as specified in the `yaml` file
#' @param data    data object used for parsing labels, strings, inferring placeholder values or
#'                choices etc.
#' @param grouping_vars reactive returning the PKNCA grouping variables (minus the
#'                subject column); used to resolve the `.pknca_groups` default token.
tlg_option_select_ui <- function(id, opt_def, data, grouping_vars = reactive(character())) {
  ns <- NS(id)

  label <- if (is.null(opt_def$label)) sub(".*-(.*)", "\\1", id) else opt_def$label

  conc_df <- if (is.data.frame(data())) data() else data()$conc$data
  choices <- {
    if (isTRUE(opt_def$choices == ".colnames")) {
      names(conc_df)
    } else if (isTRUE(opt_def$choices == ".groupcols")) {
      .sensible_group_cols(conc_df)
    } else if (isTRUE(opt_def$choices == ".urinespecs")) {
      .urine_spec_values(conc_df)
    } else if (isTRUE(opt_def$choices == ".stats")) {
      # Named vector: names are the readable labels shown in the dropdown, values
      # are the terse statistic names passed to the `stats` function argument.
      labels <- aNCA:::.STAT_LABELS
      setNames(names(labels), unname(labels))
    } else if (length(opt_def$choices) == 1 && grepl("^\\$", opt_def$choices)) {
      # `[[` rather than `[` so a tibble yields a vector: `[` returns a one-column data frame,
      # which `selectInput` then labels with the column name instead of its values.
      col <- sub("^\\$", "", opt_def$choices)
      if (col %in% names(conc_df)) {
        col_values <- unique(conc_df[[col]])
        as.character(col_values[!is.na(col_values)])
      } else {
        character(0)
      }
    } else {
      opt_def$choices
    }
  }

  selected <- {
    if (!is.null(opt_def$default)) {
      if (isTRUE(opt_def$default == ".all")) {
        choices
      } else if (isTRUE(opt_def$default == ".pknca_groups")) {
        # Default row-stratification = the PKNCA grouping variables (minus
        # USUBJID) that are actually present in this table's dataset.  When none
        # are available (e.g. before NCA has run) fall back to nothing selected,
        # which lets the TLG function use its own default.
        intersect(grouping_vars(), names(conc_df))
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
