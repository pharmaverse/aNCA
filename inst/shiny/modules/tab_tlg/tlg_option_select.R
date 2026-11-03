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

  config <- .tlg_select_config(opt_def, data, grouping_vars)

  pickerInput(
    ns("select"),
    label = label,
    selected = config$selected,
    choices = config$choices,
    multiple = isTRUE(opt_def$multiple),
    options = list(
      `none-selected-text` = opt_def$placeholder %||% "Nothing selected",
      `live-search` = TRUE,
      `actions-box` = isTRUE(opt_def$multiple),
      `selected-text-format` = "count > 3"
    )
  )
}

#' Resolve select choices and catalog defaults for creation and reset.
#' @noRd
.tlg_select_config <- function(opt_def, data, grouping_vars = reactive(character())) {
  conc_df <- if (is.data.frame(data())) data() else data()$conc$data
  if (isTRUE(opt_def$metabolites_only)) {
    conc_df <- aNCA:::filter_metabolite_rows(conc_df, allow_empty = TRUE)
  }
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
      # which the picker then labels with the column name instead of its values.
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

  selected <- opt_def$default %||% ""
  if (isTRUE(selected == ".all")) {
    selected <- choices
  } else if (isTRUE(selected == ".pknca_groups")) {
    # Default row-stratification = the PKNCA grouping variables (minus
    # USUBJID) that are actually present in this table's dataset. When none
    # are available, leave the selection empty to use the function default.
    selected <- intersect(grouping_vars(), names(conc_df))
  }

  # Multi-selects use Deselect all; only single-selects need an unset choice.
  list(choices = if (isTRUE(opt_def$multiple)) choices else c("", choices),
       selected = selected)
}

#' Function generating an input widget server for TLG option.
#' @param id            id of the input widget
#' @param opt_def       definition of the option, as specified in the `yaml` file
#' @param data          data object used for parsing labels, strings, inferring placeholder
#'                      values or choices etc.
#' @param reset_trigger a reactive expression on which the module will restore its returned value
#'                      to the default one.
#' @param grouping_vars Reactive grouping variables for the `.pknca_groups` default.
#' @param value_override Reactive selection restored from settings.
#' @param choices_from Reactive source column for a dependent filter selector.
#' @returns a reactive with the input value
tlg_option_select_server <- function(id, opt_def, data, reset_trigger,
                                     grouping_vars = reactive(character()),
                                     value_override = reactive(NULL),
                                     choices_from = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    config <- reactive({
      definition <- .tlg_select_definition(opt_def, choices_from())
      tryCatch(.tlg_select_config(definition, data, grouping_vars),
               shiny.silent.error = function(e) NULL)
    })
    value <- reactiveVal(NULL)
    selected <- function(candidate, warn = FALSE) {
      .tlg_select_value(candidate, config(), opt_def$multiple, id, warn)
    }
    observeEvent(input$select, {
      if (!"select" %in% names(input)) return()
      value(selected(input$select))
    }, ignoreNULL = FALSE)
    observeEvent(value_override(), {
      restored <- value_override()
      if (is.null(config()) && is.null(restored)) {
        value(NULL)
      } else {
        value(selected(if (is.null(restored)) config()$selected else restored,
                       warn = !is.null(restored)))
        updatePickerInput(session, "select", selected = value())
      }
    }, ignoreNULL = FALSE, priority = if (is.null(opt_def$choices_from)) 100 else 90)
    observeEvent(config()$choices, {
      req(config())
      value(selected(if (is.null(value())) config()$selected else value(),
                     warn = !is.null(value())))
      updatePickerInput(session, "select", choices = config()$choices, selected = value())
    })
    observeEvent(reset_trigger(), {
      value(if (is.null(config())) NULL else selected(config()$selected))
      updatePickerInput(session, "select", selected = value())
    }, ignoreInit = TRUE)

    reactive({
      if (is.null(config()) && is.null(value())) return(NULL)
      if (is.null(value())) selected(config()$selected) else value()
    })
  })
}

#' Keep valid selections without confusing an explicit clear with an absent input.
#' @noRd
.tlg_select_value <- function(candidate, config, multiple, id, warn = FALSE) {
  candidate <- as.character(candidate)
  if (is.null(config)) return(candidate)
  available <- as.character(config$choices)
  missing <- setdiff(candidate, c("", available))
  if (warn && length(missing)) {
    showNotification(paste0("Skipped unavailable TLG selections for ", id, ": ",
                            paste(missing, collapse = ", ")), type = "warning")
  }
  kept <- intersect(candidate, available)
  if (!isTRUE(multiple) && !length(kept)) "" else kept
}

#' Resolve the source column for a dependent selector.
#' @noRd
.tlg_select_definition <- function(definition, column) {
  if (length(column) == 1L && nzchar(column)) definition$choices <- paste0("$", column)
  definition
}
