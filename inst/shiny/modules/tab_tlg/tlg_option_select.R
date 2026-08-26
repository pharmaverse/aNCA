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

#' Column choices for the ratio TLG entries.
#'
#' `filter_ratio_rows()` derives `RATIO` and `RATIOREF` inside the TLG function, so
#' they are not columns of ADPP and the `.colnames` token cannot offer them.  The
#' four ratio entries default to splitting on `RATIO`; without this token (used via
#' `.ratiocols`) a user who touched the split dropdown lost the derived label with
#' no way to select it back.
#'
#' `RATIOREF` is missing on same-group ratios, which have no reference group, so
#' splitting by it drops those rows -- `split_and_apply()` says so when it happens.
#'
#' @param df A data frame.
#' @return The derived ratio columns followed by the data's own column names.
.ratio_col_names <- function(df) {
  unique(c(aNCA:::.RATIO_DERIVED_COLS, names(df)))
}

#' Parameter values that belong to a ratio of one family.
#'
#' Restricts the parameter filter on the ratio TLG entries (via the `.ratioparams`
#' choices token) to parameters that can actually appear.  The entries drop every
#' non-ratio row before summarizing, so offering the full `PARAM` list let a user
#' pick a value that could only ever produce an empty output.  The family matters
#' for the same reason: a parameter that only ever carries a treatment ratio is
#' just as empty on a metabolite/parent entry as a non-ratio one.  Rows are
#' classified exactly as `filter_ratio_rows()` classifies them.
#'
#' @param df A data frame.
#' @param ref_type Ratio family to keep, as in `aNCA:::filter_ratio_rows()`.
#'   Defaults to `"analyte"`, the family the `.ratioparams` entries render.
#' @return Character vector of ratio parameter names, sorted; empty if none.
.ratio_param_values <- function(df, ref_type = "analyte") {
  if (!all(c("PARAM", "PPANMETH") %in% names(df))) return(character(0))
  is_ratio <- aNCA:::.ratio_row_type(df$PPANMETH) %in% ref_type
  values <- unique(as.character(df$PARAM[is_ratio]))
  sort(values[!is.na(values)])
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

#' Resolve a `choices:` entry from the TLG yaml against the data behind an entry.
#'
#' A `.token` is looked up in the table below; a `$COLUMN` reference yields that
#' column's distinct values; anything else is a literal list taken as written.
#'
#' @param choices The option definition's `choices` value.
#' @param conc_df The data frame backing the TLG entry.
#' @return A character vector of choices, named where the dropdown label should
#'   differ from the value passed to the function.
.resolve_option_choices <- function(choices, conc_df) {
  token <- if (length(choices) == 1 && is.character(choices)) choices else ""

  resolvers <- list(
    .colnames    = function() names(conc_df),
    .ratiocols   = function() .ratio_col_names(conc_df),
    .ratioparams = function() .ratio_param_values(conc_df),
    .groupcols   = function() .sensible_group_cols(conc_df),
    .urinespecs  = function() .urine_spec_values(conc_df),
    # Named vector: names are the readable labels shown in the dropdown, values
    # are the terse statistic names passed to the `stats` function argument.
    .stats       = function() {
      labels <- aNCA:::.STAT_LABELS
      setNames(names(labels), unname(labels))
    }
  )
  if (token %in% names(resolvers)) return(resolvers[[token]]())

  if (!grepl("^\\$", token)) return(choices)

  # `[[` rather than `[` so a tibble yields a vector: `[` returns a one-column data frame,
  # which `selectInput` then labels with the column name instead of its values.
  col <- sub("^\\$", "", token)
  if (!col %in% names(conc_df)) return(character(0))
  col_values <- unique(conc_df[[col]])
  as.character(col_values[!is.na(col_values)])
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
  choices <- .resolve_option_choices(opt_def$choices, conc_df)

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
