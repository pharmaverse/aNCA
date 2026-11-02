# Shared preparation for the legacy and NEST listing renderers. Keep pivoting,
# rounding, labels, filters and annotation resolution identical.
.pkpl01_listings <- function(data, listgroup_vars, grouping_vars, param_var,
                             value_var, unit_var, title, subtitle, footnote,
                             param_filter, render_listing) {
  required_cols <- c(grouping_vars, param_var, value_var)
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("l_pkpl01: missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  if (length(param_filter) > 0) {
    data <- data[data[[param_var]] %in% param_filter, , drop = FALSE]
  }
  if (nrow(data) == 0) return(list())

  data <- apply_labels(data, type = "ADPP")

  if (is.null(subtitle)) {
    subtitle <- paste(
      paste0("!", listgroup_vars),
      paste0("$", listgroup_vars),
      sep  = ": ",
      collapse = "\n"
    )
  }

  .make_wide_listing <- function(df) {
    col_labels <- .pkpl_column_labels(df, param_var, unit_var)

    wide <- df %>%
      dplyr::mutate(
        .val_fmt = round(as.numeric(.data[[value_var]]), 3)
      ) %>%
      dplyr::select(dplyr::all_of(c(
        intersect(listgroup_vars, names(df)),
        grouping_vars, param_var, ".val_fmt"
      ))) %>%
      tidyr::pivot_wider(
        names_from  = dplyr::all_of(param_var),
        values_from = ".val_fmt",
        # When a subject has multiple rows for the same PARAM (e.g. multi-
        # interval ADPP), take the first value rather than creating list-columns.
        values_fn   = dplyr::first,
        values_fill = NA_real_
      )

    param_cols <- sort(unique(df[[param_var]]))
    param_cols <- param_cols[param_cols %in% names(wide)]

    var_labels(wide)[param_cols] <- col_labels[
      match(param_cols, sort(unique(df[[param_var]])))
    ]
    for (v in grouping_vars) {
      if (v %in% names(wide)) {
        lbl <- attr(df[[v]], "label")
        if (!is.null(lbl)) var_labels(wide)[v] <- lbl
      }
    }

    # pivot_wider spreads param_var into column headers, so param_var is no longer
    # a column in `wide`.  Exclude it from key_cols so it is not silently dropped
    # by intersect() — it already appears as the display columns (param_cols).
    key_col_candidates <- setdiff(grouping_vars, param_var)
    render_listing(
      df          = wide,
      key_cols    = intersect(key_col_candidates, names(wide)),
      disp_cols   = intersect(param_cols, names(wide)),
      main_title  = parse_annotation(data = df, text = title),
      subtitles   = gsub("<br>", "\n",
                         parse_annotation(data = df, text = subtitle)),
      main_footer = parse_annotation(data = df, text = footnote)
    )
  }

  split_and_apply(data, listgroup_vars, .make_wide_listing)
}

# Build the display headers in parameter order, using the first available unit.
.pkpl_column_labels <- function(df, param_var, unit_var) {
  params <- sort(unique(df[[param_var]]))
  if (!unit_var %in% names(df)) return(params)
  vapply(params, function(p) {
    units <- unique(df[[unit_var]][df[[param_var]] == p])
    unit <- units[!is.na(units) & nzchar(trimws(units))][1]
    if (!is.na(unit) && nchar(unit) > 0) paste0(p, " (", unit, ")") else p
  }, character(1))
}
