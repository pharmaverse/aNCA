#' NEST pilot: individual PK parameter listing
#'
#' Uses the same data preparation, rounding and annotation parser as
#' [l_pkpl01()], with `crane::tbl_listing()` for presentation. Displayed values
#' keep the existing three-decimal rounding regardless of the session's `digits`
#' option. Interval parameters (with `PPSTINT`/`PPENINT`, e.g. partial AUCs)
#' are listed per interval, with the interval appended to the parameter name.
#' Repeated keys are suppressed within their parent groups only. Excluded
#' records remain in this individual listing. Conflicting values/units within a
#' reporting key require more grouping variables and produce an explanatory error.
#'
#' @inheritParams l_pkpl01
#' @param listgroup_vars Columns used to split listings. Defaults to analyte,
#'   specimen and dose occasion (`PPCAT`, `PPSPEC`, `ATPTREF`); absent split
#'   columns are ignored.
#' @returns List of `gtsummary` listings, named by split, or an explanatory string
#'   within the list when no records remain. The `tlg_data` attribute keeps
#'   numeric values and complete keys for CSV/XLSX; `tlg_annotations` keeps
#'   resolved titles, subtitles and footnotes. Repeated-key suppression applies
#'   to presentation only.
#' @importFrom dplyr as_tibble
#' @export
l_pkpl01_nest <- function(data, listgroup_vars = c("PPCAT", "PPSPEC", "ATPTREF"),
                          grouping_vars = c("TRT01A", "USUBJID"), param_var = "PARAM",
                          value_var = "AVAL", unit_var = "AVALU",
                          title = "Listing of Individual PK Parameters", subtitle = NULL,
                          footnote = "Individual values include summary-excluded records.",
                          param_filter = NULL) {
  .nest_require(c("crane", "gtsummary", "gt"))
  data <- as_tibble(data)
  listgroup_vars <- intersect(listgroup_vars, names(data))
  missing <- setdiff(c(grouping_vars, param_var, value_var), names(data))
  if (length(missing)) stop("Missing required columns: ", paste(missing, collapse = ", "))
  if (length(param_filter)) data <- data[data[[param_var]] %in% param_filter, , drop = FALSE]
  if (!nrow(data)) return(list("No data available for this NEST listing."))
  data <- .nest_interval_param(data, param_var)
  .nest_check_cells(data, c(listgroup_vars, grouping_vars, param_var), value_var)
  if (unit_var %in% names(data)) {
    known <- !is.na(data[[unit_var]]) & nzchar(trimws(data[[unit_var]]))
    if (any(!known & !is.na(data[[value_var]]))) {
      stop("NEST pilot: a nonmissing value has no unit. Supply units before reporting.")
    }
    .nest_check_cells(data[known, , drop = FALSE], c(listgroup_vars, param_var), unit_var)
  }
  # Parameters are already filtered; interval names no longer match the filter.
  listings <- .pkpl01_listings(data, listgroup_vars, grouping_vars, param_var, value_var,
                               unit_var, title, subtitle, footnote, NULL, .nest_listing)
  if (is.null(subtitle)) {
    # The default subtitle already identifies the split, so skip the preview's group header.
    listings <- lapply(listings, function(x) {
      if (inherits(x, "gtsummary")) attr(x, "tlg_group_header") <- FALSE
      x
    })
  }
  listings
}

.nest_listing <- function(df, key_cols, disp_cols, main_title, subtitles, main_footer) {
  if (!nrow(df)) return("No data available for this NEST listing.")
  frame <- as.data.frame(df[unique(c(key_cols, disp_cols))])
  if (length(key_cols)) {
    frame <- frame[do.call(order, lapply(frame[key_cols], .natural_sort_key)), , drop = FALSE]
  }
  for (v in names(frame)) attr(frame[[v]], "label") <- .nest_label(df, v)
  rownames(frame) <- NULL
  # Safe internal names also support parameter names containing spaces or punctuation.
  ids <- paste0("col_", seq_along(frame))
  key_ids <- ids[match(key_cols, names(frame))]
  # Numeric keys become text, and values get explicit formatting, so gt and
  # flextable do not reformat them with the session's significant digits.
  display <- frame
  display[key_cols] <- lapply(display[key_cols], function(x) {
    if (is.numeric(x)) as.character(x) else x
  })
  table <- crane::tbl_listing(as_tibble(setNames(display, ids)))
  value_ids <- setdiff(ids[vapply(display, is.numeric, logical(1))], key_ids)
  if (length(value_ids)) {
    table <- gtsummary::modify_fmt_fun(table, all_of(value_ids) ~ .nest_format_number)
  }
  headers <- setNames(as.list(vapply(names(frame), function(v) .nest_label(frame, v),
                                     character(1))), ids)
  table <- do.call(gtsummary::modify_header, c(list(x = table), headers))
  original <- table$table_body
  if (length(key_ids)) table <- crane::remove_duplicate_keys(table, keys = all_of(key_ids))
  # crane suppresses each key independently; restore child keys at parent boundaries.
  for (i in seq_along(key_ids)) {
    prefix <- original[key_ids[seq_len(i)]]
    same <- c(FALSE, vapply(seq_len(nrow(prefix) - 1L), function(j) {
      identical(unname(as.list(prefix[j, ])), unname(as.list(prefix[j + 1L, ])))
    }, logical(1)))
    table$table_body[[key_ids[i]]][!same] <- as.character(original[[key_ids[i]]][!same])
  }
  attr(table, "tlg_key_count") <- length(key_cols)
  .nest_attach(table, frame, list(title = main_title %||% "", subtitle = subtitles %||% "",
                                  footnote = main_footer %||% ""))
}
