#' NEST pilot: PK parameter summary with statistics in columns
#'
#' Adapts [t_pkpt03_col()] results to a `cards` ARD and a `gtsummary` wide
#' summary. Statistics, rounding and summary exclusions use the existing aNCA
#' implementation. Units are retained as row keys. Interval parameters (with
#' `PPSTINT`/`PPENINT`, e.g. partial AUCs) are summarized per interval, with the
#' interval appended to `PARAM`. Conflicting duplicate values are rejected
#' rather than silently selecting one measurement.
#' Presentation preserves the existing decimal precision independently of the
#' session's `digits` option.
#'
#' @inheritParams t_pkpt03_col
#' @param list_vars Columns used to split tables. Defaults to analyte,
#'   specimen and dose occasion (`PPCAT`, `PPSPEC`, `ATPTREF`); absent split
#'   columns are ignored.
#' @param title,subtitle,footnote Annotation text supporting `$VAR` and `!VAR`.
#'   A `NULL` subtitle uses the table split key verbatim.
#' @returns Named list of `gtsummary` objects, or an explanatory string for
#'   empty data. Each object has `tlg_data` (numeric results), `tlg_ard`
#'   (presentation ARD), and `tlg_annotations` attributes. ARD `variable` and
#'   `stat_name` are local row/column identifiers; `tlg_ard_columns` maps the
#'   statistic identifiers to the original result columns. Grouping columns
#'   in the ARD identify the source result row. This is not a CDISC submission ARD.
#' @export
t_pkpt03_nest <- function(data, list_vars = c("PPCAT", "PPSPEC", "ATPTREF"),
                          strat_var = c("TRT01A", "PARAM"),
                          value_var = "AVAL", param_filter = NULL,
                          col_group_var = NULL, stats = NULL,
                          title = "Summary of PK Parameters", subtitle = NULL,
                          footnote = paste("Summary-excluded records are omitted;",
                                           "n counts nonmissing values.")) {
  .nest_require(c("cards", "gtsummary", "gt"))
  data <- as_tibble(data)
  data <- filter_summary_excluded(data, "PPSUMXF")
  if (length(param_filter) && "PARAM" %in% names(data)) {
    data <- data[data$PARAM %in% param_filter, , drop = FALSE]
  }
  data <- .nest_interval_param(data, "PARAM")
  strat_var <- unique(c(strat_var, setdiff(intersect("AVALU", names(data)), list_vars)))
  .nest_check_cells(data, c(list_vars, .pkpp_dedup_cols(data, strat_var, col_group_var)),
                    value_var)
  # Parameters are already filtered; interval names no longer match the filter.
  frames <- t_pkpt03_col(data, list_vars, strat_var, value_var, NULL, col_group_var, stats)
  splits <- split_and_apply(data, list_vars, identity)
  lapply(seq_along(frames), function(i) {
    frame <- frames[[i]]
    if (!nrow(frame)) return("No data available for this NEST summary.")
    table <- .nest_wide_summary(frame)
    notes <- .nest_annotations(splits[[i]], title, subtitle %||% "", footnote)
    if (is.null(subtitle)) {
      # The split key is data, not annotation syntax: never evaluate it as a template.
      notes$subtitle <- if (identical(names(frames)[i], "all")) "" else names(frames)[i]
      # The subtitle already identifies the split, so skip the preview's group header.
      attr(table, "tlg_group_header") <- FALSE
    }
    .nest_attach(table, frame, notes)
  }) %>% setNames(names(frames))
}

.nest_wide_summary <- function(frame) {
  stat_cols <- names(frame)[sub(paste0("^.*", .GROUP_SEP), "", names(frame)) %in%
                              names(.STAT_LABELS)]
  if (!length(stat_cols)) stop("Select at least one valid summary statistic.", call. = FALSE)
  keys <- setdiff(names(frame), stat_cols)
  row_ids <- paste0("row_", seq_len(nrow(frame)))
  stat_ids <- paste0("s", seq_along(stat_cols))
  labels <- vapply(stat_cols, function(v) .nest_label(frame, v), character(1))
  ard <- do.call(rbind, lapply(seq_along(stat_cols), function(j) {
    out <- data.frame(variable = row_ids, context = "summary", stat_name = stat_ids[j],
                      stat_label = unname(labels[j]), stringsAsFactors = FALSE)
    out$stat <- as.list(frame[[stat_cols[j]]])
    out$fmt_fun <- rep(list(.nest_format_number), nrow(out))
    out$warning <- out$error <- rep(list(NULL), nrow(out))
    cbind(out, frame[keys])
  }))
  ard <- cards::as_card(ard)
  table <- gtsummary::tbl_ard_wide_summary(
    ard, statistic = paste0("{", stat_ids, "}"),
    type = setNames(as.list(rep("continuous", length(row_ids))), row_ids)
  )
  # Restore separate row-key columns rather than concatenating ambiguous labels.
  # Numeric keys (e.g. dose) become text so gt does not reformat them with session digits.
  key_ids <- if (length(keys)) paste0("key_", seq_along(keys)) else character()
  key_frame <- frame[keys]
  key_frame[] <- lapply(key_frame, function(x) if (is.numeric(x)) as.character(x) else x)
  table <- gtsummary::modify_table_body(table, function(body) {
    cbind(setNames(key_frame[match(body$variable, row_ids), , drop = FALSE], key_ids), body)
  })
  table <- gtsummary::modify_column_hide(table, columns = "label")
  if (length(keys)) {
    headers <- setNames(as.list(vapply(keys, function(v) .nest_label(frame, v),
                                       character(1))), key_ids)
    table <- do.call(gtsummary::modify_header, c(list(x = table), headers))
  }
  groups <- attr(frame, "col_groups")
  for (group in names(groups)) {
    cols <- paste0("stat_", match(groups[[group]], stat_cols))
    table <- do.call(gtsummary::modify_spanning_header,
                     c(list(x = table), setNames(as.list(rep(group, length(cols))), cols)))
  }
  attr(table, "tlg_key_count") <- length(keys)
  attr(table, "tlg_ard") <- ard
  attr(table, "tlg_ard_columns") <- setNames(stat_cols, stat_ids)
  table
}
