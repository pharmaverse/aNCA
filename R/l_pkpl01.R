#' Individual PK Parameters Listing (pkpl01)
#'
#' Creates individual-level listings of PK parameters from ADPP data using the
#' same engine as [l_pkcl01()].  Returns one listing per unique combination of
#' `listgroup_vars` (default: `PPCAT` x `PPSPEC`).
#'
#' @param data A CDISC ADPP data frame (from `export_cdisc()$adpp`).
#' @param listgroup_vars Character vector of columns used to split the output
#'   into separate listings. Default: `c("PPCAT", "PPSPEC")`.
#' @param grouping_vars Character vector of key/header columns within each
#'   listing (shown as indented row keys). Default: `c("TRT01A", "USUBJID")`.
#' @param param_var Column whose unique values become display columns after
#'   pivoting wide. Default: `"PARAM"`.
#' @param value_var Column containing the numeric analysis value. Default: `"AVAL"`.
#' @param unit_var Column containing the parameter unit used to build column
#'   headers (`"<PARAM> (<unit>)"`). Default: `"AVALU"`.
#' @param title Main listing title string.
#' @param subtitle Per-listing subtitle. Supports `$VAR` / `!VAR` annotation
#'   syntax. Defaults to the unique values of `listgroup_vars`.
#' @param footnote Footnote string.
#'
#' @return A named list of `listing_df` objects (one per `listgroup_vars`
#'   combination), suitable for printing in a Shiny `verbatimTextOutput`.
#'
#' @examples
#' \dontrun{
#' adpp <- export_cdisc(res_nca)$adpp
#' listings <- l_pkpl01(adpp)
#' print(listings[[1]])
#' }
#'
#' @import dplyr formatters
#' @importFrom stats setNames
#' @importFrom tidyr pivot_wider
#' @export
l_pkpl01 <- function(
  data,
  listgroup_vars  = c("PPCAT", "PPSPEC"),
  grouping_vars   = c("TRT01A", "USUBJID"),
  param_var       = "PARAM",
  value_var       = "AVAL",
  unit_var        = "AVALU",
  title    = "Listing of Individual PK Parameters",
  subtitle = NULL,
  footnote = NULL
) {
  if (!requireNamespace("rlistings", quietly = TRUE)) {
    stop(
      "Package 'rlistings' is required for PK parameter listings. ",
      "Install it with install.packages('rlistings')"
    )
  }

  required_cols <- c(grouping_vars, param_var, value_var)
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("l_pkpl01: missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

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
    has_unit <- unit_var %in% names(df)

    col_labels <- if (has_unit) {
      vapply(sort(unique(df[[param_var]])), function(p) {
        u <- unique(df[[unit_var]][df[[param_var]] == p])
        u <- u[!is.na(u)][1]
        if (!is.na(u) && nchar(u) > 0) paste0(p, " (", u, ")") else p
      }, character(1))
    } else {
      sort(unique(df[[param_var]]))
    }

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
    rlistings::as_listing(
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

#' @describeIn l_pkpl01 Listing of metabolite-to-parent ratios (pkpl01 M/P).
#'   Lists the M/P ratio rows computed by **Parameter Selection > Ratios** during the
#'   NCA run -- the ones whose `PPANMETH` reference group is another analyte -- with
#'   one listing per `"<metabolite> / <parent>"` pair, so the denominator is named
#'   rather than left for the reader to infer. See [t_pkpt03_MP_col()].
#' @param listgroup_vars Columns used to split the output into separate listings.
#'   Defaults to the derived `RATIO` label and `PPSPEC`.
#' @param ... Additional arguments forwarded to [l_pkpl01()].
#' @export
l_pkpl01_mp <- function(data, listgroup_vars = c("RATIO", "PPSPEC"), ...) {
  l_pkpl01(
    filter_ratio_rows(data, "l_pkpl01_mp", ref_type = "analyte"),
    listgroup_vars = listgroup_vars,
    ...
  )
}

#' Individual Treatment Ratio Listing (pkpl04)
#'
#' Produces a per-subject listing of the treatment ratios computed by
#' **Parameter Selection > Ratios** during the NCA run. PK parameters become
#' display columns (one column per PARAM value) and the rows are keyed by
#' `TRT01A` and `USUBJID`.
#'
#' @details
#' Ratio rows are selected on the `" TO "` that `calculate_ratios()` writes into
#' `PPANMETH`, and narrowed to those whose reference group is *not* the analyte
#' column: treatment, dose profile, route or specimen references. Analyte
#' references are metabolite/parent ratios and belong to [l_pkpl01_mp()], which
#' selects the complement of this set from the same mechanism.
#'
#' Ratios are computed as part of the NCA run, so a ratio added afterwards does
#' not appear until the run is repeated. When the data holds no treatment ratios
#' the listing errors with that instruction rather than silently listing raw
#' parameter values, which are not ratios.
#'
#' Note: `PARAM` is listed in `grouping_vars` so that it participates in the
#' `pivot_wider` step (each unique PARAM value becomes a column header). After
#' pivoting, `PARAM` is no longer a row-key column; the actual listing keys
#' are `TRT01A` and `USUBJID`.
#'
#' @inheritParams l_pkpl01
#' @param listgroup_vars Columns used to split the output into separate listings.
#'   Defaults to the derived `RATIO` label and `PPSPEC`.
#' @param grouping_vars Columns used to identify row keys before pivoting.
#'   `PARAM` must be included so it is spread into display columns.
#'   Default: `c("PARAM", "TRT01A", "USUBJID")`.
#' @param ... Additional arguments forwarded to [l_pkpl01()].
#'
#' @return A named list of `listing_df` objects.
#'
#' @examples
#' \dontrun{
#' adpp <- export_cdisc(res_nca)$adpp
#' listings <- l_pkpl04_mp(adpp)
#' print(listings[[1]])
#' }
#'
#' @export
l_pkpl04_mp <- function(
  data,
  listgroup_vars = c("RATIO", "PPSPEC"),
  grouping_vars = c("PARAM", "TRT01A", "USUBJID"),
  title         = "Listing of Individual Treatment Ratios",
  ...
) {
  l_pkpl01(
    filter_ratio_rows(data, "l_pkpl04_mp", ref_type = "other"),
    listgroup_vars = listgroup_vars,
    grouping_vars = grouping_vars,
    title = title,
    ...
  )
}
