#' Individual PK Parameters Listing (pkpl01)
#'
#' Creates individual-level listings of PK parameters from ADPP data using the
#' same engine as [l_pkcl01()].  Returns one listing per unique combination of
#' `listgroup_vars` (default: `PPCAT` × `PPSPEC`).
#'
#' @param data A CDISC ADPP data frame (from `export_cdisc()$adpp`).
#' @param listgroup_vars Character vector of columns used to split the output
#'   into separate listings. Default: `c("PPCAT", "PPSPEC")`.
#' @param grouping_vars Character vector of key/header columns within each
#'   listing. Default: `c("TRT01A", "USUBJID")`.
#' @param displaying_vars Character vector of columns shown as data columns.
#'   Default: `c("PARAM", "AVAL", "AVALU")`.
#' @param title Main listing title string.
#' @param subtitle Per-listing subtitle. Supports `$VAR` / `!VAR` annotation
#'   syntax. Defaults to the unique values of `listgroup_vars`.
#' @param footnote Footnote string.
#' @param formatting_vars_table Optional data frame overriding auto-generated
#'   column formatting (same format as in [l_pkcl01()]).
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

  data <- apply_labels(data)

  if (is.null(subtitle)) {
    subtitle <- paste(
      paste0("!", listgroup_vars),
      paste0("$", listgroup_vars),
      sep  = ": ",
      collapse = "\n"
    )
  }

  present_list_vars <- intersect(listgroup_vars, names(data))

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
        intersect(present_list_vars, names(df)),
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

    rlistings::as_listing(
      df          = wide,
      key_cols    = intersect(grouping_vars, names(wide)),
      disp_cols   = intersect(param_cols, names(wide)),
      main_title  = parse_annotation(data = df, text = title),
      subtitles   = gsub("<br>", "\n",
                         parse_annotation(data = df, text = subtitle)),
      main_footer = parse_annotation(data = df, text = footnote)
    )
  }

  split_and_apply(data, present_list_vars, .make_wide_listing)
}

#' @describeIn l_pkpl01 Listing filtered to metabolite rows (pkpl01 M/P).
#'   Uses the same METABFL → PPCAT → PARAM fallback as [t_pkpt03_MP_col()].
#' @export
l_pkpl01_mp <- function(data, ...) {
  l_pkpl01(filter_metabolite_rows(data, "l_pkpl01_mp"), ...)
}

#' Individual Treatment Comparison Listing (pkpl04)
#'
#' Produces a per-subject listing of individual PK parameter values organised
#' for treatment comparison. Each listing page covers one PPCAT/PPSPEC
#' combination; within each page data is keyed by PARAM, TRT01A, and USUBJID
#' so that a reader can visually compare each subject's values across treatment
#' arms (pkpl04 style).
#'
#' @details
#' This listing shows the raw individual `AVAL` values from ADPP, not
#' pre-computed ratios. If your ADPP contains NCA ratio parameters (e.g.
#' metabolite-to-parent AUC ratios added via the aNCA ratio-calculation
#' module), those parameters are displayed here just like any other PARAM row.
#' The `_mp` suffix in the function name reflects its typical use with
#' metabolite/parent ratio parameters, but no metabolite filtering is applied —
#' all PARAM values in the data are included.
#'
#' @inheritParams l_pkpl01
#' @param grouping_vars Key/header columns. Default: `c("PARAM", "TRT01A",
#'   "USUBJID")`.
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
#' @import dplyr formatters
#' @importFrom stats setNames
#' @export
l_pkpl04_mp <- function(
  data,
  listgroup_vars = c("PPCAT", "PPSPEC"),
  grouping_vars  = c("PARAM", "TRT01A", "USUBJID"),
  param_var      = "PARAM",
  value_var      = "AVAL",
  unit_var       = "AVALU",
  title    = "Listing of Individual PK Parameter Values by Treatment",
  subtitle = NULL,
  footnote = NULL
) {
  l_pkpl01(
    data           = data,
    listgroup_vars = listgroup_vars,
    grouping_vars  = grouping_vars,
    param_var      = param_var,
    value_var      = value_var,
    unit_var       = unit_var,
    title          = title,
    subtitle       = subtitle,
    footnote       = footnote
  )
}

