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
#' @param param_filter Optional character vector of values from `param_var` to
#'   display as columns. `NULL` or an empty vector (default) keeps all parameters.
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
  footnote = NULL,
  param_filter = NULL
) {
  if (!requireNamespace("rlistings", quietly = TRUE)) {
    stop(
      "Package 'rlistings' is required for PK parameter listings. ",
      "Install it with install.packages('rlistings')"
    )
  }

  .pkpl01_listings(
    data, listgroup_vars, grouping_vars, param_var, value_var, unit_var,
    title, subtitle, footnote, param_filter, rlistings::as_listing
  )
}

#' @describeIn l_pkpl01 Listing filtered to metabolite rows (pkpl01 M/P).
#'   Uses the same METABFL -> PPCAT -> PARAM fallback as [t_pkpt03_MP_col()].
#' @param ... Additional arguments forwarded to [l_pkpl01()].
#' @export
l_pkpl01_mp <- function(data, ...) {
  l_pkpl01(filter_metabolite_rows(data, "l_pkpl01_mp"), ...)
}

#' Individual Treatment Comparison Listing (pkpl04)
#'
#' Produces a per-subject listing of individual PK parameter values organised
#' for treatment comparison. Each listing page covers one PPCAT/PPSPEC
#' combination. PK parameters become display columns (one column per PARAM value)
#' and the rows are keyed by `TRT01A` and `USUBJID`.
#'
#' @details
#' This listing shows the raw individual `AVAL` values from ADPP, not
#' pre-computed ratios. If your ADPP contains NCA ratio parameters (e.g.
#' metabolite-to-parent AUC ratios added via the aNCA ratio-calculation
#' module), those parameters are displayed here just like any other PARAM row.
#' The `_mp` suffix in the function name reflects its typical use with
#' metabolite/parent ratio parameters, but no metabolite filtering is applied --
#' all PARAM values in the data are included.
#'
#' @inheritParams l_pkpl01
#' @param grouping_vars Columns used to identify row keys before pivoting.
#'   The parameter column is supplied separately by `param_var`.
#'   Default: `c("TRT01A", "USUBJID")`.
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
  grouping_vars = c("TRT01A", "USUBJID"),
  title         = "Listing of Individual PK Parameter Values by Treatment",
  ...
) {
  l_pkpl01(data, grouping_vars = grouping_vars, title = title, ...)
}
