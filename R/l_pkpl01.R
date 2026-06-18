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
        values_from = ".val_fmt"
      )

    param_cols <- sort(unique(df[[param_var]]))
    param_cols <- param_cols[param_cols %in% names(wide)]

    formatters::var_labels(wide)[param_cols] <- col_labels[
      match(param_cols, sort(unique(df[[param_var]])))
    ]
    for (v in grouping_vars) {
      if (v %in% names(wide)) {
        lbl <- attr(df[[v]], "label")
        if (!is.null(lbl)) formatters::var_labels(wide)[v] <- lbl
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

  if (length(present_list_vars) == 0) {
    return(list(all = .make_wide_listing(data)))
  }

  split_keys <- do.call(
    interaction,
    c(lapply(present_list_vars, function(v) as.character(data[[v]])),
      list(sep = " / ", drop = TRUE))
  )

  listings <- lapply(levels(split_keys), function(key) {
    .make_wide_listing(data[split_keys == key, , drop = FALSE])
  })
  setNames(listings, levels(split_keys))
}

#' @describeIn l_pkpl01 Listing filtered to metabolite rows (pkpl01 M/P).
#'   Uses the same METABFL → PPCAT → PARAM fallback as [t_pkpt03_MP_col()].
#' @export
l_pkpl01_mp <- function(data, ...) {
  if ("METABFL" %in% names(data) &&
      any(!is.na(data$METABFL) & data$METABFL != "")) {
    data <- data[!is.na(data$METABFL) & data$METABFL != "", , drop = FALSE]
    return(l_pkpl01(data, ...))
  }

  for (col in c("PPCAT", "PARAM")) {
    if (col %in% names(data)) {
      if (any(grepl("metab", data[[col]], ignore.case = TRUE))) {
        data <- data[grepl("metab", data[[col]], ignore.case = TRUE), ,
                     drop = FALSE]
        return(l_pkpl01(data, ...))
      }
    }
  }

  stop(
    "l_pkpl01_mp: no metabolite data found. ",
    "METABFL is absent or all missing, and no PPCAT/PARAM values contain ",
    "'metab'. Include METABFL as a grouping variable in your NCA run, or ",
    "ensure metabolite rows are labelled with 'metab' in PPCAT or PARAM."
  )
}

#' Individual Treatment Ratio Listing (pkpl04)
#'
#' Produces a per-subject listing of PK parameter values organised for
#' treatment comparison.  Each listing page covers one PPCAT/PPSPEC
#' combination; within each page the data is keyed by PARAM and USUBJID so
#' readers can compare values across treatment arms side-by-side in the
#' key-column header rows.
#'
#' @details
#' This listing shows raw individual AVAL values (not pre-computed ratios).
#' It is designed for crossover studies where the same subject appears in
#' multiple treatment arms, but it also renders for parallel designs.
#'
#' @inheritParams l_pkpl01
#' @param grouping_vars Key/header columns. Default: `c("PARAM", "TRT01A",
#'   "USUBJID")`.
#' @param displaying_vars Data columns. Default: `c("AVAL", "AVALU")`.
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

