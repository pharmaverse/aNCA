#' Identify parent/metabolite pairs from mapped concentration data
#'
#' Pairing uses the drug and study, not an analyte-name prefix. A drug may have
#' several metabolites, but each must have exactly one unflagged parent.
#' @param adnca Mapped concentration data with PARAM, METABFL and DOSETRT.
#' @returns A data frame of study/drug keys, parent and metabolite names.
#' @noRd
.mp_analyte_pairs <- function(adnca) {
  required <- c("PARAM", "METABFL", "DOSETRT")
  missing <- setdiff(required, names(adnca))
  if (length(missing)) stop("M/P ratios: missing ADNCA columns: ", paste(missing, collapse = ", "))
  keys <- intersect(c("STUDYID", "DOSETRT"), names(adnca))
  info <- unique(adnca[union(keys, required)])
  info$PARAM <- as.character(info$PARAM)
  if (anyNA(info[c(keys, "PARAM")]) || any(!nzchar(as.character(info$DOSETRT)))) {
    stop("M/P ratios: map non-missing PARAM and DOSETRT values before identifying the parent.")
  }
  is_metabolite <- info$METABFL %in% "Y"
  metabolites <- unique(info[is_metabolite, c(keys, "PARAM"), drop = FALSE])
  parents <- unique(info[!is_metabolite, c(keys, "PARAM"), drop = FALSE])
  names(metabolites)[names(metabolites) == "PARAM"] <- "metabolite"
  names(parents)[names(parents) == "PARAM"] <- "parent"
  if (!nrow(metabolites)) stop("M/P ratios: no analyte is marked with METABFL = 'Y'.")
  parents <- merge(parents, unique(metabolites[keys]), by = keys)
  if (anyDuplicated(parents[keys])) {
    stop("M/P ratios: ambiguous parent mapping within a study/DOSETRT group.")
  }
  pairs <- merge(metabolites, parents, by = keys, all.x = TRUE)
  if (anyNA(pairs$parent)) {
    stop("M/P ratios: no parent found for a metabolite. Map the pair to the same DOSETRT.")
  }
  if (any(pairs$parent == pairs$metabolite)) {
    stop("M/P ratios: ambiguous METABFL values; an analyte is both parent and metabolite.")
  }
  pairs
}

#' Profile identifiers that must not be collapsed in an M/P output
#' @param data ADPP data.
#' @returns Present identifiers with more than one value in this output.
#' @noRd
.mp_profile_vars <- function(data) {
  cols <- intersect(c(
    "STUDYID", "PPSPEC", "DOSETRT", "ATPTREF", "AVISIT", "AVISITN", "APERIOD", "APERIODC",
    "PERIOD", "ROUTE", "PPSTINT", "PPENINT"
  ), names(data))
  cols[vapply(data[cols], function(x) length(unique(x)) > 1L, logical(1))]
}

#' Validate one-to-one values before matching the parent and metabolite
#' @param data ADPP rows for the two analytes.
#' @param keys Subject, parameter and profile matching columns.
#' @param caller Calling TLG function for diagnostics.
#' @returns Rows without equivalent repeated records.
#' @noRd
.mp_unique_rows <- function(data, keys, caller) {
  # Repeated export rows with the same value are equivalent. Different values,
  # units or exclusions at the same key are ambiguous, not candidates to average.
  value_cols <- intersect(c("AVAL", "AVALU", "PPSUMXF"), names(data))
  data <- data[!duplicated(data[c(keys, "PPCAT", value_cols)]), , drop = FALSE]
  if (anyDuplicated(data[c(keys, "PPCAT")])) {
    stop(caller, ": ambiguous parent/metabolite values for the same subject, ",
         "parameter and profile.")
  }
  data
}

#' Select ordinary ADPP inputs for an explicit parent/metabolite pair
#' @param data ADPP data.
#' @param parent,metabolite Single analyte names from PPCAT.
#' @param caller Calling TLG function for diagnostics.
#' @returns Raw parameter rows for the pair, excluding already-derived ratios.
#' @noRd
.mp_input_data <- function(data, parent, metabolite, caller) {
  valid_name <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  if (!valid_name(parent) || !valid_name(metabolite) || identical(parent, metabolite)) {
    stop(caller, ": provide distinct parent and metabolite analyte names from PPCAT.")
  }
  required <- c("USUBJID", "PPCAT", "PARAMCD", "AVAL")
  missing <- setdiff(required, names(data))
  if (length(missing)) stop(caller, ": missing ADPP columns: ", paste(missing, collapse = ", "))
  data <- data[data$PPCAT %in% c(parent, metabolite), , drop = FALSE]
  if ("PPANMETH" %in% names(data)) {
    data <- data[is.na(.ratio_row_type(data$PPANMETH)), , drop = FALSE]
  }
  data
}

#' Restrict M/P outputs to the calculated value and unit columns
#' @param value_var Numeric output column; must be AVAL.
#' @param unit_var Output unit column; must be AVALU.
#' @returns Invisibly NULL, or an error for unsupported columns.
#' @noRd
.mp_check_columns <- function(value_var, unit_var = "AVALU") {
  if (!identical(value_var, "AVAL")) {
    stop("M/P outputs: value_var must be 'AVAL' (the calculated ratio).", call. = FALSE)
  }
  if (!identical(unit_var, "AVALU")) {
    stop("M/P listings: unit_var must be 'AVALU' (the ratio unit).", call. = FALSE)
  }
  invisible(NULL)
}

#' Calculate individual metabolite/parent ratios from ordinary ADPP values
#'
#' Explicit analyte names keep this usable outside Shiny. The app derives these
#' names from ADNCA with `.mp_analyte_pairs()`. Neither missing matches nor
#' duplicate denominators are replaced with another subject's value or an average.
#' @param data ADPP data.
#' @param parent,metabolite Single analyte names from PPCAT.
#' @param caller Calling TLG function for diagnostics.
#' @param summary Exclude ratios if either input has PPSUMXF == "Y".
#' @returns ADPP-shaped ratio rows with RATIO and RATIOREF display columns.
#' @importFrom dplyr left_join
#' @noRd
.mp_ratio_data <- function(data, parent, metabolite, caller, summary = FALSE) {
  data <- .mp_input_data(data, parent, metabolite, caller)
  keys <- intersect(c(
    "STUDYID", "USUBJID", "PARAMCD", "PPSPEC", "TRT01A", "TRT01P", "DOSETRT",
    "ATPTREF", "AVISIT", "AVISITN", "APERIOD", "APERIODC", "PERIOD", "ROUTE",
    "PPSTINT", "PPENINT", "DOSEA", "DOSEU"
  ), names(data))
  if (anyNA(data[intersect(c("STUDYID", "USUBJID", "PARAMCD"), keys)])) {
    stop(caller, ": missing subject, study or parameter identifiers; cannot pair M/P values.")
  }
  if (!"AVALU" %in% names(data)) data$AVALU <- rep("", nrow(data))
  if (!"PPSUMXF" %in% names(data)) data$PPSUMXF <- rep("", nrow(data))
  data <- .mp_unique_rows(data, keys, caller)
  numerator <- data[data$PPCAT == metabolite, , drop = FALSE]
  denominator <- data[data$PPCAT == parent, c(keys, "AVAL", "AVALU", "PPSUMXF"), drop = FALSE]
  names(denominator)[match(c("AVAL", "AVALU", "PPSUMXF"), names(denominator))] <-
    c(".mp_parent_value", ".mp_parent_unit", ".mp_parent_excluded")
  out <- left_join(numerator, denominator, by = keys)
  out$PPSUMXF <- ifelse(out$PPSUMXF %in% "Y" | out$.mp_parent_excluded %in% "Y", "Y", "")
  if (summary) out <- out[out$PPSUMXF != "Y", , drop = FALSE]
  .mp_finish_ratios(out, parent, metabolite, caller)
}

#' Finish matched M/P values and their display metadata
#' @param data Matched numerator and denominator rows.
#' @param parent,metabolite Analyte names.
#' @param caller Calling TLG function for diagnostics.
#' @returns Finite ratios with dimensionless units and explicit analyte labels.
#' @noRd
.mp_finish_ratios <- function(data, parent, metabolite, caller) {
  units <- function(x) ifelse(is.na(x), "", as.character(x))
  factor <- get_conversion_factor(units(data$.mp_parent_unit), units(data$AVALU))
  denominator <- data$.mp_parent_value * factor
  value <- data$AVAL / denominator
  valid <- is.finite(data$AVAL) & is.finite(data$.mp_parent_value) &
    is.finite(denominator) & denominator != 0 & is.finite(value)
  if (!any(valid)) {
    stop(caller, ": no usable M/P values. Check the pair, exclusions, matching profiles, ",
         "units and non-missing/non-zero parent values.")
  }
  if (any(!valid)) {
    .tlg_warn(caller, ": skipped ", sum(!valid), " unpaired or invalid M/P value(s) ",
              "(missing/non-finite values, zero parent or incompatible units).")
  }
  data <- data[valid, , drop = FALSE]
  data$AVAL <- value[valid]
  data$AVALU <- "fraction"
  if ("AVALC" %in% names(data)) data$AVALC <- as.character(data$AVAL)
  data$RATIO <- paste(metabolite, parent, sep = " / ")
  data$RATIOREF <- parent
  attr(data$AVAL, "label") <- "Metabolite / Parent Ratio"
  attr(data$RATIO, "label") <- "Metabolite / Parent"
  attr(data$RATIOREF, "label") <- "Parent (reference analyte)"
  data[c(".mp_parent_value", ".mp_parent_unit", ".mp_parent_excluded")] <- NULL
  # Missing optional intervals/visits are valid for some parameters. Keep these
  # groups visible when other rows have the corresponding identifier populated.
  for (key in .mp_profile_vars(data)) {
    if (anyNA(data[[key]])) {
      data[[key]] <- as.character(data[[key]])
      data[[key]][is.na(data[[key]])] <- "(unspecified)"
    }
  }
  data
}
