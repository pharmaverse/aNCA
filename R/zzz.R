#' @importFrom utils globalVariables
.onLoad <- function(libname, pkgname) {
  # Implemented to remedy global binding issues with non-standard evaluation (e.g. in dplyr) while
  # running R CMD check.
  utils::globalVariables(c(
    ".", ":=", "AFRLT", "AFRLT.dose", "ANALYTE", "ARRLT", "AVAL", "CI_lower", "CI_upper",
    "Count.missing", "Count.total", "DOSEA", "DOSNO", "Dataset", "EVID", "Geocv", "Geomean", "IX",
    "IX_color", "IX_shape", "IX_stroke", "Label", "Max", "Mean", "Median", "Min", "N", "NRRLT",
    "PARAM", "PCRFTDTM", "PCSEQ", "PCSPEC", "PPORRES", "PPORRESU", "PPSTRES", "PPSTRESC",
    "PPSTRESN", "RANGE", "Ratio", "Ratio_Type", "SD", "SD_max", "SD_min", "SE", "STUDYID",
    "Spec1_Label", "Spec1_Units", "Spec1_Value", "Spec2_Label", "Spec2_Units", "Spec2_Value",
    "TIME", "USUBJID", "Unit", "Value", "data", "digits", "dose_time", "end", "end_dose", "exclude",
    "exclude_half.life", "format_fun", "id_list", "id_plot", "install.packages",
    "interval_name", "interval_name_col", "interval_next", "interval_prev",
    "is.excluded.hl", "is.included.hl", "lambda.z", "lambda.z.ix", "lambda.z.method",
    "lambda.z.n.points", "lambda.z.time.first", "log10_CI", "log10_Mean", "log10_SD",
    "log10_SE", "next_dose", "next_nom_dose",
    "nom_dose_time", "nom_interval_next", "nom_interval_prev",
    "start", "start_dose", "time_dose", "type_interval", "var_name", "view",
    "zero_str", "PPTESTCD", "PPSTRESU", "PPTESTCD_unit"
  ))
}
