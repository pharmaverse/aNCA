# Mapping between uppercase CDISC dataset names and lowercase keys
CDISC_DS_KEY_MAP <- c(PP = "pp", ADPP = "adpp", ADNCA = "adnca")

#' Generate Pre-Specifications for CDISC Datasets
#'
#' Extracts variable-level metadata from `metadata_nca_variables` for the
#' requested CDISC datasets (ADNCA, ADPP, PP). When `cdisc_data` is provided,
#' specs are narrowed to only the variables present in each dataset.
#'
#' @param datasets Character vector of dataset names to generate specs for.
#'   Defaults to `c("ADNCA", "ADPP", "PP")`.
#' @param cdisc_data Optional named list of data frames (as returned by
#'   `export_cdisc()`). Names should be lowercase (`pp`, `adpp`, `adnca`).
#'   When provided, each spec is filtered to only variables present as columns
#'   in the corresponding data frame.
#'
#' @returns A named list of data frames, one per dataset, each containing
#'   columns: Dataset, Order, Variable, Label, Type, Role, Core, Length.
#'
#' @importFrom writexl write_xlsx
#' @export
generate_pre_specs <- function(datasets = c("ADNCA", "ADPP", "PP"),
                               cdisc_data = NULL) {
  spec_cols <- c("Dataset", "Order", "Variable", "Label", "Type", "Role", "Core", "Length")

  all_specs <- metadata_nca_variables %>%
    filter(Dataset %in% datasets) %>%
    select(all_of(spec_cols)) %>%
    arrange(Dataset, Order) %>%
    split(.[["Dataset"]])

  if (!is.null(cdisc_data)) {
    for (ds_name in names(all_specs)) {
      key <- CDISC_DS_KEY_MAP[[ds_name]]
      if (!is.null(key) && key %in% names(cdisc_data)) {
        present_vars <- names(cdisc_data[[key]])
        all_specs[[ds_name]] <- all_specs[[ds_name]] %>%
          filter(Variable %in% present_vars)
      }
    }
  }

  all_specs
}
