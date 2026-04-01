# Create a PKNCAdata Object for NCA or Slope Analysis

This function updates a previously prepared `PKNCAdata` object based on
user selections for method, analyte, dose, specimen, and parameters.

## Usage

``` r
PKNCA_update_data_object(
  adnca_data,
  method,
  selected_analytes,
  selected_profile,
  selected_pcspec,
  start_impute = TRUE,
  hl_adj_rules = NULL,
  exclusion_list = NULL,
  keep_interval_cols = NULL,
  min_hl_points = 3,
  parameter_selections = NULL,
  int_parameters = NULL,
  blq_imputation_rule = NULL,
  custom_units_table = NULL
)
```

## Arguments

- adnca_data:

  A reactive PKNCAdata object

- method:

  NCA calculation method selection

- selected_analytes:

  User selected analytes

- selected_profile:

  User selected dose numbers/profiles

- selected_pcspec:

  User selected specimen

- start_impute:

  Logical indicating whether to impute start concentration values. Also
  forwarded to
  [`update_main_intervals()`](https://pharmaverse.github.io/aNCA/reference/update_main_intervals.md)
  when `parameter_selections` is provided.

- hl_adj_rules:

  A data frame containing half-life adjustment rules. It must contain
  group columns and rule specification columns; TYPE: (Inclusion,
  Exclusion), RANGE: (start-end).

- exclusion_list:

  List of exclusion reasons and row indices to apply to the
  concentration data. Each item in the list should have:

  - reason: character string with the exclusion reason (e.g.,
    "Vomiting")

  - rows: integer vector of row indices to apply the exclusion to

- keep_interval_cols:

  Optional character vector of additional columns to keep in the
  intervals data frame and when the NCA is run (pk.nca) also in the
  results

- min_hl_points:

  Minimum number of points to use for half-life calculation. Must be
  \>= 2. Default is 3 (PKNCA default).

- parameter_selections:

  Optional named list of selected PKNCA parameters by study type
  (forwarded to
  [`update_main_intervals()`](https://pharmaverse.github.io/aNCA/reference/update_main_intervals.md)).

- int_parameters:

  Optional data frame containing partial AUC ranges (forwarded to
  [`update_main_intervals()`](https://pharmaverse.github.io/aNCA/reference/update_main_intervals.md)).

- blq_imputation_rule:

  Optional list defining the BLQ imputation rule (forwarded to
  [`update_main_intervals()`](https://pharmaverse.github.io/aNCA/reference/update_main_intervals.md)).

- custom_units_table:

  Optional data frame with PPSTRESU overrides. When provided, applied
  via
  [`dplyr::rows_update()`](https://dplyr.tidyverse.org/reference/rows.html)
  on the PKNCAdata units table.

## Value

A fully configured `PKNCAdata` object.

## Details

Step 1: Update units in the `PKNCAdata` object ensuring unique analytes
have their unique units

Step 2: Set `PKNCAoptions` for NCA calculation

Step 3: Format intervals using
[`format_pkncadata_intervals()`](https://pharmaverse.github.io/aNCA/reference/format_pkncadata_intervals.md)

Step 4: Apply filtering based on user selections and partial aucs

Step 5: Impute start values if requested

Step 6: Indicate points excluded / selected manually for half-life

Step 7 (optional): Update intervals with parameter selections per study
type and partial AUC ranges via
[`update_main_intervals()`](https://pharmaverse.github.io/aNCA/reference/update_main_intervals.md).

Step 8 (optional): Apply custom units table for PPSTRESU overrides.

Note\*: The function assumes that the `adnca_data` object has been
created using the
[`PKNCA_create_data_object()`](https://pharmaverse.github.io/aNCA/reference/PKNCA_create_data_object.md)
function.
