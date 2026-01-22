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
  should_impute_c0 = TRUE,
  exclusion_list = NULL,
  keep_interval_cols = NULL
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

- should_impute_c0:

  Logical indicating whether to impute start concentration values

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

Note\*: The function assumes that the `adnca_data` object has been
created using the
[`PKNCA_create_data_object()`](https://pharmaverse.github.io/aNCA/reference/PKNCA_create_data_object.md)
function.
