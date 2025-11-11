# Create a PKNCAdata Object for NCA or Slope Analysis

This function updates a previously prepared `PKNCAdata` object based on
user selections for method, analyte, dose, specimen, and parameters.

## Usage

``` r
PKNCA_update_data_object(
  adnca_data,
  auc_data,
  method,
  selected_analytes,
  selected_profile,
  selected_pcspec,
  params,
  should_impute_c0 = TRUE
)
```

## Arguments

- adnca_data:

  A reactive PKNCAdata object

- auc_data:

  A data frame containing partial aucs added by user

- method:

  NCA calculation method selection

- selected_analytes:

  User selected analytes

- selected_profile:

  User selected dose numbers/profiles

- selected_pcspec:

  User selected specimen

- params:

  A list of parameters for NCA calculation

- should_impute_c0:

  Logical indicating if start values should be imputed

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
