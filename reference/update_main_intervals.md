# Update an intervals data frame with user-selected parameters by study type

Update an intervals data frame with user-selected parameters by study
type

## Usage

``` r
update_main_intervals(
  data,
  parameter_selections,
  study_types_df,
  auc_data,
  impute = TRUE,
  blq_imputation_rule = NULL
)
```

## Arguments

- data:

  A PKNCAdata object containing intervals and dosing data.

- parameter_selections:

  A named list of selected PKNCA parameters by study type.

- study_types_df:

  A data frame mapping analysis profiles to their study type.

- auc_data:

  A data frame containing partial AUC ranges.

- impute:

  Logical indicating whether to impute start values for parameters.

- blq_imputation_rule:

  A list defining the Below Limit of Quantification (BLQ) imputation
  rule using PKNCA format. The list should either contain three elements
  named: `first`, `middle`, and `last` or two elements named
  `before.tmax` and `after.tmax`. Each element can be a numeric value
  (substituting the BLQ value), or a string such as `"drop"` (ignores
  the value) or `"keep"` (keeps the value as 0). Default is NULL, which
  does not specify any BLQ imputation in any interval.

## Value

An updated PKNCAdata object with parameter intervals based on user
selections.
