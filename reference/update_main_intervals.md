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
  impute = TRUE
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

## Value

An updated PKNCAdata object with parameter intervals based on user
selections.
