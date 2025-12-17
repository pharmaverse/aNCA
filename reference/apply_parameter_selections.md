# Helper to Apply Default or Override Parameter Selections

Populates a selection data frame with boolean columns for each study
type, indicating which parameters are selected based on either default
rules or a provided override list.

## Usage

``` r
apply_parameter_selections(
  selection_df,
  study_type_names,
  default_params,
  selections_override = NULL
)
```

## Arguments

- selection_df:

  A data frame containing PK parameters and their metadata. Must include
  a 'PKNCA' column and logical columns for various attributes (e.g.,
  'can_excretion', 'can_single_dose').

- study_type_names:

  A character vector of study type names to generate selection columns
  for.

- default_params:

  A character vector of default PKNCA parameters to select.

- selections_override:

  An optional named list where names correspond to study types and
  values are character vectors of PKNCA parameters to select. If NULL,
  default logic is applied.

## Value

The 'selection_df' data frame with added boolean columns for each study
type.
