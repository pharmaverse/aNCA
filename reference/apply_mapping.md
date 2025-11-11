# Apply UI-Based Column Mapping to a Dataset

This function takes a dataset and applies user-specified column mappings
provided through a Shiny `input` object. It renames columns based on the
selected mappings, handles special cases such as `ADOSEDUR`, updates
units for key variables, applies labels, and removes detected duplicate
concentration records.

## Usage

``` r
apply_mapping(
  dataset,
  mapping,
  desired_order,
  silent = TRUE,
  req_mappings = c("USUBJID", "AFRLT", "NFRLT", "ARRLT", "NRRLT", "PCSPEC", "ROUTE",
    "AVAL", "STUDYID", "ATPTREF", "AVALU", "RRLTU", "DOSEU", "PARAM")
)
```

## Arguments

- dataset:

  A data frame containing the raw data to be transformed.

- mapping:

  A named list of column mappings.

- desired_order:

  A character vector specifying the desired column order in the output
  dataset.

- silent:

  Boolean, whether to print message with applied mapping. Defaults to
  `TRUE`.

- req_mappings:

  A character vector indicating the names of the mapping object that
  must always be populated

## Value

A transformed data frame with:

- Renamed columns according to user mappings

- Created columns for mapped non-existent variables (e.g., `mg/L` in
  `AVALU`)

- Created columns for unmapped required variables (e.g., `ADOSEDUR`)

- Ordered columns as specified

- Labels applied to columns (via
  [`apply_labels()`](https://pharmaverse.github.io/aNCA/reference/apply_labels.md))

- Concentration duplicates removed based on key identifiers: `AFRLT`,
  `STUDYID`, `PCSPEC`, `DOSETRT`, `USUBJID`, and `PARAM`

## Details

- Validates that all required columns are mapped and no duplicates
  exist.

- If `ADOSEDUR` is not mapped, it is assigned a value of `0`.

- Removes concentration data duplicates using all columns except
  `ARRLT`, `NRRLT`, and `ATPTREF`.
