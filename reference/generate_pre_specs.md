# Generate Pre-Specifications for CDISC Datasets

Extracts variable-level metadata from `metadata_nca_variables` for the
requested CDISC datasets (ADNCA, ADPP, PP). When `cdisc_data` is
provided, specs are narrowed to only the variables present in each
dataset.

## Usage

``` r
generate_pre_specs(datasets = c("ADNCA", "ADPP", "PP"), cdisc_data = NULL)
```

## Arguments

- datasets:

  Character vector of dataset names to generate specs for. Defaults to
  `c("ADNCA", "ADPP", "PP")`.

- cdisc_data:

  Optional named list of data frames (as returned by
  [`export_cdisc()`](https://pharmaverse.github.io/aNCA/reference/export_cdisc.md)).
  Names should be lowercase (`pp`, `adpp`, `adnca`). When provided, each
  spec is filtered to only variables present as columns in the
  corresponding data frame.

## Value

A named list of data frames, one per dataset, each containing columns:
Dataset, Order, Variable, Label, Type, Role, Core, Length.
