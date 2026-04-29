# Export CDISC Data

This function processes the results from a PKNCA and exports them into
CDISC compliant datasets. Attention: All parameters that do no match
pptest dataframe will be lost in this pipeline!

## Usage

``` r
export_cdisc(res_nca, grouping_vars = character(0), flag_rules = NULL)
```

## Arguments

- res_nca:

  Object with results of the NCA analysis. If `res_nca$result` contains
  a `.pp_excl` column (logical), excluded rows are merged into the
  `exclude` column so they appear in `PPSUMFL`/`PPSUMRSN`. If
  `.pp_excl_reason` (character) is also present, it populates
  `PPSUMRSN`.

- grouping_vars:

  Character vector of non-standard grouping variable names to include as
  additional columns in ADNCA, ADPP, and PP outputs. Defaults to
  `character(0)`.

- flag_rules:

  Character vector of flag rule exclusion messages applied during NCA
  (e.g., `c("R2ADJ < 0.8", "AUCPEO > 20")`). Each entry generates a
  CRITy/CRITyFL column pair in ADPP, plus PPSUMFL and PPSUMRSN columns.
  Defaults to `NULL` (no flags).

## Value

A list with two data frames:

- pp:

  A data frame containing the PP (Pharmacokinetic Parameters) domain
  data.

- adpp:

  A data frame containing the ADPP (Analysis Dataset for Pharmacokinetic
  Parameters) domain data.

## Details

Outputs are the following:

- pknca_result Output from function call `pk.nca()` (formatted)

- pknca_result_raw Output from function call `pk.nca()` (needs to be
  merged with upper later on but now we avoid merge conflict)
