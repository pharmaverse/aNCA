# Reshape PKNCA Results

This function reshapes the structure of the results produced by the main
function of the PKNCA package (pk.nca) in a way that each row represents
all the main results summarized for each profile in each
individual/subject. Excluding the ID variables, each column name
corresponds with a calculated parameter and between brackets its
corresponding units. AUC intervals, if present, are be added as
additional columns.

## Usage

``` r
pivot_wider_pknca_results(myres, flag_rules = NULL, extra_vars_to_keep = NULL)
```

## Arguments

- myres:

  The output of PKNCA::pk.nca. It makes some additional assumptions:

  1.  CDISC denomination of actual and nominal time variables (AFRLT,
      ARRLT, NFRLT, NRRLT).

  2.  Intervals must include a column (`type_interval`) to differentiate
      between the custom AUC ranges ("manual") and main parameter
      calculations ("main").

  3.  Includes `PPSTRES` and `PPSTRESU` variables in results dataset.

  4.  Columns `start_dose` and `end_dose` must express the actual start
      and end times of the dose, relative to the last reference dose.

  5.  Temporarily: CDISC denomination of PK parameters related to
      half-life: "LAMZNPT", "LAMZLL", "LAMZ" Used to derive `LAMZNPT`
      and `LAMZMTD`.

- flag_rules:

  A named list of flagging rules to be applied to the results. Each rule
  should be a list with two elements: `is.checked` (logical) indicating
  whether the rule should be applied, and `threshold` (numeric)
  specifying the threshold value for flagging. The name of each rule
  should correspond to a parameter in the results data.frame as a
  PPTESTCD (e.g., "R2ADJ", "AUCPEO", "AUCPEP", "LAMZSPN").

- extra_vars_to_keep:

  Optional character vector of variable names to join from the
  concentration data to the output. Default is NULL.

## Value

A data frame which provides an easy overview on the results from the NCA
in each profile/subject and how it was computed lambda (half life) and
the results of the NCA parameters (cmax, AUC, AUClast)
