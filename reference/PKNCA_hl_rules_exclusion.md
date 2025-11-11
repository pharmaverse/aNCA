# Exclude NCA results based on user-defined rules over the half-life related parameters This function applies exclusion rules to the NCA results based on user-defined parameters.

Exclude NCA results based on user-defined rules over the half-life
related parameters This function applies exclusion rules to the NCA
results based on user-defined parameters.

## Usage

``` r
PKNCA_hl_rules_exclusion(res, rules)
```

## Arguments

- res:

  A PKNCAresults object containing the NCA results.

- rules:

  A list of exclusion rules where each rule is a named vector.

## Value

A PKNCAresults object with the exclusions applied.

## Details

The function iterates over the rules and applies the exclusion criteria
to the NCA results. For any parameter that is not aucpext.obs or
aucpext.pred it applies a minimum threshold, and for aucpext.obs and
aucpext.pred it applies a maximum threshold.
