# Does a set of validation findings block saving?

Predicate used by the export/Save flow to refuse a save when any
`error`-severity finding is present. `warning`-severity findings (for
example selected custom grouping columns) do not block saving.

## Usage

``` r
cdisc_validation_blocks_save(findings)
```

## Arguments

- findings:

  A findings data frame as returned by
  [`validate_cdisc_types()`](https://pharmaverse.github.io/aNCA/reference/validate_cdisc_types.md).

## Value

`TRUE` when at least one `error`-severity finding exists, otherwise
`FALSE`.

## Examples

``` r
findings <- validate_cdisc_types(list())
cdisc_validation_blocks_save(findings)
#> [1] FALSE
```
