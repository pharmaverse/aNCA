# Do export findings block saving?

Predicate used by the export/Save flow to refuse a save when any
`error`-severity finding is present in the output of
[`validate_export_outputs()`](https://pharmaverse.github.io/aNCA/reference/validate_export_outputs.md).

## Usage

``` r
export_validation_blocks_save(findings)
```

## Arguments

- findings:

  A findings data frame as returned by
  [`validate_export_outputs()`](https://pharmaverse.github.io/aNCA/reference/validate_export_outputs.md).

## Value

`TRUE` when at least one `error`-severity finding exists, otherwise
`FALSE`.

## Examples

``` r
findings <- validate_export_outputs(list())
export_validation_blocks_save(findings)
#> [1] FALSE
```
