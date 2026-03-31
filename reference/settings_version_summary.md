# Build a summary table of version entries

Creates a data.frame with one row per version, suitable for display in a
modal.

## Usage

``` r
settings_version_summary(versions)
```

## Arguments

- versions:

  List of version entries.

## Value

A data.frame with columns: `index`, `comment`, `datetime`, `dataset`,
`anca_version`, `tab`.
