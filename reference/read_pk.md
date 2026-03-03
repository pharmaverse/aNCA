# Reads datasets from various file formats.

Reads datasets from various file formats.

## Usage

``` r
read_pk(path)
```

## Arguments

- path:

  Character string with path to the dataset file.

## Value

A data.frame object with loaded data.

## Details

Currently supported file formats include:

- `rds`

- `xlsx`

- `sas7bdat`

- `xpt`

- `parquet`

## Examples

``` r
path <- system.file("shiny/tests/testthat/dummy_simplified.csv", package = "aNCA")
df <- read_pk(path)
```
