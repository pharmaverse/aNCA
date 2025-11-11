# Reads PK datasets from various file formats.

Reads PK datasets from various file formats.

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
df <- read_pk(system.file("shiny/data/Dummy_data.csv", package = "aNCA"))
```
