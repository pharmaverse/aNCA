# Translate Terms from One Nomenclature to Another

This function translates a character vector of terms from one
nomenclature to another using a mapping file.

## Usage

``` r
translate_terms(
  input_terms,
  mapping_col = "PKNCA",
  target_col = "PPTESTCD",
  metadata = metadata_nca_parameters
)
```

## Arguments

- input_terms:

  A character vector of terms to be translated.

- mapping_col:

  Character indicating the column name in the metadata file of the input
  terms. Default is "PKNCA".

- target_col:

  Character indicating the column name in the metadata file of the
  target terms. Default is "PPTESTCD".

- metadata:

  Dataset used to do the mapping that contains the mapping and target
  columns.

## Value

A character vector of translated terms. Input terms not available in
mapping_col will be returned with the same value.

## Examples

``` r
input_terms <- c("adj.r.squared", "ae", "nonexistent_term")
translate_terms(input_terms)
#> [1] "R2ADJ"            "RCAMINT"          "nonexistent_term"
```
