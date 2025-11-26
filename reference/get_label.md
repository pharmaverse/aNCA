# Get the Label of a Heading

This function retrieves the label of a heading from a labels file.

## Usage

``` r
get_label(variable, type = "ADNCA", labels_df = metadata_nca_variables)
```

## Arguments

- variable:

  The variable for which the label is to be retrieved.

- type:

  The type of the dataset for which the label is to be retrieved.

- labels_df:

  A data frame containing at least the columns "Variable", "Label", and
  "Dataset".

## Value

The label of the heading if it exists in the labels file, otherwise the
variable name.

## Examples

``` r
 LABELS <- data.frame(
   Variable = c("USUBJID", "AVAL"),
   Label = c("Unique Subject Identifier", "Analysis Value"),
   Dataset = c("ADNCA", "ADNCA")
 )
 get_label("USUBJID", "ADNCA", LABELS)  # Returns "Unique Subject Identifier"
#> [1] "Unique Subject Identifier"
 get_label("AGE", "ADNCA", LABELS)  # Returns "AGE"
#> [1] "AGE"
```
