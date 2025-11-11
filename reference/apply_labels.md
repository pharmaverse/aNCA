# Apply Labels to a dataset

This function adds "label" attributes to all columns in a dataset

## Usage

``` r
apply_labels(data, labels_df = metadata_nca_variables, type = "ADPC")
```

## Arguments

- data:

  The dataset to which labels will be applied.

- labels_df:

  A data frame containing at least the columns "Variable", "Label", and
  "Dataset".

- type:

  The type variable in labels_df for which the labels are to be applied.

## Value

The same dataset with label attributes applied to all columns. If a
column is not present in the labels list, it will be assigned the name
of the col. If label already exists in the original data, it will be
preserved.

## Examples

``` r
 data <- data.frame(USUBJID = c(1, 2, 3), AVAL = c(4, 5, 6))
 labels <- data.frame(
   Variable = c("USUBJID", "AVAL"),
   Label = c("Unique Subject Identifier", "Analysis Value"),
   Dataset = c("ADPC", "ADPC")
 )
 data <- apply_labels(data, labels, "ADPC")
 print(attr(data$USUBJID, "label")) # "Unique Subject Identifier"
#> [1] "Unique Subject Identifier"
 print(attr(data$AVAL, "label"))    # "Analysis Value"
#> [1] "Analysis Value"
```
