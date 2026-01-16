# Helper function to add "label" attribute to columns based on parameter names.

Helper function to add "label" attribute to columns based on parameter
names.

## Usage

``` r
add_label_attribute(df, myres)
```

## Arguments

- df:

  Data frame to which labels will be added.

- myres:

  The output of PKNCA::pk.nca.

## Value

Data frame with "label" attributes added to specified columns.

## Examples

``` r
# 1. Setup example data
df <- data.frame(a = c(5.2, 6.3), b = c(50.1, 60.2))
names(df) <- c("CMAX[ng/mL]", "AUCLST[hr*ng/mL]")

# 2. Setup mock metadata (myres)
myres <- list(
  result = data.frame(
    PPTESTCD = c("CMAX", "AUCLST"),
    PPSTRESU = c("ng/mL", "hr*ng/mL"),
    type_interval = "main",
    start = 0,
    end = 24
  )
)

# 3. Apply labels
labeled_df <- add_label_attribute(df, myres)

# 4. Check label
attr(labeled_df[["CMAX[ng/mL]"]], "label")
#> [1] "Max Conc"
```
