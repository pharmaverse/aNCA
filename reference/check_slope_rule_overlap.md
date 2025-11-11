# Check overlap between existing and new slope rulesets

Takes in tables with existing and incoming selections and exclusions,
finds any overlap and differences, edits the ruleset table accordingly.

## Usage

``` r
check_slope_rule_overlap(existing, new, slope_groups, .keep = FALSE)
```

## Arguments

- existing:

  Data frame with existing selections and exclusions.

- new:

  Data frame with new rule to be added or removed.

- slope_groups:

  List with column names that define the groups.

- .keep:

  Whether to force keep fully overlapping rulesets. If FALSE, it will be
  assumed that the user wants to remove rule if new range already exists
  in the dataset. If TRUE, in that case full range will be kept.

## Value

Data frame with full ruleset, adjusted for new rules.
