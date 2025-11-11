# Convert Volume Units to Match Concentration Denominator Units

This function identifies rows associated with excretion samples (e.g.,
urine, feces, bile) and adjusts the `VOLUME` and `VOLUMEU` columns so
that the volume unit matches the denominator unit in the corresponding
concentration unit (`AVALU`). This is necessary for PKNCA calculation of
excretion parameters.

## Usage

``` r
convert_volume_units(
  df,
  avalu = "AVALU",
  volume = "VOLUME",
  volumeu = "VOLUMEU"
)
```

## Arguments

- df:

  A data frame containing pharmacokinetic data.

- avalu:

  A character string specifying the column name for concentration values
  (default: "AVALU").

- volume:

  A character string specifying the column name for volume or mass
  values (default: "VOLUME").

- volumeu:

  A character string specifying the column name for volume or mass units
  (default: "VOLUMEU"). It must contain the following columns:

  PCSPEC

  :   Sample type (e.g., urine, feces, bile, plasma).

  AVAL

  :   Concentration values.

  AVALU

  :   Concentration units (e.g., "ug/mL", "mg/g").

  VOLUME

  :   Volume or mass values for integration.

  VOLUMEU

  :   Units for the `VOLUME` column (e.g., "mL", "g").

## Value

A modified data frame with `VOLUME` and `VOLUMEU` converted (where
necessary) so that multiplying `AVAL * VOLUME` results in a unit with
consistent dimensionality (typically mass or moles). A new column
`AMOUNTU` is created to represent the product of `AVALU` and `VOLUMEU`.

## Details

It uses the units package to perform unit-safe conversions. If a direct
conversion between volume and the concentration denominator is not
possible (e.g., between mass and volume), a fallback conversion is
attempted using a neutral density of `1 (target_unit / original_unit)`.
The function modifies only the `VOLUME` and `VOLUMEU` columns when
necessary and leaves all other data unchanged.

The function:

1.  Parses the denominator from `AVALU` (e.g., "ug/mL" → "mL").

2.  Attempts to convert the corresponding `VOLUME` to that unit.

3.  If direct conversion fails, assumes a neutral density of 1 (i.e.,
    `1 unit_target / unit_original`) and retries.

4.  Leaves units unchanged for non-excreta samples or already-valid
    combinations.

The function assumes that the `AVALU` column contains concentration
units in the form of "x/y" (e.g., "ug/mL", "mg/g").

## Examples

``` r
df <- data.frame(
  PCSPEC = c("urine", "feces", "plasma"),
  AVAL = c(100, 5, 70),
  AVALU = c("ug/mL", "mg/g", "ng/mL"),
  VOLUME = c(2, 1.5, 3),
  VOLUMEU = c("L", "mL", "mL"),
  stringsAsFactors = FALSE
)

df_converted <- convert_volume_units(df)
#> Row 1: Converted volume from 2 L to 2000 mL based on concentration unit ug/mL
#> Row 2: Converted volume from 1.5 mL to 1.5 g based on concentration unit mg/g
```
