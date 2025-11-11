# Detect study types

This function detects the type of study based on the provided data.

## Usage

``` r
detect_study_types(
  data,
  groups,
  metabfl_column,
  route_column,
  volume_column = "volume"
)
```

## Arguments

- data:

  The dataset containing the study types to be identified. Assumed to be
  the output from aNCA formatted concentration data. Must contain group
  columns, the specified route, analyte and drug columns, and ADOSEDUR.

- groups:

  the grouping variables for the study type detection.

- metabfl_column:

  A character string specifying the column name for the metabolite flag,
  which is coded as "Y" for metabolites

- route_column:

  A character string specifying the column name for the route of
  administration.

- volume_column:

  A character string specifying the column name for the volume of a
  sample. Extravascular samples must be written as `extravascular`. Can
  be set to `volume` if not applicable.

## Value

A data frame summarizing the detected study types, including the
grouping columns and the identified type.

## Details

The function identifies a possible five different types of studies based
on grouping by `STUDYID`, `DOSETRT`, `USUBJID`, `PCSPEC`, and the route
column. The study types are determined as follows:

- "Excretion Data": If the volume column for a group is not NA and has
  values \> 0.

- "Single Extravascular Dose": If there is only one dose and TRTRINT is
  not available, and the route is extravascular.

- "Single IV Infusion Dose": If there is only one dose and TRTRINT is
  not present, and the route is not extravascular.

- "Single IV Bolus Dose": If there is only one dose and TRTRINT is not
  present, the route is not extravascular, and ADOSEDUR == 0.

- "Multiple Extravascular Doses": If there are multiple doses (or
  TRTRINT is available) and the route is extravascular.

- "Multiple IV Infusion Doses": If there are multiple doses (or TRTRINT
  is available) and the route is not extravascular.

- "Multiple IV Bolus Doses": If there are multiple doses or TRTRINT is
  not present, the route is not extravascular, and ADOSEDUR == 0.

- If none of these conditions are met, the type is marker as "Unknown".

## Examples

``` r
sample_data <- data.frame(
  STUDYID = "STUDY001",
  DOSETRT = "Drug",
  ANALYTE = "DOSETRT",
  USUBJID = c(
    # 1. Single IV Dose subject
    "Subj-01", "Subj-01",
    # 2. Multiple Extravascular Doses subject (identified by TRTRINT)
    "Subj-02", "Subj-02",
    # 3. Excretion Data subject (identified by positive volume)
    "Subj-03", "Subj-03"
  ),
  PCSPEC = "PLASMA",
  DOSNOA = c(
    1, 1,        # Single dose
    1, 1,        # Appears as single dose...
    1, 1         # Single dose
  ),
  ROUTE = c(
    "INTRAVENOUS", "INTRAVENOUS",
    "extravascular", "extravascular",
    "INTRAVENOUS", "INTRAVENOUS"
  ),
  ADOSEDUR = c(
  0, 0,
  0, 0,
  2, 2),
  SAMPLE_VOLUME = c(
    NA, 0,
    NA, 0,
    10, 12       # Positive volume indicates excretion
  ),
  TRTRINT = c(
    NA, NA,
    24, 24,      # ...but TRTRINT indicates a multiple-dose regimen
    NA, NA
  ),
  METABFL = c(
    "N", "N",
    "N", "N",
    "Y", "Y"    # mark last subject as metabolite
  )
)

study_summary <- detect_study_types(
  data = sample_data,
  groups = c("USUBJID", "PCSPEC", "DOSETRT"),
  metabfl_column = "METABFL",
  route_column = "ROUTE",
  volume_column = "SAMPLE_VOLUME"
)
```
