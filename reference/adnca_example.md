# adnca_example

A pharmacokinetic concentration-time dataset containing multiple dose
administration data with specimen types (SERUM and URINE), dosing
information, and subject demographics. This is an example dataset for
demonstrating pharmacokinetic analysis workflows. It follows the
ADaMIG-ADNCA v1.0 structure.

## Usage

``` r
adnca_example
```

## Format

A data frame with 76 rows and 32 variables:

- STUDYID:

  Character. Study identifier.

- USUBJID:

  Character. Unique subject identifier.

- PCSPEC:

  Character. Specimen type (SERUM or URINE).

- PARAM:

  Character. Parameter name (e.g., DrugA).

- METABFL:

  Character. Metabolite flag indicator.

- AFRLT:

  Numeric. Actual relative time to first dose (hours).

- NFRLT:

  Numeric. Nominal relative time to first dose (hours).

- ARRLT:

  Numeric. Actual relative time to reference dose (hours).

- NRRLT:

  Numeric. Nominal relative time to reference dose (hours).

- TRTRINT:

  Numeric. Treatment interval in hours.

- RRLTU:

  Character. Relative reference time units.

- AVAL:

  Numeric. Actual analysis value (concentration).

- AVALU:

  Character. Analysis value units (ug/mL or mg/mL).

- VOLUME:

  Numeric. Volume of specimen collected (mL for urine).

- VOLUMEU:

  Character. Volume units.

- DOSETRT:

  Character. Dose treatment identifier.

- TRT01A:

  Character. Actual treatment received (formatted dose and route).

- DOSEA:

  Numeric. Actual dose amount (mg).

- DOSEU:

  Character. Dose units.

- ROUTE:

  Character. Route of administration.

- ADOSEDUR:

  Numeric. Actual dose duration (hours).

- ATPTREF:

  Character. Analysis time point reference (e.g., DOSE 1).

- AGE:

  Numeric. Subject age.

- AGEU:

  Character. Age units.

- RACE:

  Character. Race/ethnicity of subject.

- SEX:

  Character. Sex of subject.

- NCA1XRS:

  Character. NCA result code 1 (if applicable).

- NCA2XRS:

  Character. NCA result code 2 (if applicable).

- NEFRLT:

  Numeric. Nominal relative time to first dose (alternative column).

- NERRLT:

  Numeric. Nominal relative time to reference dose (alternative column).

- AEFRLT:

  Numeric. Actual relative time to first dose (alternative column).

- AERRLT:

  Numeric. Actual relative time to reference dose (alternative column).

- DOSFRM:

  Character. Dose formulation (e.g., "TABLET").

## Source

Example data created for package demonstration and testing purposes.

## Details

This dataset contains pharmacokinetic data from multiple subjects
receiving multiple dose regimens with different levels and routes of
administration at 24-hour intervals. Concentration measurements are
collected at multiple time points post-dose for both serum and urine
samples. Analytes include parent drug and a metabolite.
