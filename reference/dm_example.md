# dm_example

An SDTM DM-like demographics dataset derived from
[`adnca_example`](https://pharmaverse.github.io/aNCA/reference/adnca_example.md).
Contains one row per subject with demographic variables and a synthetic
treatment start date (RFXSTDTC) that serves as the reference origin for
datetimes in
[`pc_example`](https://pharmaverse.github.io/aNCA/reference/pc_example.md)
and
[`ex_example`](https://pharmaverse.github.io/aNCA/reference/ex_example.md).

## Usage

``` r
dm_example
```

## Format

A data frame with one row per subject and 9 variables:

- DOMAIN:

  Character. Domain Abbreviation.

- STUDYID:

  Character. Study identifier.

- USUBJID:

  Character. Unique subject identifier.

- AGE:

  Numeric. Age of the subject.

- AGEU:

  Character. Age units (e.g., "Years").

- SEX:

  Character. Sex of the subject.

- RACE:

  Character. Race of the subject.

- ARM:

  Character. Planned treatment arm (mapped from TRT01A).

- ACTARM:

  Character. Actual treatment arm (mapped from TRT01A).

- RFXSTDTC:

  Character. Date/time of first study treatment (ISO 8601). Synthetic,
  staggered by 3 days per subject to simulate different enrollment
  dates.

## Source

Derived from
[`adnca_example`](https://pharmaverse.github.io/aNCA/reference/adnca_example.md)
via `data-raw/sdtm_example.R`.
