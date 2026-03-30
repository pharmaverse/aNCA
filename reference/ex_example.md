# ex_example

An SDTM EX-like exposure/dosing dataset derived from
[`adnca_example`](https://pharmaverse.github.io/aNCA/reference/adnca_example.md).
Contains one row per dosing event per subject. Intended to support and
test the PC+EX upload workflow (see GitHub issue \#624).

## Usage

``` r
ex_example
```

## Format

A data frame with 11 variables:

- DOMAIN:

  Character. Domain abbreviation.

- STUDYID:

  Character. Study identifier.

- USUBJID:

  Character. Unique subject identifier.

- EXTRT:

  Character. Name of the treatment (mapped from DOSETRT).

- EXDOSE:

  Numeric. Dose amount (mapped from DOSEA).

- EXDOSU:

  Character. Dose units (mapped from DOSEU).

- EXROUTE:

  Character. Route of administration (mapped from ROUTE).

- EXSTDTC:

  Character. Dosing start datetime (ISO 8601), synthesised from AFRLT -
  ARRLT using the subject's RFXSTDTC as reference origin.

- EXENDTC:

  Character. Dosing end datetime (ISO 8601), computed as EXSTDTC +
  ADOSEDUR.

- EXDUR:

  Character. Duration of dose in ISO 8601 duration format (e.g.,
  "PT2.9H"), derived from ADOSEDUR.

- EXELTM:

  Character. Planned elapsed time from first dose in ISO 8601 duration
  format (e.g., "PT0H"), derived from NFRLT - NRRLT relative to the
  subject's first dose.

## Source

Derived from
[`adnca_example`](https://pharmaverse.github.io/aNCA/reference/adnca_example.md)
and
[`dm_example`](https://pharmaverse.github.io/aNCA/reference/dm_example.md)
via `data-raw/sdtm_example.R`.
