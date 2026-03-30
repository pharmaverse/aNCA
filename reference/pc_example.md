# pc_example

An SDTM PC-like pharmacokinetic concentrations dataset derived from
[`adnca_example`](https://pharmaverse.github.io/aNCA/reference/adnca_example.md).
Intended to support and test the PC+EX upload workflow (see GitHub issue
\#624).

## Usage

``` r
pc_example
```

## Format

A data frame with the same number of rows as `adnca_example` and 11
variables:

- DOMAIN:

  Character. Domain Abbreviation.

- STUDYID:

  Character. Study identifier.

- USUBJID:

  Character. Unique subject identifier.

- PCTEST:

  Character. Name of the measured analyte (mapped from PARAM).

- PCSPEC:

  Character. Specimen type (e.g., SERUM, URINE).

- PCSTRESN:

  Numeric. Numeric concentration result (mapped from AVAL).

- PCSTRESU:

  Character. Concentration units (mapped from AVALU).

- PCDTC:

  Character. Sample collection datetime (ISO 8601), synthesised from
  AFRLT using an arbitrary reference origin.

- PCRFTDTC:

  Character. Reference dose datetime (ISO 8601), synthesised from
  AFRLT - ARRLT.

- PCELTM:

  Character. Planned elapsed time from reference dose in ISO 8601
  duration format (e.g., "PT0.5H"), derived from NFRLT.

- VOLUME:

  Numeric. Volume of specimen collected (mL), for urine samples.

- VOLUMEU:

  Character. Volume units.

## Source

Derived from
[`adnca_example`](https://pharmaverse.github.io/aNCA/reference/adnca_example.md)
and
[`dm_example`](https://pharmaverse.github.io/aNCA/reference/dm_example.md)
via `data-raw/sdtm_example.R`.
