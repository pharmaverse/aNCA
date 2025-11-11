# metadata_nca_parameters

A dataset containing the mapping between PKNCA terms and CDISC terms. It
mainly involves:

- Parameters: PPTEST, PPTESTCD

## Usage

``` r
metadata_nca_parameters
```

## Format

A data frame with 123 rows and 6 variables:

- PKNCA:

  PKNCA term

- PPTESTCD:

  CDISC term

- PPTEST:

  Official CDISC term

- input_names:

  Combination of PPTESTCD + ": " + PPTEST. Used for App inputs

- FUN:

  PKNCA function used to calculate the parameter

- description:

  PKNCA description of the term

- is_cdisc_sure:

  Logical indicating if the term is a CDISC official name

- unit_type:

  Type of unit associated with the term

- TYPE:

  Type of data associated with the parameter/term

- CAT:

  Arbitrary assigned subclass for the parameter/term

- Depends:

  PKNCA derived. Designates all directly used parameters in calculation

- can_excretion:

  Logical. Indicates if the parameter can be used in excretion analysis

- can_non_excretion:

  Logical. Indicates if the parameter can be used in non-excretion
  analysis

- can_single_dose:

  Logical. Indicates if the parameter can be used in single dose
  analysis

- can_multiple_dose:

  Logical. Indicates if the parameter can be used in multiple dose
  analysis

- can_extravascular:

  Logical. Indicates if the parameter can be used in extravascular
  analysis

## Source

Generated for use in the `translate_nomenclature` function.
