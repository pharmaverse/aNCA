# metadata_nca_variables

A dataset containing pharmacokinetic variable specifications.

## Usage

``` r
metadata_nca_variables
```

## Format

A data frame with 361 rows and 14 variables:

- Dataset:

  Character. Indicates the dataset the variable belongs to (PP, ADPC,
  ADPP).

- Order:

  Numeric. Variable order within its domain, based on Role, Core and
  Variable

- Variable:

  Character. The short name of the variable.

- Label:

  Character. A descriptive label for the variable.

- Type:

  Character. Data type of the variable (Char, Num, text, integer, float,
  dateTime).

- Role:

  Character. The CDISC role of the variable (e.g., Identifier, Topic,
  Timing...).

- Core:

  Character. Indicates the core status of the variable (Req = Required,
  Perm = Permissible, Exp = Expected, Cond = Conditional).

- company_specific:

  Logical. Indicates if the variable is company-specific (not CDISC).

- is.core:

  Logical. TRUE if the variable is a core variable (always needed to be
  present).

- Length:

  Numeric. The maximum length of the variable.

- Controlled_Terms:

  Character. Reference to controlled terminology (e.g., C85839, C66731).

- is.used:

  Logical. TRUE if the variable is meant to be included.

- Values:

  Character. Possible values (if applicable) for the variable separated
  by ', '.

- is.mapped:

  Logical. TRUE if the variable is mapped in ADNCA (App's input).

- mapping_tooltip:

  Character. Tooltip text for mapping guidance in the App.

- mapping_section:

  Character. Mapping section where the variable is classified in the
  App.

- mapping_alternatives:

  Character. Alternative column names for the variable.

- mapping_order:

  Numeric. Defines the mapped variables order in the mapped dataset

## Source

Used for PP and ADPP mapping rules and checks in the export_cdisc
function
