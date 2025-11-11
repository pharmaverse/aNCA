# Parses annotations in the context of data. Special characters and syntax are substituted by actual data and/or substituted for format that is better parsed via rendering functions (e.g. plotly).

Parses annotations in the context of data. Special characters and syntax
are substituted by actual data and/or substituted for format that is
better parsed via rendering functions (e.g. plotly).

## Usage

``` r
parse_annotation(data, text)
```

## Arguments

- data:

  Data frame containing data to reference. Should include columns and
  labels referenced in the text string. Referenced variables should be
  able to produce single unique result.

- text:

  Character text to parse.

## Value

Parsed annotation text.

## Details

- `\n` character is substituted for `<br>` tag in order to add new lines
  in rendered image.

- `$COLNAME` is parsed to provide unique data value from the mentioned
  column.

- `!COLNAME` is parsed to provide `label` attribute for a given column
  name. If any values are missing from the provided data, they are
  substituted for `ERR` string.
