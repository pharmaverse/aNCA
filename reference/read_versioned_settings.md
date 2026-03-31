# Read a versioned settings YAML file

Parses a YAML file in the versioned format (with `current` and
optionally `previous` keys). Returns a list with `versions` (a list of
version entries, most recent first).

## Usage

``` r
read_versioned_settings(path, obj = NULL)
```

## Arguments

- path:

  Character string with path to the YAML file.

- obj:

  Optional pre-parsed YAML list. When supplied, `path` is ignored and no
  file I/O occurs.

## Value

A list with element `versions`.
