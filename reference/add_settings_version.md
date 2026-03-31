# Add a new version to an existing versioned settings list

Prepends a new version entry so it becomes the current version. The
previous current entry moves to the `previous` list.

## Usage

``` r
add_settings_version(versions, new_version)
```

## Arguments

- versions:

  Existing list of version entries.

- new_version:

  A single version entry (from
  [`create_settings_version()`](https://pharmaverse.github.io/aNCA/reference/create_settings_version.md)).

## Value

Updated list of version entries with the new entry first.
