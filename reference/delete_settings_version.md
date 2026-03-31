# Delete a version from a versioned settings list

Removes the version at the given index. Cannot delete the last remaining
version.

## Usage

``` r
delete_settings_version(versions, index)
```

## Arguments

- versions:

  List of version entries.

- index:

  Integer index of the version to remove.

## Value

Updated list of version entries.
