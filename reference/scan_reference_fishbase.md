# Scan list of references for character string for fish species

Scan list of references for character string for fish species

## Usage

``` r
scan_reference_fishbase(fish, chr, mirror = "se")
```

## Arguments

- fish:

  Vector of fish species with genus and species information.

- chr:

  Character string to search.

- mirror:

  Character string defining the url mirror to use. Defaults to `se`. In
  case data extraction is slow use a different mirror. Try to avoid
  frequently used mirrors like `uk` or `com`.

## Value

Dataframe of potentially relevant references.

## Examples

``` r
if (FALSE) { # \dontrun{
# For some reason the examples break with appveyor.
fish <- c("Gadus morhua", "Merlangius merlangus")
df <- scan_reference_fishbase(fish, chr = "diet")
df <- scan_reference_fishbase(fish, chr = "xxx")
df <- scan_reference_fishbase(fish, chr = "feed")
} # }
```
