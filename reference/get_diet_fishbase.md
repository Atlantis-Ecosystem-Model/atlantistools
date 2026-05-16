# Extract reference for diet information from http:://www.fishbase.se

This function extracts reference for diet information from
http:://www.fishbase.se

## Usage

``` r
get_diet_fishbase(fish, mirror = "se")
```

## Arguments

- fish:

  Vector of fish species with genus and species information.

- mirror:

  Character string defining the url mirror to use. Defaults to `se`. In
  case data extraction is slow use a different mirror. Try to avoid
  frequently used mirrors like `uk` or `com`.

## Value

Dataframe with species, country, locality, linf and k.

## Examples

``` r
if (FALSE) { # \dontrun{
# For some reason the examples break with appveyor.
fish <- c("Gadus morhua", "Merlangius merlangus", "Maurolicus muelleri")
diet <- get_diet_fishbase(fish)

fish <- c("Gadus morhua", "Merlangius merlangus", "Ammodytes marinus")
diet <- get_diet_fishbase(fish)
} # }
```
