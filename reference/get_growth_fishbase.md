# Extract growth parameters from http:://www.fishbase.se.

This function extracts values for Linf, k and t0 from
http:://www.fishbase.se

## Usage

``` r
get_growth_fishbase(fish, mirror = "se")
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

## Details

Before the actual extraction takes place fishbase IDs for every species
are extracted using
[`get_ids_fishbase`](https://andybeet.github.io/atlantistools/reference/get_ids_fishbase.md).
The IDs are needed to generate the urls later on.

## Examples

``` r
if (FALSE) { # \dontrun{
# For some reason the examples break with appveyor.
fish <- c("Gadus morhua", "Merlangius merlangus")
df <- get_growth_fishbase(fish)
head(df)

df <- get_growth_fishbase(fish, mirror = "de")
head(df)

fish <- c("Sprattus sprattus")
df <- get_growth_fishbase(fish)
head(df)
# Only use for debugging purposes.
fish <- read.csv("Z:/my_data_alex/fish_species_names_from_ibts.csv", stringsAsFactors = FALSE)[, 1]
url <- get_growth_fishbase(fish)
url <- urls$ref_url
} # }
```
