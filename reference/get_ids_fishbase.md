# Extract fishbase IDs using the package "rfishbase" to generate species specific fishbase URLs

This function extracts fishbase IDs using the database provided by the
`rfishbase` package.

## Usage

``` r
get_ids_fishbase(fish)
```

## Arguments

- fish:

  Vector of fish species with genus and species information.

## Value

named vector with species names and fishbase IDs.

## Details

The function depends on the package "rfishbase" which creates a local
copy of the fishbase database. The IDs are needed to generate URLs to
scan www.fishbase.se for detailed informations about fish growth for
example.

## Examples

``` r
fish <- c("Gadus morhua", "Merlangius merlangus", "Clupea harengus")
get_ids_fishbase(fish)
#>         Gadus morhua Merlangius merlangus      Clupea harengus 
#>                   69                   29                   24 
```
