# Extract values for Atlantis parameters from the biological parameter file.

Extract values for Atlantis parameters from the biological parameter
file.

## Usage

``` r
extract_prm(prm_biol, variables)

extract_prm_cohort(prm_biol, variables)
```

## Arguments

- prm_biol:

  Character string giving the connection to the biological
  parameterfile. The filename usually contains `biol_fishing` and does
  end in `.prm`.

- variables:

  Character string giving the flag to search for. This should be a
  combination of the parameter name and the group-Code.

## Value

numeric vector.

## Examples

``` r
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")

# You can pass a single variable
extract_prm(prm_biol, variables = "KWRR_FVS")
#> [1] 7000

# Or multiple variables
extract_prm(prm_biol, variables = paste("KWRR", c("FVS", "FPS"), sep = "_"))
#> [1] 7.0e+03 7.5e-02

# Use extract_prm_cohort do extract data for age specific parameters.
# They are usually stored in the next line following the parameter tag.
extract_prm_cohort(prm_biol, variables = "C_FVS")
#> $C_FVS
#>  [1]  40  40  40 120 150 250 250 300 300 300
#> 
extract_prm_cohort(prm_biol, variables = paste("C", c("FVS", "FPS"), sep = "_"))
#> $C_FVS
#>  [1]  40  40  40 120 150 250 250 300 300 300
#> 
#> $C_FPS
#>  [1] 2e-04 3e-01 6e-01 6e-01 6e-01 6e-01 5e-01 5e-01 4e-01 4e-01
#> 
```
