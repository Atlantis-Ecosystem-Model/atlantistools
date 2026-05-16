# Extract conversion factor used to transform data from nitrogen in mg to biomass in tonnes.

Extract conversion factor used to transform data from nitrogen in mg to
biomass in tonnes.

## Usage

``` r
get_conv_mgnbiot(prm_biol)
```

## Arguments

- prm_biol:

  Character string giving the connection to the biological
  parameterfile. The filename usually contains `biol_fishing` and does
  end in `.prm`.

## Value

Conversion factor as numeric value.

## See also

Other get functions:
[`get_boundary()`](https://andybeet.github.io/atlantistools/reference/get_boundary.md),
[`get_colpal()`](https://andybeet.github.io/atlantistools/reference/get_colpal.md),
[`get_groups()`](https://andybeet.github.io/atlantistools/reference/get_groups.md)

## Examples

``` r
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")

get_conv_mgnbiot(prm_biol)
#> [1] 1.14e-07
```
