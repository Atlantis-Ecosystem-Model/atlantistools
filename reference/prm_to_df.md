# Extract parameters from the biological parameter file and transform them to a dataframe.

Extract parameters from the biological parameter file and transform them
to a dataframe.

## Usage

``` r
prm_to_df(prm_biol, fgs, group, parameter)

prm_to_df_ages(prm_biol, fgs, group, parameter)
```

## Arguments

- prm_biol:

  Character string giving the connection to the biological
  parameterfile. The filename usually contains `biol_fishing` and does
  end in `.prm`.

- fgs:

  Character string giving the connection to the functional groups file.
  The filename usually contains `Groups` and does end in `.csv`.

- group:

  Character vector giving the functional Groups to extract.

- parameter:

  Character vector giving the parameters to extract.

## Value

Dataframe with columns 'species' and as many columns as parameters.

## Examples

``` r
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
group <- c("FPS", "FVS")
parameter <- c("mum", "C")

prm_to_df_ages(prm_biol, fgs, group, parameter)
#> # A tibble: 20 × 4
#>    species                  agecl     mum        c
#>    <chr>                    <int>   <dbl>    <dbl>
#>  1 Small planktivorous fish     1   0.003   0.0002
#>  2 Small planktivorous fish     2   0.5     0.3   
#>  3 Small planktivorous fish     3   0.5     0.6   
#>  4 Small planktivorous fish     4   0.5     0.6   
#>  5 Small planktivorous fish     5   0.5     0.6   
#>  6 Small planktivorous fish     6   0.5     0.6   
#>  7 Small planktivorous fish     7   0.5     0.5   
#>  8 Small planktivorous fish     8   0.5     0.5   
#>  9 Small planktivorous fish     9   0.5     0.4   
#> 10 Small planktivorous fish    10   0.5     0.4   
#> 11 Shallow piscivorous fish     1 150      40     
#> 12 Shallow piscivorous fish     2 150      40     
#> 13 Shallow piscivorous fish     3 150      40     
#> 14 Shallow piscivorous fish     4 250     120     
#> 15 Shallow piscivorous fish     5 250     150     
#> 16 Shallow piscivorous fish     6 250     250     
#> 17 Shallow piscivorous fish     7 250     250     
#> 18 Shallow piscivorous fish     8 250     300     
#> 19 Shallow piscivorous fish     9 250     300     
#> 20 Shallow piscivorous fish    10 250     300     
prm_to_df(prm_biol, fgs, group, parameter)
#>                    species  c mum
#> 1 Small planktivorous fish 10  10
#> 2 Shallow piscivorous fish 10  10
```
