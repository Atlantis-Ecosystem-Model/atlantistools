# Load mortality information from outputMort.txt

Loads Mort.txt file and partitions mortality based on fishing (F) and
other mortality (M) Note: As the Atlantis manual states "This file is
currenlty only useful for looking at **relative M vs F values** for a
species, as it does not give accurate mortalities". Also if a species is
set as isImpacted in the *functional_group.csv*, it will have some F
value even if it is not explicity targeted by fishing.

## Usage

``` r
load_mort(mortFile, prm_run, fgs, convert_names = F)
```

## Arguments

- mortFile:

  Character string giving the path to the Mort.txt file. The filename
  usually contains `Mort` and ends in `.txt`".

- prm_run:

  Character string giving the connection of the run parameterfile. The
  filename usually contains `run_fishing` and ends in `.prm`".

- fgs:

  Character string giving the connection to the functional groups file.
  The filename usually contains `Groups` and does end in `.csv`.

- convert_names:

  Logical indicating if group codes are transformed to LongNames
  (`TRUE`) or not (default = `FALSE`).

## Value

Data frame with information about sources of mortality (M, F).

## See also

Other load functions:
[`load_box()`](https://andybeet.github.io/atlantistools/reference/load_box.md),
[`load_bps()`](https://andybeet.github.io/atlantistools/reference/load_bps.md),
[`load_dietcheck()`](https://andybeet.github.io/atlantistools/reference/load_dietcheck.md),
[`load_fgs()`](https://andybeet.github.io/atlantistools/reference/load_fgs.md),
[`load_init()`](https://andybeet.github.io/atlantistools/reference/load_init.md),
[`load_init_age()`](https://andybeet.github.io/atlantistools/reference/load_init_age.md),
[`load_nc()`](https://andybeet.github.io/atlantistools/reference/load_nc.md),
[`load_nc_physics()`](https://andybeet.github.io/atlantistools/reference/load_nc_physics.md),
[`load_rec()`](https://andybeet.github.io/atlantistools/reference/load_rec.md),
[`load_spec_mort()`](https://andybeet.github.io/atlantistools/reference/load_spec_mort.md),
[`load_spec_pred_mort()`](https://andybeet.github.io/atlantistools/reference/load_spec_pred_mort.md),
[`load_txt()`](https://andybeet.github.io/atlantistools/reference/load_txt.md)

## Examples

``` r
d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
mortFile <- file.path(d, "outputSETASMort.txt")
prm_run <- file.path(d, "VMPA_setas_run_fishing_F_New.prm")
fgs <- file.path(d, "SETasGroups.csv")

df <- load_mort(mortFile, prm_run, fgs)
head(df)
#> # A tibble: 6 × 4
#>    time code  source atoutput
#>   <dbl> <chr> <chr>     <dbl>
#> 1     1 BB    F             0
#> 2     2 BB    F             0
#> 3     3 BB    F             0
#> 4     4 BB    F             0
#> 5     5 BB    F             0
#> 6     6 BB    F             0

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
mortFile <- file.path(d, "outputSETASMort.txt")
prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")

df <- load_mort(mortFile, prm_run, fgs)
head(df)
#> # A tibble: 6 × 4
#>    time code  source atoutput
#>   <dbl> <chr> <chr>     <dbl>
#> 1     1 BB    F             0
#> 2     2 BB    F             0
#> 3     3 BB    F             0
#> 4     4 BB    F             0
#> 5     5 BB    F             0
#> 6     6 BB    F             0
```
