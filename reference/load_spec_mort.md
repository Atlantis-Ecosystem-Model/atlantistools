# Load mortality information from specificMort.txt

Reads in the specificMort.txt file. Three values of instantaneous
mortality for each functional group, age group, and stock. Predation
(M2), other natural mortality (M1), Fishing (F)

## Usage

``` r
load_spec_mort(mortFile, prm_run, fgs, convert_names = FALSE, removeZeros = T)
```

## Arguments

- mortFile:

  Character string giving the path to the specificMort.txt file. The
  filename usually contains `SpecificMort` and ends in `.txt`".

- prm_run:

  Character string giving the connection of the run parameterfile. The
  filename usually contains `run_fishing` and ends in `.prm`".

- fgs:

  Character string giving the connection to the functional groups file.
  The filename usually contains `Groups` and does end in `.csv`.

- convert_names:

  Logical indicating if group codes are transformed to LongNames
  (`TRUE`) or not (default = `FALSE`).

- removeZeros:

  Boolean. Remove all zeros from output. (Default = T)

## Value

Data frame with information about sources of mortality (M1, M2, F).

## See also

Other load functions:
[`load_box()`](https://andybeet.github.io/atlantistools/reference/load_box.md),
[`load_bps()`](https://andybeet.github.io/atlantistools/reference/load_bps.md),
[`load_dietcheck()`](https://andybeet.github.io/atlantistools/reference/load_dietcheck.md),
[`load_fgs()`](https://andybeet.github.io/atlantistools/reference/load_fgs.md),
[`load_init()`](https://andybeet.github.io/atlantistools/reference/load_init.md),
[`load_init_age()`](https://andybeet.github.io/atlantistools/reference/load_init_age.md),
[`load_mort()`](https://andybeet.github.io/atlantistools/reference/load_mort.md),
[`load_nc()`](https://andybeet.github.io/atlantistools/reference/load_nc.md),
[`load_nc_physics()`](https://andybeet.github.io/atlantistools/reference/load_nc_physics.md),
[`load_rec()`](https://andybeet.github.io/atlantistools/reference/load_rec.md),
[`load_spec_pred_mort()`](https://andybeet.github.io/atlantistools/reference/load_spec_pred_mort.md),
[`load_txt()`](https://andybeet.github.io/atlantistools/reference/load_txt.md)

## Examples

``` r

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
specmort <- file.path(d, "outputSETASSpecificMort.txt")
prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")

df <- load_spec_mort(specmort, prm_run, fgs)
head(df)
#> # A tibble: 6 × 5
#>    time code  agecl mort  atoutput
#>   <dbl> <chr> <dbl> <chr>    <dbl>
#> 1     1 BML       1 M1    1.27e-15
#> 2     2 BML       1 M1    1.06e-15
#> 3     1 CEP       1 M1    3.82e- 5
#> 4     2 CEP       1 M1    1.87e-13
#> 5     1 CEP       1 M2    6.80e+ 8
#> 6     2 CEP       1 M2    4.39e+ 0
```
