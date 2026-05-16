# Load mortality information from outputSpecificPredMort.txt

Load mortality information from outputSpecificPredMort.txt

## Usage

``` r
load_spec_pred_mort(
  specmort,
  prm_run,
  fgs,
  convert_names = FALSE,
  version_flag = 2
)
```

## Arguments

- specmort:

  Character string giving the connection of the specific mortality file.
  The filename usually contains `SpecificPredMort` and ends in `.txt`".

- prm_run:

  Character string giving the connection of the run parameterfile. The
  filename usually contains `run_fishing` and ends in `.prm`".

- fgs:

  Character string giving the connection to the functional groups file.
  The filename usually contains `Groups` and does end in `.csv`.

- convert_names:

  Logical indicating if group codes are transformed to LongNames
  (`TRUE`) or not (default = `FALSE`).

- version_flag:

  The version of ATLANTIS model. 1 for bec_dev, 2 for trunk.
  `default is 2.`.

## Value

Dataframe with information about ssb in tonnes and recruits in
thousands.

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
[`load_spec_mort()`](https://andybeet.github.io/atlantistools/reference/load_spec_mort.md),
[`load_txt()`](https://andybeet.github.io/atlantistools/reference/load_txt.md)

## Examples

``` r
d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
specmort <- file.path(d, "outputSETASSpecificPredMort.txt")
prm_run <- file.path(d, "VMPA_setas_run_fishing_F_New.prm")
fgs <- file.path(d, "SETasGroups.csv")

df <- load_spec_pred_mort(specmort, prm_run, fgs, version_flag = 1)
head(df)
#> # A tibble: 6 × 5
#>    time pred  agecl prey       atoutput
#>   <dbl> <chr> <dbl> <chr>         <dbl>
#> 1   0.2 CEP       1 CEP   0.00000000883
#> 2   0.2 FVS       1 FVS   0.00000962   
#> 3   0.4 CEP       1 CEP   0.0000000100 
#> 4   0.4 FVS       1 FVS   0.00000962   
#> 5   0.6 CEP       1 CEP   0.00000000898
#> 6   0.6 CEP       2 FPS   0.000161     

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
specmort <- file.path(d, "outputSETASSpecificPredMort.txt")
prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")

df <- load_spec_pred_mort(specmort, prm_run, fgs)
#> Error in dplyr::arrange(tidyr::pivot_longer(data, cols = dplyr::setdiff(names(data),     id_col), names_to = "code", values_to = "atoutput"), id_col,     code): ℹ In argument: `..1 = id_col`.
#> Caused by error:
#> ! `..1` must be size 1512 or 1, not 4.
head(df)
#> # A tibble: 6 × 5
#>    time pred  agecl prey       atoutput
#>   <dbl> <chr> <dbl> <chr>         <dbl>
#> 1   0.2 CEP       1 CEP   0.00000000883
#> 2   0.2 FVS       1 FVS   0.00000962   
#> 3   0.4 CEP       1 CEP   0.0000000100 
#> 4   0.4 FVS       1 FVS   0.00000962   
#> 5   0.6 CEP       1 CEP   0.00000000898
#> 6   0.6 CEP       2 FPS   0.000161     
```
