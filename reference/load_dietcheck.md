# Read in the atlantis dietcheck.txt file and perform some basic data transformations.

Read in the atlantis dietcheck.txt file and perform some basic data
transformations.

## Usage

``` r
load_dietcheck(
  dietcheck,
  fgs,
  prm_run,
  convert_names = FALSE,
  report = FALSE,
  version_flag = 2
)
```

## Arguments

- dietcheck:

  Character string giving the connection of the dietcheck file. The
  filename usually contains `Dietcheck` and ends in `.txt`".

- fgs:

  Character string giving the connection to the functional groups file.
  The filename usually contains `Groups` and does end in `.csv`.

- prm_run:

  Character string giving the connection of the run parameterfile. The
  filename usually contains `run_fishing` and ends in `.prm`".

- convert_names:

  Logical indicating if group codes are transformed to LongNames
  (`TRUE`) or not (default = `FALSE`).

- report:

  Logical indicating if incomplete DietCheck information shall be
  printed `TRUE` or not `FALSE`.

- version_flag:

  The version of ATLANTIS model. 1 for bec_dev, 2 for trunk.
  `default is 2.`.

## Value

A `data.frame` in long format with the following column names: time,
pred, habitat, prey and atoutput (i.e., variable).

## See also

Other load functions:
[`load_box()`](https://andybeet.github.io/atlantistools/reference/load_box.md),
[`load_bps()`](https://andybeet.github.io/atlantistools/reference/load_bps.md),
[`load_fgs()`](https://andybeet.github.io/atlantistools/reference/load_fgs.md),
[`load_init()`](https://andybeet.github.io/atlantistools/reference/load_init.md),
[`load_init_age()`](https://andybeet.github.io/atlantistools/reference/load_init_age.md),
[`load_mort()`](https://andybeet.github.io/atlantistools/reference/load_mort.md),
[`load_nc()`](https://andybeet.github.io/atlantistools/reference/load_nc.md),
[`load_nc_physics()`](https://andybeet.github.io/atlantistools/reference/load_nc_physics.md),
[`load_rec()`](https://andybeet.github.io/atlantistools/reference/load_rec.md),
[`load_spec_mort()`](https://andybeet.github.io/atlantistools/reference/load_spec_mort.md),
[`load_spec_pred_mort()`](https://andybeet.github.io/atlantistools/reference/load_spec_pred_mort.md),
[`load_txt()`](https://andybeet.github.io/atlantistools/reference/load_txt.md)

## Examples

``` r
# Apply to bec-dev models.
d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
dietcheck <- file.path(d, "outputSETASDietCheck.txt")
fgs <- file.path(d, "SETasGroups.csv")
prm_run <- file.path(d, "VMPA_setas_run_fishing_F_New.prm")

diet <- load_dietcheck(dietcheck, fgs, prm_run, version_flag = 1)
#> Error in dplyr::arrange(tidyr::pivot_longer(diet, cols = prey_col_start:ncol(diet),     names_to = "prey", values_to = "atoutput"), Time, Predator,     Cohort, Updated, prey): ℹ In argument: `..3 = Cohort`.
#> Caused by error:
#> ! object 'Cohort' not found
head(diet, n = 10)
#> Error: object 'diet' not found

# Apply to trunk models.
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
dietcheck <- file.path(d, "outputSETASDietCheck.txt")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")
diet <- load_dietcheck(dietcheck, fgs, prm_run)
head(diet, n = 10)
#> # A tibble: 10 × 5
#>     time pred  agecl prey  atoutput
#>    <dbl> <chr> <dbl> <chr>    <dbl>
#>  1     1 BML       1 DL    0.231   
#>  2     1 BML       1 DR    0.769   
#>  3     1 CEP       1 CEP   0.648   
#>  4     1 CEP       1 FPS   0.352   
#>  5     1 CEP       2 FPS   1       
#>  6     1 FPS       1 DL    0.0929  
#>  7     1 FPS       1 DR    0.00043 
#>  8     1 FPS       1 PL    0.907   
#>  9     1 FPS       2 DL    0.0789  
#> 10     1 FPS       2 DR    0.000394
```
