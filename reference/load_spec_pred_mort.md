# Load mortality information from outputSpecificPredMort.txt

Load mortality information from outputSpecificPredMort.txt

## Usage

``` r
load_spec_pred_mort(specmort, prm_run, fgs, convert_names = FALSE)
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

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
specmort <- file.path(d, "outputSETASSpecificPredMort.txt")
prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")

df <- load_spec_pred_mort(specmort, prm_run, fgs)
head(df)
#> # A tibble: 6 × 5
#>    time pred  agecl prey  atoutput
#>   <dbl> <chr> <dbl> <chr>    <dbl>
#> 1     1 CEP       1 CEP   4.73e+ 3
#> 2     1 CEP       1 FVS   6.80e+ 8
#> 3     1 CEP       2 CEP   3.92e-27
#> 4     1 CEP       2 FVS   9.74e- 5
#> 5     1 DC        1 BML   3.26e- 7
#> 6     1 DL        1 BML   3.23e- 9
```
