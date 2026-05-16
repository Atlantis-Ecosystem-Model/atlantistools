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

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
specmort <- file.path(d, "outputSETASSpecificPredMort.txt")
prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")

df <- load_spec_pred_mort(specmort, prm_run, fgs)
#> Error in dplyr::arrange(tidyr::pivot_longer(data, cols = dplyr::setdiff(names(data),     id_col), names_to = "code", values_to = "atoutput"), id_col,     code): ℹ In argument: `..1 = id_col`.
#> Caused by error:
#> ! `..1` must be size 1512 or 1, not 4.
head(df)
#>                                               
#> 1 function (x, df1, df2, ncp, log = FALSE)    
#> 2 {                                           
#> 3     if (missing(ncp))                       
#> 4         .Call(C_df, x, df1, df2, log)       
#> 5     else .Call(C_dnf, x, df1, df2, ncp, log)
#> 6 }                                           
```
