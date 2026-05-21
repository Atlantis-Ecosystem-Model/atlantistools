# Load information for SSB and Recruits from an Atlantis model run.

Load information for SSB and Recruits from an Atlantis model run.

## Usage

``` r
load_rec(yoy, ssb, prm_biol)
```

## Arguments

- yoy:

  Character string giving the connection of the YOY file. The filename
  usually contains `outputYOY` and ends in `.txt`".

- ssb:

  Character string giving the connection of the YOY file. The filename
  usually contains `outputSSB` and ends in `.txt`".

- prm_biol:

  Character string giving the connection to the biological
  parameterfile. The filename usually contains `biol_fishing` and does
  end in `.prm`.

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
[`load_spec_mort()`](https://andybeet.github.io/atlantistools/reference/load_spec_mort.md),
[`load_spec_pred_mort()`](https://andybeet.github.io/atlantistools/reference/load_spec_pred_mort.md),
[`load_txt()`](https://andybeet.github.io/atlantistools/reference/load_txt.md)

## Examples

``` r
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
yoy <- file.path(d, "outputSETASYOY.txt")
ssb <- file.path(d, "outputSETASSSB.txt")
prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")

load_rec(yoy, ssb, prm_biol)
#> # A tibble: 8 × 4
#>   species  time   ssb     rec
#>   <chr>   <dbl> <dbl>   <dbl>
#> 1 FPS        0     0  103637.
#> 2 FVS        0     0  330283.
#> 3 FPS      365  1320.  61643.
#> 4 FVS      365  6010. 717322.
#> 5 FPS      730   992.   2041.
#> 6 FVS      730  2365.  67151.
#> 7 FPS     1094.  889.    799.
#> 8 FVS     1094. 1748.  50751.
```
