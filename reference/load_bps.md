# Extracts the names of the epibenthic biomasspools from the initial conditions file.

Use `fgs` `data.frame` as read in by
[`load_fgs`](https://andybeet.github.io/atlantistools/reference/load_fgs.md)
to get the biomass pool information.

## Usage

``` r
load_bps(fgs, init)
```

## Arguments

- fgs:

  Character string giving the connection to the functional groups file.
  The filename usually contains `Groups` and does end in `.csv`.

- init:

  Character string giving the connection of the initial conditions
  netcdf file. The filename usually contains `init` and ends in `.nc`.

## Value

Character `vector` of epibenthic biomass pools.

## See also

[`load_fgs`](https://andybeet.github.io/atlantistools/reference/load_fgs.md)

Other load functions:
[`load_box()`](https://andybeet.github.io/atlantistools/reference/load_box.md),
[`load_dietcheck()`](https://andybeet.github.io/atlantistools/reference/load_dietcheck.md),
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
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
init <- file.path(d, "INIT_VMPA_Jan2015.nc")

load_bps(fgs, init)
#> [1] "Megazoobenthos"
```
