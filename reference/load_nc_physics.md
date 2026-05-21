# Load Atlantis outputfiles (netcdf)

This function loads Atlantis outputfiles (netcdf) and converts them to a
dataframe.

## Usage

``` r
load_nc_physics(
  nc,
  select_physics,
  prm_run,
  bboxes,
  aggregate_layers = FALSE,
  warn_zeros = FALSE
)
```

## Arguments

- nc:

  Character string giving the connection of the netcdf file to read in.
  The filename usually contains `output` and ends in `.nc`".

- select_physics:

  Character vector of physical variables which shall be read in. Names
  have to match the ones used in the ncdf file.

- prm_run:

  Character string giving the connection of the run parameterfile. The
  filename usually contains `run_fishing` and ends in `.prm`".

- bboxes:

  Integer vector giving the box-id of the boundary boxes. Can be created
  with `get_boundary`.

- aggregate_layers:

  Logical indicating if values for layers should be aggregated (`TRUE`)
  or not (`FALSE`). Default is `FALSE`.

- warn_zeros:

  Logical indicating if check for actual zeros in the data shall be
  printed or not. Default is `FALSE`.

## Value

A `data.frame` in long format with the following column names: variable,
time, polygon, layer, and atoutput (i.e., variable).

## Details

This functions converts the ATLANTIS output to a dataframe which can be
processed in R.

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
[`load_rec()`](https://andybeet.github.io/atlantistools/reference/load_rec.md),
[`load_spec_mort()`](https://andybeet.github.io/atlantistools/reference/load_spec_mort.md),
[`load_spec_pred_mort()`](https://andybeet.github.io/atlantistools/reference/load_spec_pred_mort.md),
[`load_txt()`](https://andybeet.github.io/atlantistools/reference/load_txt.md)

## Examples

``` r

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
nc <- file.path(d, "outputSETAS.nc")
prm_run <- file.path(d, "VMPA_setas_run_fishing_F_Trunk.prm")
bboxes <- get_boundary(boxinfo = load_box(file.path(d, bgm = "VMPA_setas.bgm")))
select_physics = c("salt", "NO3", "NH3", "Temp", "Chl_a", "Denitrifiction")

test <- load_nc_physics(nc, select_physics, prm_run, bboxes)
head(test)
#>    variable polygon layer time     atoutput
#> 32    Chl_a       1     0    1 1.195206e+00
#> 33    Chl_a       1     1    1 9.336022e-01
#> 34    Chl_a       1     2    1 2.946814e-02
#> 35    Chl_a       1     3    1 2.166251e-03
#> 36    Chl_a       1     6    1 2.147856e+02
#> 37    Chl_a       2     0    1 1.590395e-01

test <- load_nc_physics(nc, select_physics = "nominal_dz", prm_run, bboxes)
head(test)
#>     variable polygon layer time atoutput
#> 1 nominal_dz       1     0    0       10
#> 2 nominal_dz       1     1    0       50
#> 3 nominal_dz       1     2    0       30
#> 4 nominal_dz       1     3    0       20
#> 5 nominal_dz       1     6    0        1
#> 6 nominal_dz       2     0    0      216
```
