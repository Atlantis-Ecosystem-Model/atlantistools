# Function to load various txt files from Atlantis simulations

Function to load various txt files from Atlantis simulations

## Usage

``` r
load_txt(file, id_col = "Time")
```

## Arguments

- file:

  Character string giving the connection of the output file. The
  filename usually contains `output` and ends in `.txt`".

- id_col:

  Character strings giving the names of the columns which are not
  variables. Data from all other columns will be gathered with tidyr.
  Default is `"Time"`.

## Value

Dataframe in tidy format!

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
[`load_spec_pred_mort()`](https://andybeet.github.io/atlantistools/reference/load_spec_pred_mort.md)

## Examples

``` r
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
file <- file.path(d, "outputSETASSSB.txt")
load_txt(file)
#> # A tibble: 8 × 3
#>    time code  atoutput
#>   <dbl> <chr>    <dbl>
#> 1    0  FPS         0 
#> 2    0  FVS         0 
#> 3  365  FPS      1320.
#> 4  365  FVS      6010.
#> 5  730  FPS       992.
#> 6  730  FVS      2365.
#> 7 1094. FPS       889.
#> 8 1094. FVS      1748.
```
