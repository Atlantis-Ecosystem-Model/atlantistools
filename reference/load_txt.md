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
d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
file <- file.path(d, "outputSETASSSB.txt")
load_txt(file)
#> # A tibble: 32 × 3
#>     time code  atoutput
#>    <dbl> <chr>    <dbl>
#>  1     0 FPS      1321.
#>  2    73 FPS      1318.
#>  3   146 FPS      1314.
#>  4   219 FPS       986.
#>  5   292 FPS       984.
#>  6   365 FPS       982.
#>  7   438 FPS       979.
#>  8   511 FPS       976.
#>  9   584 FPS       871.
#> 10   657 FPS       870.
#> # ℹ 22 more rows

file <- file.path(d, "outputSETASYOY.txt")
load_txt(file)
#> # A tibble: 32 × 3
#>     time code  atoutput
#>    <dbl> <chr>    <dbl>
#>  1     0 FPS.0   1.22  
#>  2    73 FPS.0   1.22  
#>  3   146 FPS.0   1.22  
#>  4   219 FPS.0   0.724 
#>  5   292 FPS.0   0.724 
#>  6   365 FPS.0   0.724 
#>  7   438 FPS.0   0.724 
#>  8   511 FPS.0   0.724 
#>  9   584 FPS.0   0.0240
#> 10   657 FPS.0   0.0240
#> # ℹ 22 more rows
```
