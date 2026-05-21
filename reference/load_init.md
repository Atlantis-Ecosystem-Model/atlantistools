# This function is used to read in data from the initial conditions file.

This function is used to read in data from the initial conditions file.

## Usage

``` r
load_init(init, vars)
```

## Arguments

- init:

  Character string giving the connection of the initial conditions
  netcdf file. The filename usually contains `init` and ends in `.nc`.

- vars:

  Vector of character strings giving the variables to extract from the
  netcdf file.

## Value

A list of dataframes with columns atoutput, polygon and layer (if
present).

## See also

Other load functions:
[`load_box()`](https://andybeet.github.io/atlantistools/reference/load_box.md),
[`load_bps()`](https://andybeet.github.io/atlantistools/reference/load_bps.md),
[`load_dietcheck()`](https://andybeet.github.io/atlantistools/reference/load_dietcheck.md),
[`load_fgs()`](https://andybeet.github.io/atlantistools/reference/load_fgs.md),
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
init <- file.path(d, "INIT_VMPA_Jan2015.nc")

load_init(init, vars = "Planktiv_S_Fish1_Nums")
#> [[1]]
#>    atoutput polygon layer
#> 1        NA       0     0
#> 2        NA       0     1
#> 3        NA       0     2
#> 4        NA       0     3
#> 5        NA       0     4
#> 6        NA       0    NA
#> 7        NA       0     6
#> 8   7258389       1     0
#> 9        NA       1     1
#> 10       NA       1     2
#> 11       NA       1     3
#> 12       NA       1    NA
#> 13       NA       1    NA
#> 14       NA       1     6
#> 15  5181795       2     0
#> 16       NA       2     1
#> 17       NA       2     2
#> 18       NA       2     3
#> 19       NA       2     4
#> 20       NA       2    NA
#> 21       NA       2     6
#> 22  7270500       3     0
#> 23       NA       3     1
#> 24       NA       3     2
#> 25       NA       3     3
#> 26       NA       3     4
#> 27       NA       3    NA
#> 28       NA       3     6
#> 29  9743202       4     0
#> 30       NA       4     1
#> 31       NA       4     2
#> 32       NA       4     3
#> 33       NA       4     4
#> 34       NA       4     5
#> 35       NA       4     6
#> 36  8944112       5     0
#> 37       NA       5     1
#> 38       NA       5     2
#> 39       NA       5     3
#> 40       NA       5     4
#> 41       NA       5     5
#> 42       NA       5     6
#> 43       NA       6     0
#> 44       NA       6     1
#> 45       NA       6     2
#> 46       NA       6     3
#> 47       NA       6     4
#> 48       NA       6    NA
#> 49       NA       6     6
#> 50       NA       7     0
#> 51       NA       7     1
#> 52       NA       7     2
#> 53       NA       7     3
#> 54       NA       7     4
#> 55       NA       7     5
#> 56       NA       7     6
#> 57       NA       8     0
#> 58       NA       8     1
#> 59       NA       8     2
#> 60       NA       8     3
#> 61       NA       8     4
#> 62       NA       8     5
#> 63       NA       8     6
#> 64       NA       9     0
#> 65       NA       9     1
#> 66       NA       9     2
#> 67       NA       9     3
#> 68       NA       9     4
#> 69       NA       9     5
#> 70       NA       9     6
#> 71       NA      10     0
#> 72       NA      10     1
#> 73       NA      10     2
#> 74       NA      10     3
#> 75       NA      10     4
#> 76       NA      10     5
#> 77       NA      10     6
#> 
load_init(init, vars = c("Planktiv_S_Fish2_ResN", "Planktiv_S_Fish3_ResN"))
#> [[1]]
#>    atoutput polygon layer
#> 1        18       0     0
#> 2        18       0     1
#> 3        18       0     2
#> 4        18       0     3
#> 5        18       0     4
#> 6        18       0    NA
#> 7        18       0     6
#> 8        18       1     0
#> 9        18       1     1
#> 10       18       1     2
#> 11       18       1     3
#> 12       18       1    NA
#> 13       18       1    NA
#> 14       18       1     6
#> 15       18       2     0
#> 16       18       2     1
#> 17       18       2     2
#> 18       18       2     3
#> 19       18       2     4
#> 20       18       2    NA
#> 21       18       2     6
#> 22       18       3     0
#> 23       18       3     1
#> 24       18       3     2
#> 25       18       3     3
#> 26       18       3     4
#> 27       18       3    NA
#> 28       18       3     6
#> 29       18       4     0
#> 30       18       4     1
#> 31       18       4     2
#> 32       18       4     3
#> 33       18       4     4
#> 34       18       4     5
#> 35       18       4     6
#> 36       18       5     0
#> 37       18       5     1
#> 38       18       5     2
#> 39       18       5     3
#> 40       18       5     4
#> 41       18       5     5
#> 42       18       5     6
#> 43       18       6     0
#> 44       18       6     1
#> 45       18       6     2
#> 46       18       6     3
#> 47       18       6     4
#> 48       18       6    NA
#> 49       18       6     6
#> 50       18       7     0
#> 51       18       7     1
#> 52       18       7     2
#> 53       18       7     3
#> 54       18       7     4
#> 55       18       7     5
#> 56       18       7     6
#> 57       18       8     0
#> 58       18       8     1
#> 59       18       8     2
#> 60       18       8     3
#> 61       18       8     4
#> 62       18       8     5
#> 63       18       8     6
#> 64       18       9     0
#> 65       18       9     1
#> 66       18       9     2
#> 67       18       9     3
#> 68       18       9     4
#> 69       18       9     5
#> 70       18       9     6
#> 71       18      10     0
#> 72       18      10     1
#> 73       18      10     2
#> 74       18      10     3
#> 75       18      10     4
#> 76       18      10     5
#> 77       18      10     6
#> 
#> [[2]]
#>    atoutput polygon layer
#> 1        68       0     0
#> 2        68       0     1
#> 3        68       0     2
#> 4        68       0     3
#> 5        68       0     4
#> 6        68       0    NA
#> 7        68       0     6
#> 8        68       1     0
#> 9        68       1     1
#> 10       68       1     2
#> 11       68       1     3
#> 12       68       1    NA
#> 13       68       1    NA
#> 14       68       1     6
#> 15       68       2     0
#> 16       68       2     1
#> 17       68       2     2
#> 18       68       2     3
#> 19       68       2     4
#> 20       68       2    NA
#> 21       68       2     6
#> 22       68       3     0
#> 23       68       3     1
#> 24       68       3     2
#> 25       68       3     3
#> 26       68       3     4
#> 27       68       3    NA
#> 28       68       3     6
#> 29       68       4     0
#> 30       68       4     1
#> 31       68       4     2
#> 32       68       4     3
#> 33       68       4     4
#> 34       68       4     5
#> 35       68       4     6
#> 36       68       5     0
#> 37       68       5     1
#> 38       68       5     2
#> 39       68       5     3
#> 40       68       5     4
#> 41       68       5     5
#> 42       68       5     6
#> 43       68       6     0
#> 44       68       6     1
#> 45       68       6     2
#> 46       68       6     3
#> 47       68       6     4
#> 48       68       6    NA
#> 49       68       6     6
#> 50       68       7     0
#> 51       68       7     1
#> 52       68       7     2
#> 53       68       7     3
#> 54       68       7     4
#> 55       68       7     5
#> 56       68       7     6
#> 57       68       8     0
#> 58       68       8     1
#> 59       68       8     2
#> 60       68       8     3
#> 61       68       8     4
#> 62       68       8     5
#> 63       68       8     6
#> 64       68       9     0
#> 65       68       9     1
#> 66       68       9     2
#> 67       68       9     3
#> 68       68       9     4
#> 69       68       9     5
#> 70       68       9     6
#> 71       68      10     0
#> 72       68      10     1
#> 73       68      10     2
#> 74       68      10     3
#> 75       68      10     4
#> 76       68      10     5
#> 77       68      10     6
#> 
```
