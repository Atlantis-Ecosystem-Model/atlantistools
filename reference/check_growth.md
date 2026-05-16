# This function is used to check the individual growth per group over time.

This function is used to check the individual growth per group over
time.

## Usage

``` r
check_growth(data, yearly = FALSE)
```

## Arguments

- data:

  Dataframe with information about individual age based growth over
  time. This should be generated with
  [`preprocess()`](https://andybeet.github.io/atlantistools/reference/preprocess.md).
  You can test either structural or reserve weight.

- yearly:

  Logical specifying if relative change in individual weight shall be
  calculated on a yearly basis (`TRUE`) ot not (`FALSE`). Default is
  `FALSE`.

## Value

Dataframe showing the output of the linear model fit (slope &
F-statistic) per group and age.

## Examples

``` r
check_growth(preprocess$structn_age)
#>                     species agecl   relchange
#> 1  Shallow piscivorous fish     1 0.300163886
#> 2  Shallow piscivorous fish     2 1.762140040
#> 3  Shallow piscivorous fish     3 2.035049257
#> 4  Shallow piscivorous fish     4 1.679338958
#> 5  Shallow piscivorous fish     5 0.280039346
#> 6  Shallow piscivorous fish     6 0.068357266
#> 7  Shallow piscivorous fish     7 0.022428892
#> 8  Shallow piscivorous fish     8 0.006193832
#> 9  Shallow piscivorous fish     9 0.001737569
#> 10 Shallow piscivorous fish    10 0.000523105
#> 11 Small planktivorous fish     1 0.009224579
#> 12 Small planktivorous fish     2 5.071715256
#> 13 Small planktivorous fish     3 5.000424324
#> 14 Small planktivorous fish     4 4.801431149
#> 15 Small planktivorous fish     5 2.672596333
#> 16 Small planktivorous fish     6 1.010632700
#> 17 Small planktivorous fish     7 0.434577861
#> 18 Small planktivorous fish     8 0.226607580
#> 19 Small planktivorous fish     9 0.113634825
#> 20 Small planktivorous fish    10 0.059358666
check_growth(preprocess$resn_age)
#>                     species agecl relchange
#> 1  Shallow piscivorous fish     1 0.3190605
#> 2  Shallow piscivorous fish     2 1.2065826
#> 3  Shallow piscivorous fish     3 1.4687036
#> 4  Shallow piscivorous fish     4 1.0450495
#> 5  Shallow piscivorous fish     5 0.5075330
#> 6  Shallow piscivorous fish     6 0.9778014
#> 7  Shallow piscivorous fish     7 0.9080526
#> 8  Shallow piscivorous fish     8 0.8825714
#> 9  Shallow piscivorous fish     9 0.8759850
#> 10 Shallow piscivorous fish    10 0.8758981
#> 11 Small planktivorous fish     1 1.4216224
#> 12 Small planktivorous fish     2 5.1090951
#> 13 Small planktivorous fish     3 4.9968949
#> 14 Small planktivorous fish     4 4.8025838
#> 15 Small planktivorous fish     5 2.7824400
#> 16 Small planktivorous fish     6 1.4396822
#> 17 Small planktivorous fish     7 0.8444690
#> 18 Small planktivorous fish     8 0.6086534
#> 19 Small planktivorous fish     9 0.4859172
#> 20 Small planktivorous fish    10 0.4188423
check_growth(preprocess$resn_age, yearly = TRUE)
#>                     species agecl time    relchange
#> 1  Shallow piscivorous fish     1    0 3.190605e-01
#> 2  Shallow piscivorous fish     1    1 3.414760e-15
#> 3  Shallow piscivorous fish     1    2 5.609964e-15
#> 4  Shallow piscivorous fish     1    3 0.000000e+00
#> 5  Shallow piscivorous fish     2    0 7.498057e-01
#> 6  Shallow piscivorous fish     2    1 2.610444e-01
#> 7  Shallow piscivorous fish     2    2 2.793840e-15
#> 8  Shallow piscivorous fish     2    3 0.000000e+00
#> 9  Shallow piscivorous fish     3    0 1.600174e-01
#> 10 Shallow piscivorous fish     3    1 7.138319e-01
#> 11 Shallow piscivorous fish     3    2 2.439099e-01
#> 12 Shallow piscivorous fish     3    3 0.000000e+00
#> 13 Shallow piscivorous fish     4    0 3.673513e-02
#> 14 Shallow piscivorous fish     4    1 1.648091e-01
#> 15 Shallow piscivorous fish     4    2 7.178441e-01
#> 16 Shallow piscivorous fish     4    3 0.000000e+00
#> 17 Shallow piscivorous fish     5    0 3.131523e-01
#> 18 Shallow piscivorous fish     5    1 3.714145e-02
#> 19 Shallow piscivorous fish     5    2 1.509997e-01
#> 20 Shallow piscivorous fish     5    3 0.000000e+00
#> 21 Shallow piscivorous fish     6    0 8.885994e-01
#> 22 Shallow piscivorous fish     6    1 5.200416e-02
#> 23 Shallow piscivorous fish     6    2 8.989204e-02
#> 24 Shallow piscivorous fish     6    3 0.000000e+00
#> 25 Shallow piscivorous fish     7    0 8.807687e-01
#> 26 Shallow piscivorous fish     7    1 4.157585e-02
#> 27 Shallow piscivorous fish     7    2 6.536562e-02
#> 28 Shallow piscivorous fish     7    3 0.000000e+00
#> 29 Shallow piscivorous fish     8    0 8.778114e-01
#> 30 Shallow piscivorous fish     8    1 4.632822e-02
#> 31 Shallow piscivorous fish     8    2 6.556563e-02
#> 32 Shallow piscivorous fish     8    3 0.000000e+00
#> 33 Shallow piscivorous fish     9    0 8.759850e-01
#> 34 Shallow piscivorous fish     9    1 4.548689e-02
#> 35 Shallow piscivorous fish     9    2 6.255716e-02
#> 36 Shallow piscivorous fish     9    3 0.000000e+00
#> 37 Shallow piscivorous fish    10    0 8.758981e-01
#> 38 Shallow piscivorous fish    10    1 4.547106e-02
#> 39 Shallow piscivorous fish    10    2 6.182415e-02
#> 40 Shallow piscivorous fish    10    3 0.000000e+00
#> 41 Small planktivorous fish     1    0 1.421368e+00
#> 42 Small planktivorous fish     1    1 2.396908e-03
#> 43 Small planktivorous fish     1    2 1.642526e-03
#> 44 Small planktivorous fish     1    3 0.000000e+00
#> 45 Small planktivorous fish     2    0 4.840190e+00
#> 46 Small planktivorous fish     2    1 1.661280e-01
#> 47 Small planktivorous fish     2    2 8.949249e-02
#> 48 Small planktivorous fish     2    3 0.000000e+00
#> 49 Small planktivorous fish     3    0 1.608460e+00
#> 50 Small planktivorous fish     3    1 1.315512e+00
#> 51 Small planktivorous fish     3    2 6.823521e-02
#> 52 Small planktivorous fish     3    3 0.000000e+00
#> 53 Small planktivorous fish     4    0 8.792245e-01
#> 54 Small planktivorous fish     4    1 8.372645e-01
#> 55 Small planktivorous fish     4    2 7.387879e-01
#> 56 Small planktivorous fish     4    3 0.000000e+00
#> 57 Small planktivorous fish     5    0 6.020948e-01
#> 58 Small planktivorous fish     5    1 4.230114e-01
#> 59 Small planktivorous fish     5    2 6.943297e-01
#> 60 Small planktivorous fish     5    3 0.000000e+00
#> 61 Small planktivorous fish     6    0 4.681182e-01
#> 62 Small planktivorous fish     6    1 2.071930e-01
#> 63 Small planktivorous fish     6    2 3.984055e-01
#> 64 Small planktivorous fish     6    3 0.000000e+00
#> 65 Small planktivorous fish     7    0 4.159818e-01
#> 66 Small planktivorous fish     7    1 1.112488e-01
#> 67 Small planktivorous fish     7    2 1.857428e-01
#> 68 Small planktivorous fish     7    3 0.000000e+00
#> 69 Small planktivorous fish     8    0 3.839788e-01
#> 70 Small planktivorous fish     8    1 6.330271e-02
#> 71 Small planktivorous fish     8    2 1.048023e-01
#> 72 Small planktivorous fish     8    3 0.000000e+00
#> 73 Small planktivorous fish     9    0 3.625716e-01
#> 74 Small planktivorous fish     9    1 3.755135e-02
#> 75 Small planktivorous fish     9    2 5.966937e-02
#> 76 Small planktivorous fish     9    3 0.000000e+00
#> 77 Small planktivorous fish    10    0 3.601159e-01
#> 78 Small planktivorous fish    10    1 1.546043e-02
#> 79 Small planktivorous fish    10    2 3.553834e-02
#> 80 Small planktivorous fish    10    3 0.000000e+00
```
