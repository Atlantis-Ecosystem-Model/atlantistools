# Calculate relative timeseries using the initial value as benchmark.

Calculate relative timeseries using the initial value as benchmark.

## Usage

``` r
convert_relative_initial(data, col = "atoutput")
```

## Arguments

- data:

  Dataframe to apply the transformation to.

- col:

  Character value giving the name of the column to transform. Default is
  `"atoutput"`.

## Value

Dataframe with transformed column 'col'.

## Examples

``` r
df <- convert_relative_initial(ref_structn)
head(df[df$layer == 1, ], n = 15)
#>                     species agecl polygon layer time atoutput
#> 2  Small planktivorous fish     1       1     1    0 1.000000
#> 7  Small planktivorous fish     1       2     1    0 1.000000
#> 13 Small planktivorous fish     1       3     1    0 1.000000
#> 19 Small planktivorous fish     1       4     1    0 1.000000
#> 26 Small planktivorous fish     1       5     1    0 1.000000
#> 33 Small planktivorous fish     1       1     1    1 1.001849
#> 38 Small planktivorous fish     1       2     1    1 1.001849
#> 44 Small planktivorous fish     1       3     1    1 1.001849
#> 50 Small planktivorous fish     1       4     1    1 1.001849
#> 57 Small planktivorous fish     1       5     1    1 1.001849
#> 64 Small planktivorous fish     1       1     1    2 1.000976
#> 69 Small planktivorous fish     1       2     1    2 1.000976
#> 75 Small planktivorous fish     1       3     1    2 1.000976
#> 81 Small planktivorous fish     1       4     1    2 1.000976
#> 88 Small planktivorous fish     1       5     1    2 1.000976
```
