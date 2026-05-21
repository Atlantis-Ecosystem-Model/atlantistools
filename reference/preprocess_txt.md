# Preprocess dataframes loaded in with `load_txt()`

`sep_col` is split into multiple columns given by `into`. If column
ageclass is present and values start with 0 one is added to align with
agestructure in other functions. Columns without any informations
(`length(unique()) == 1`) are dropped. If the first time step only has
zeros as values remove these values. remove zeros overall!

## Usage

``` r
preprocess_txt(df_txt, sep_col = "code", into, removeZeros = TRUE)
```

## Arguments

- df_txt:

  Dataframe read in with
  [`load_txt()`](https://andybeet.github.io/atlantistools/reference/load_txt.md).

- sep_col:

  Column to separate into multiple columns. Default is `"code"`.

- into:

  Character vector given the columns to split sep_col in.

- removeZeros:

  Boolean. Remove all zeros. (Default = T)

## Value

Tidy dataframe.

## Examples

``` r

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
df <- load_txt(file = file.path(d, "outputSETASSpecificMort.txt"))
df <- preprocess_txt(df_txt = df, into = c("species", "agecl", "empty_col", "mort"))
head(df)
#> # A tibble: 6 × 5
#>    time species agecl mort  atoutput
#>   <dbl> <chr>   <dbl> <chr>    <dbl>
#> 1   365 BML         1 M1    1.27e-15
#> 2   365 CEP         1 M1    3.82e- 5
#> 3   365 CEP         1 M2    6.80e+ 8
#> 4   365 CEP         2 M1    1.08e-19
#> 5   365 CEP         2 M2    9.74e- 5
#> 6   365 DL          1 M2    7.39e- 9
```
