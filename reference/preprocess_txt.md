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
d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
df <- load_txt(file = file.path(d, "outputSETASSpecificPredMort.txt"))
df <- preprocess_txt(df_txt = df, into = c("pred", "agecl", "empty_col1", "prey", "empty_col2"))
head(df)
#> # A tibble: 6 × 5
#>    time pred  agecl prey       atoutput
#>   <dbl> <chr> <dbl> <chr>         <dbl>
#> 1    73 CEP       1 CEP   0.00000000883
#> 2   146 CEP       1 CEP   0.0000000100 
#> 3   219 CEP       1 CEP   0.00000000898
#> 4   292 CEP       1 CEP   0.00000000808
#> 5   365 CEP       1 CEP   0.0000000119 
#> 6   438 CEP       1 CEP   0.0000000131 

df <- load_txt(file = file.path(d, "outputSETASSpecificMort.txt"))
df <- preprocess_txt(df_txt = df, into = c("species", "agecl", "empty_col", "mort"))
head(df)
#> # A tibble: 6 × 5
#>    time species agecl mort  atoutput
#>   <dbl> <chr>   <dbl> <chr>    <dbl>
#> 1   365 BML         1 M1    1.06e-15
#> 2   730 BML         1 M1    1.03e-15
#> 3   365 CEP         1 F     5.63e-18
#> 4   730 CEP         1 F     3.83e-17
#> 5   365 CEP         1 M2    3.70e- 4
#> 6   730 CEP         1 M2    4.91e- 4
```
