# This function is used to combine model output from different simulations!

This function is used to combine model output from different
simulations!

## Usage

``` r
combine_runs(outs, runs)
```

## Arguments

- outs:

  List of preprocessed Atlantis simulations. Each entry is a list
  generated with \`preprocess()\`.

- runs:

  Vector of character strings giving the name of each simulation
  settings.

## Value

Named list with the the same format as in \`preprocess()\`. Each
dataframe has an additional column run.

## See also

Other combine functions:
[`combine_groups()`](https://andybeet.github.io/atlantistools/reference/combine_groups.md)

## Examples

``` r
outs <- list(preprocess, preprocess)
runs <- c("run1", "run2")
test <- combine_runs(outs, runs)
names(test[[1]])
#> [1] "species"  "time"     "atoutput" "run"     
head(test[[1]])
#> # A tibble: 6 × 4
#>   species   time atoutput run  
#>   <chr>    <dbl>    <dbl> <chr>
#> 1 Carrion3   0.2 1.06e-10 run1 
#> 2 Carrion3   0.4 1.07e-10 run1 
#> 3 Carrion3   0.6 1.08e-10 run1 
#> 4 Carrion3   0.8 1.08e-10 run1 
#> 5 Carrion3   1   1.09e-10 run1 
#> 6 Carrion3   1.2 1.09e-10 run1 
```
