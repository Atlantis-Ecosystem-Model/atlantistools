# Convert timestep to actual time!

Convert timestep to actual time!

## Usage

``` r
convert_time(prm_run, col)
```

## Arguments

- prm_run:

  Character string giving the connection of the run parameterfile. The
  filename usually contains `run_fishing` and ends in `.prm`".

- col:

  Numeric vector. Usually a column in a dataframe with information about
  time. Either given as timesteps or days.

## Value

Numeric vector with the time in years.

## See also

Other convert functions:
[`convert_bgm()`](https://andybeet.github.io/atlantistools/reference/convert_bgm.md),
[`convert_factor()`](https://andybeet.github.io/atlantistools/reference/convert_factor.md)
