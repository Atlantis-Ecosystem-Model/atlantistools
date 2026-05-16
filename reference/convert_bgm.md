# Transform data from bgm-file to map dataframe.

Transform data from bgm-file to map dataframe.

## Usage

``` r
convert_bgm(bgm)
```

## Arguments

- bgm:

  Character string giving the connection to the atlantis bgm file. The
  filename ends in `.bgm`.

## See also

Other convert functions:
[`convert_factor()`](https://andybeet.github.io/atlantistools/reference/convert_factor.md),
[`convert_time()`](https://andybeet.github.io/atlantistools/reference/convert_time.md)

## Examples

``` r
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
bgm <- file.path(d, "VMPA_setas.bgm")

bgm <- convert_bgm(bgm)
head(bgm)
#>         lat     long inside_lat inside_long polygon
#> 1 -43.50757 146.0266  -43.91705     146.535       0
#> 2 -43.74006 145.9709  -43.91705     146.535       0
#> 3 -44.34406 146.8537  -43.91705     146.535       0
#> 4 -44.07555 147.1758  -43.91705     146.535       0
#> 5 -44.07486 147.1776  -43.91705     146.535       0
#> 6 -43.50757 146.0266  -43.91705     146.535       0
```
