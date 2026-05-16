# Plot recruitment.

Plot recruitment.

## Usage

``` r
plot_rec(data, ex_data, ncol = 7)
```

## Arguments

- data:

  Dataframe with information about ssb and recruits. This is created
  from atlantis output files YOY.txt and SSB.txt (Usually
  output\[...\]YOY.txt' & 'output\[...\]SSB.txt') using
  [`load_rec`](https://andybeet.github.io/atlantistools/reference/load_rec.md).

- ex_data:

  Dataframe to compare the atlantis run with.

- ncol:

  Number of columns in multipanel plot. Default is `7`.

## Value

ggplot2 object

## See also

Other plot functions:
[`plot_bar()`](https://andybeet.github.io/atlantistools/reference/plot_bar.md),
[`plot_boxes()`](https://andybeet.github.io/atlantistools/reference/plot_boxes.md),
[`plot_diet()`](https://andybeet.github.io/atlantistools/reference/plot_diet.md),
[`plot_diet_bec_dev()`](https://andybeet.github.io/atlantistools/reference/plot_diet_bec_dev.md),
[`plot_line()`](https://andybeet.github.io/atlantistools/reference/plot_line.md),
[`plot_species()`](https://andybeet.github.io/atlantistools/reference/plot_species.md)

## Examples

``` r
if (FALSE) { # \dontrun{
d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
ex_data <- read.csv(file.path(d, "setas-ssb-rec.csv"), stringsAsFactors = FALSE)
plot_rec(preprocess_setas$ssb_rec, ex_data)
} # }
```
