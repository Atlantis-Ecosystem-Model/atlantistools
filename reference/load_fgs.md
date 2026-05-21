# Load the functional group file

Read in the functional group file as dataframe.

## Usage

``` r
load_fgs(fgs)
```

## Arguments

- fgs:

  Character string giving the connection to the functional groups file.
  The filename usually contains `Groups` and does end in `.csv`.

## Value

A `data.frame` of functional group information.

## See also

Other load functions:
[`load_box()`](https://andybeet.github.io/atlantistools/reference/load_box.md),
[`load_bps()`](https://andybeet.github.io/atlantistools/reference/load_bps.md),
[`load_dietcheck()`](https://andybeet.github.io/atlantistools/reference/load_dietcheck.md),
[`load_init()`](https://andybeet.github.io/atlantistools/reference/load_init.md),
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
file <- "SETasGroupsDem_NoCep.csv"
fgs <- load_fgs(file.path(d, file))
head(fgs)
#>   Code Index IsTurnedOn            Name                 LongName NumCohorts
#> 1  FPS     1          1 Planktiv_S_Fish Small planktivorous fish         10
#> 2  FVS     2          1   Pisciv_S_Fish Shallow piscivorous fish         10
#> 3  CEP     3          1      Cephalopod               Cephalopod          2
#> 4  BML     4          1  Megazoobenthos           Megazoobenthos          1
#> 5   PL     5          1          Diatom                   Diatom          1
#> 6   DL     6          1         Lab_Det          Labile detritus          1
#>   NumGeneTypes NumStages NumSpawns NumAgeClassSize NumStocks VerticallyMigrates
#> 1            1         2         1               1         1                  1
#> 2            1         2         1               1         1                  1
#> 3            1         2         1               1         1                  1
#> 4            1         1         1               1         1                  1
#> 5            1         1         1               1         1                  0
#> 6            1         1         1               1         1                  0
#>   HorizontallyMigrates IsFished IsImpacted isTAC    GroupType IsPredator
#> 1                    1        1          1     1         FISH          1
#> 2                    1        1          1     1         FISH          1
#> 3                    1        1          1     1          CEP          1
#> 4                    1        1          1     1 MOB_EP_OTHER          1
#> 5                    0        0          0     0       LG_PHY          0
#> 6                    0        0          0     0      LAB_DET          0
#>   IsCover IsSiliconDep IsAssessed IsCatchGrazer OverWinters isCultured
#> 1       0            0          1             0           0          0
#> 2       0            0          1             0           0          0
#> 3       0            0          1             0           0          0
#> 4       0            0          1             0           0          0
#> 5       0            1          1             0           0          0
#> 6       0            0          1             0           0          0
#>   isHabDepend
#> 1           0
#> 2           0
#> 3           0
#> 4           0
#> 5           0
#> 6           0
```
