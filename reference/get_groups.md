# Collection of similar functions which get specific columns from the Atlantis `functionalGroups.csv`

This collection of functions uses the dataframe of functional groups
created with
[`load_fgs`](https://andybeet.github.io/atlantistools/reference/load_fgs.md)
and creates various character strings of group names or acronym names.

## Usage

``` r
get_groups(fgs)

get_age_groups(fgs)

get_acronyms(fgs)

get_age_acronyms(fgs)

get_nonage_acronyms(fgs)

get_fish_acronyms(fgs)

get_cohorts_acronyms(fgs, numCohorts = 10)

get_fished_acronyms(fgs)

get_turnedon_acronyms(fgs)
```

## Arguments

- fgs:

  Character string giving the connection to the functional groups file.
  The filename usually contains `Groups` and does end in `.csv`.

- numCohorts:

  integer. Number of cohorts by which to filter (Default = 10)

## Value

Character string.

## Details

Currently, the following character strings can be created

`get_groups`: Extract column "Name"

`get_age_groups`: Extract column "Name". Selects groups with 10
ageclasses.

`get_acronyms`: Extract column "Code"

`get_age_acronyms`: Extract column "Code". Selects groups with 10
ageclasses.

`get_nonage_acronyms`: Extracts columns "Code". Only groups with
ageclasses different from 10 are selected.

`get_fish_acronyms`: Extract column "Code". Only groups with InvertType
equal to "FISH" or "SHARK" are selected.

`get_cohorts_acronyms`: Extracts column "Code" based on Cohort size

`get_fished_acronyms`: Extracts column "Code" based on whether a species
is fished in the model (IsFished == 1)

`get_turnedon_acronyms`: Extracts column "Code" based on whether species
are turned on in the model (IsTurnedOn == 1)

## See also

Other get functions:
[`get_boundary()`](https://andybeet.github.io/atlantistools/reference/get_boundary.md),
[`get_colpal()`](https://andybeet.github.io/atlantistools/reference/get_colpal.md),
[`get_conv_mgnbiot()`](https://andybeet.github.io/atlantistools/reference/get_conv_mgnbiot.md)

## Examples

``` r
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")

get_age_groups(fgs)
#> [1] "Planktiv_S_Fish" "Pisciv_S_Fish"  
get_nonage_acronyms(fgs)
#> [1] "CEP" "BML" "PL"  "DL"  "DR"  "DC" 
```
