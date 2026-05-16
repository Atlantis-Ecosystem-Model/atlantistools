# Combine ageclasses to juvenile and adult stanza according to age at maturity.

Combine ageclasses to juvenile and adult stanza according to age at
maturity.

## Usage

``` r
combine_ages(data, grp_col, agemat, value_col = "atoutput")
```

## Arguments

- data:

  Dataframe with ageclass specific information.

- grp_col:

  Character string giving the name of the group column in `data`. E.g.
  'species', 'pred', 'prey' etc.

- agemat:

  First mature age class for age structured groups. This dataframe
  should be generated with
  [`prm_to_df`](https://andybeet.github.io/atlantistools/reference/prm_to_df.md)
  using "age_mat" as parameter.

- value_col:

  Character string giving the name of the column to sum. Default is
  `"atoutput"`.

## Value

Dataframe with ageclasses combined to stanzas.

## Examples

``` r
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
prm_biol <- file.path(d, "VMPA_setas_biol_fishing_Trunk.prm")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")

agemat <- prm_to_df(prm_biol = prm_biol, fgs = fgs,
                    group = get_age_acronyms(fgs = fgs),
                    parameter = "age_mat")

combine_ages(ref_nums, grp_col = "species", agemat = agemat)
#> Joining with `by = join_by(species)`
#> # A tibble: 282 × 6
#>    species                  polygon layer  time species_stanza atoutput
#>    <chr>                      <int> <dbl> <dbl>          <dbl>    <dbl>
#>  1 Shallow piscivorous fish       1     0     0              1  2.28e 6
#>  2 Shallow piscivorous fish       1     0     0              2  1.56e 6
#>  3 Shallow piscivorous fish       1     0     4              1  9.97e36
#>  4 Shallow piscivorous fish       1     0     4              2  8.97e37
#>  5 Shallow piscivorous fish       1     1     4              1  9.97e36
#>  6 Shallow piscivorous fish       1     1     4              2  8.97e37
#>  7 Shallow piscivorous fish       1     2     1              1  1.81e 8
#>  8 Shallow piscivorous fish       1     2     1              2  5.98e 7
#>  9 Shallow piscivorous fish       1     2     2              1  1.70e 7
#> 10 Shallow piscivorous fish       1     2     2              2  1.81e 8
#> # ℹ 272 more rows
```
