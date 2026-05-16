# Combine values from different groups if specific groups only have a low contribution to the overall value.

Combine values from different groups if specific groups only have a low
contribution to the overall value.

## Usage

``` r
combine_groups(
  data,
  group_col,
  groups = names(data)[!is.element(names(data), c("atoutput", "time", group_col))],
  combine_thresh = 15
)
```

## Arguments

- data:

  Dataframe whose groups shall be combined.

- group_col:

  Character string giving the name of the group column in \`data\`.

- groups:

  Vector of character strings giving the grouping variables.

- combine_thresh:

  Integer indicating the number of groups to display. Default is `15`.

## Value

dataframe with groups combined to "Rest" if contribution is low.

## See also

Other combine functions:
[`combine_runs()`](https://andybeet.github.io/atlantistools/reference/combine_runs.md)

## Examples

``` r
df <- combine_groups(ref_dm, group_col = "prey")
#> Joining with `by = join_by(pred, agecl, prey)`
df <- combine_groups(ref_dm, group_col = "prey", combine_thresh = 2)
#> Joining with `by = join_by(pred, agecl, prey)`
#> Joining with `by = join_by(pred, agecl, prey)`
```
