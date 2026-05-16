# Aggregate data using dplyr functionality.

This function is a 'wrapper' for the group_by and summarize procedure
used in dplyr to aggregate dataframes.

## Usage

``` r
agg_data(data, col = "atoutput", groups, out = "atoutput", fun)

agg_perc(data, col = "atoutput", groups, out = "atoutput")

group_data(data, groups)
```

## Arguments

- data:

  Dataframe the aggregation is applied to.

- col:

  Column of the dataframe the summarise function is applied. Default is
  `atoutput`.

- groups:

  Vector of character strings giving the grouping variables.

- out:

  Character string specifying the name of the output column. Default is
  `atoutput`.

- fun:

  Aggregation function to apply.

## Value

grouped datarame with the aggregated data.

## Examples

``` r
agg_ref_nums <- agg_data(data = ref_nums, groups = c("species", "agecl"), fun = mean)
```
