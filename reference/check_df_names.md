# Function to check the names of a dataframe.

This function is used in most plotting routines to check if the correct
dataframe is passed.

## Usage

``` r
check_df_names(data, expect, optional = NULL)
```

## Arguments

- data:

  Dataframe to check.

- expect:

  Character vector giving the names of the expected columns

- optional:

  Character vector giving the names of optional columns. Default is
  `NULL`.

## Examples

``` r
if (FALSE) { # \dontrun{
check_df_names(preprocess$biomass_age, expect = c("time", "species", "atoutput", "ages"))
} # }
```
