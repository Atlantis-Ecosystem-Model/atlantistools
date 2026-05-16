# Extract numeric values from string.

The function splits any character string at each tab and space and
returns all (min_only = FALSE) or only the first (min_only = T) numeric
value found in the string.

## Usage

``` r
str_split_twice(char, min_only = TRUE)
```

## Arguments

- char:

  Character string.

- min_only:

  Logical specifying if only the first numeric value (`TRUE`) or all
  numeric values (`FALSE`) should be returned. Default is `TRUE`.

## Value

numeric values inside `char` string.

## Examples

``` r
str_split_twice(char = "Hello   15")
#> [1] 15
str_split_twice(char = "flag1 15  16\t15", min_only = FALSE)
#> [1] 15 16 15
```
