# Function to convert any column with information about functional groups to a factor whose levels use the LongName of the functional groups file.

This function is used to match the labels of the plots!

## Usage

``` r
convert_factor(data_fgs, col)
```

## Arguments

- data_fgs:

  Dataframe with information about functionalGroups. Usually loaded with
  load_fgs().

- col:

  Column of the dataframe which is converted to a factor.

## Value

Column of a dataframe in factor format.

## See also

Other convert functions:
[`convert_bgm()`](https://andybeet.github.io/atlantistools/reference/convert_bgm.md),
[`convert_time()`](https://andybeet.github.io/atlantistools/reference/convert_time.md)
