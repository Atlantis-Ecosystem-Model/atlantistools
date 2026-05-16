# Extract the bibliographic info from www.fishbase.org.

Extract bibliographic information for growth parameters (linf, k, t0)
from www.fishbase.org

## Usage

``` r
get_ref_fishbase(ref_id, mirror = "se")
```

## Arguments

- ref_id:

  vector of reference ids.

- mirror:

  Character string defining the url mirror to use. Defaults to `se`. In
  case data extraction is slow use a different mirror. Try to avoid
  frequently used mirrors like `uk` or `com`.

## Value

Dataframe

## Examples

``` r
if (FALSE) { # \dontrun{
df <- get_growth_fishbase("Scyliorhinus canicula")

df$data_ref[df$data_ref == df$main_ref] <- NA
df <- tidyr::gather_(data = df,
                     key_col = "ref_type",
                     value_col = "ref_id",
                     gather_cols = c("main_ref", "data_ref"), na.rm = TRUE)
ref_id <- unique(df$ref_id)
get_ref_fishbase(ref_id)
} # }
```
