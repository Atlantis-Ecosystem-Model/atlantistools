# Convert reference to bib-tex-key.

Convert reference to bib-tex-key.

## Usage

``` r
ref_to_bibkey(ref_df, bib)
```

## Arguments

- ref_df:

  dataframe with columns author, year, title in case title is missing
  match is performed based on author and year only.

- bib:

  character string giving the name of the .bib file.

## Value

Character vector.
