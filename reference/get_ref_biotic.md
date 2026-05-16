# Extract bibliographic info from www.marlin.ac.uk/biotic.

Extract bibliographic information for growth, diets, distribution for
invertebrate species.

## Usage

``` r
get_ref_biotic(taxon, test = FALSE)
```

## Arguments

- taxon:

  Character vector of taxon names to search.

- test:

  Logical set to `TRUE` in case you need to run package development
  tests. Defaults to `FALSE`.

## Value

Dataframe

## Examples

``` r
if (FALSE) { # \dontrun{
taxon <- "Cancer pagurus"
taxon <- "Carcinus maenas"
taxon <- "xxx yyy"
taxon <- "Liocarcinus depurator"
taxon <- "Asterias rubens"
taxon <- "Henricia oculata"
taxon <- "Ensis ensis"
taxon <- "Ampelisca spinipes"
taxon <- "Tubularia indivisa"
taxon <- "Ophelia borealis"

df <- get_ref_biotic(taxon)
} # }
```
