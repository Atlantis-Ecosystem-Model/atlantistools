# Dietmatrix.

See `data-raw/data-create-reference-dfs.R` for further information.

## Usage

``` r
ref_dietmatrix
```

## Format

- pred:

  Name of the functional groups given as character string. The names
  match with the column 'LongName' in the functionalGroups.csv file.

- pred_stanza:

  Predator stanza. 1 = juvenile; 2 = adult.

- prey_stanza:

  Prey stanza. 1 = juvenile; 2 = adult.

- code:

  Flag from the biological parameter file.

- prey:

  Name of the functional groups given as character string. The names
  match with the column 'LongName' in the functionalGroups.csv file.

- avail:

  Obseravtion column storing the actual output value. Availability
  ranging from 0 to 1.

- prey_id:

  Preyid index based on the functional groups file.
