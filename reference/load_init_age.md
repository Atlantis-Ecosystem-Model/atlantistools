# This function loads weight at age data (in mgN) from the initial conditions file.

This function loads weight at age data (in mgN) from the initial
conditions file.

## Usage

``` r
load_init_age(init, fgs, select_variable, select_groups = NULL, bboxes)

load_init_nonage(
  init,
  fgs,
  select_variable = "N",
  select_groups = NULL,
  bboxes,
  bps
)

load_init_stanza(
  init,
  fgs,
  select_variable = "N",
  select_groups = NULL,
  bboxes
)

load_init_physics(init, select_variable, bboxes)

load_init_weight(init, fgs, bboxes)
```

## Arguments

- init:

  Character string giving the connection of the initial conditions
  netcdf file. The filename usually contains `init` and ends in `.nc`.

- fgs:

  Character string giving the connection to the functional groups file.
  The filename usually contains `Groups` and does end in `.csv`.

- select_variable:

  Character value specifying which variable to load. For `load_init_age`
  this can be "Nums", "ResN", "StructN", For `load_init_nonage` please
  select "N" (default) For `load_init_physics` simply pass the names of
  the physical variables.

- select_groups:

  Character vector of functional groups which shall be read in. Names
  have to match the ones used in the ncdf file. Check column "Name" in
  "functionalGroups.csv" for clarification. Default is `NULL` resulting
  in all available groups.

- bboxes:

  Integer vector giving the box-id of the boundary boxes. Can be created
  with `get_boundary`.

- bps:

  Vector of character strings giving the complete list of epibenthic
  functional groups (Only present in the sediment layer). The names have
  to match the column 'Name' in the 'functionalGroups.csv' file. Can be
  created with `load_bps`.#'

## Value

A dataframes with columns atoutput, polygon, layer (if present), species
(if present).

## See also

Other load functions:
[`load_box()`](https://andybeet.github.io/atlantistools/reference/load_box.md),
[`load_bps()`](https://andybeet.github.io/atlantistools/reference/load_bps.md),
[`load_dietcheck()`](https://andybeet.github.io/atlantistools/reference/load_dietcheck.md),
[`load_fgs()`](https://andybeet.github.io/atlantistools/reference/load_fgs.md),
[`load_init()`](https://andybeet.github.io/atlantistools/reference/load_init.md),
[`load_mort()`](https://andybeet.github.io/atlantistools/reference/load_mort.md),
[`load_nc()`](https://andybeet.github.io/atlantistools/reference/load_nc.md),
[`load_nc_physics()`](https://andybeet.github.io/atlantistools/reference/load_nc_physics.md),
[`load_rec()`](https://andybeet.github.io/atlantistools/reference/load_rec.md),
[`load_spec_mort()`](https://andybeet.github.io/atlantistools/reference/load_spec_mort.md),
[`load_spec_pred_mort()`](https://andybeet.github.io/atlantistools/reference/load_spec_pred_mort.md),
[`load_txt()`](https://andybeet.github.io/atlantistools/reference/load_txt.md)

## Author

Alexander Keth

## Examples

``` r
d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
init <- file.path(d, "INIT_VMPA_Jan2015.nc")
fgs <- file.path(d, "SETasGroupsDem_NoCep.csv")
bboxes <- get_boundary(load_box(bgm = file.path(d, "VMPA_setas.bgm")))

bps <- load_bps(fgs = fgs, init = init)

# There are no values in the initial conditions file. Therefore atoutput is NA.
load_init_age(init = init, fgs = fgs, bboxes = bboxes,
              select_variable = "ResN",
              select_groups = "Planktiv_S_Fish")
#>     atoutput polygon layer                  species agecl
#> 1       0.25       1     0 Small planktivorous fish     1
#> 2       0.25       1     1 Small planktivorous fish     1
#> 3       0.25       1     2 Small planktivorous fish     1
#> 4       0.25       1     3 Small planktivorous fish     1
#> 5       0.25       1     6 Small planktivorous fish     1
#> 6       0.25       2     0 Small planktivorous fish     1
#> 7       0.25       2     1 Small planktivorous fish     1
#> 8       0.25       2     2 Small planktivorous fish     1
#> 9       0.25       2     3 Small planktivorous fish     1
#> 10      0.25       2     4 Small planktivorous fish     1
#> 11      0.25       2     6 Small planktivorous fish     1
#> 12      0.25       3     0 Small planktivorous fish     1
#> 13      0.25       3     1 Small planktivorous fish     1
#> 14      0.25       3     2 Small planktivorous fish     1
#> 15      0.25       3     3 Small planktivorous fish     1
#> 16      0.25       3     4 Small planktivorous fish     1
#> 17      0.25       3     6 Small planktivorous fish     1
#> 18      0.25       4     0 Small planktivorous fish     1
#> 19      0.25       4     1 Small planktivorous fish     1
#> 20      0.25       4     2 Small planktivorous fish     1
#> 21      0.25       4     3 Small planktivorous fish     1
#> 22      0.25       4     4 Small planktivorous fish     1
#> 23      0.25       4     5 Small planktivorous fish     1
#> 24      0.25       4     6 Small planktivorous fish     1
#> 25      0.25       5     0 Small planktivorous fish     1
#> 26      0.25       5     1 Small planktivorous fish     1
#> 27      0.25       5     2 Small planktivorous fish     1
#> 28      0.25       5     3 Small planktivorous fish     1
#> 29      0.25       5     4 Small planktivorous fish     1
#> 30      0.25       5     5 Small planktivorous fish     1
#> 31      0.25       5     6 Small planktivorous fish     1
#> 32     18.00       1     0 Small planktivorous fish     2
#> 33     18.00       1     1 Small planktivorous fish     2
#> 34     18.00       1     2 Small planktivorous fish     2
#> 35     18.00       1     3 Small planktivorous fish     2
#> 36     18.00       1     6 Small planktivorous fish     2
#> 37     18.00       2     0 Small planktivorous fish     2
#> 38     18.00       2     1 Small planktivorous fish     2
#> 39     18.00       2     2 Small planktivorous fish     2
#> 40     18.00       2     3 Small planktivorous fish     2
#> 41     18.00       2     4 Small planktivorous fish     2
#> 42     18.00       2     6 Small planktivorous fish     2
#> 43     18.00       3     0 Small planktivorous fish     2
#> 44     18.00       3     1 Small planktivorous fish     2
#> 45     18.00       3     2 Small planktivorous fish     2
#> 46     18.00       3     3 Small planktivorous fish     2
#> 47     18.00       3     4 Small planktivorous fish     2
#> 48     18.00       3     6 Small planktivorous fish     2
#> 49     18.00       4     0 Small planktivorous fish     2
#> 50     18.00       4     1 Small planktivorous fish     2
#> 51     18.00       4     2 Small planktivorous fish     2
#> 52     18.00       4     3 Small planktivorous fish     2
#> 53     18.00       4     4 Small planktivorous fish     2
#> 54     18.00       4     5 Small planktivorous fish     2
#> 55     18.00       4     6 Small planktivorous fish     2
#> 56     18.00       5     0 Small planktivorous fish     2
#> 57     18.00       5     1 Small planktivorous fish     2
#> 58     18.00       5     2 Small planktivorous fish     2
#> 59     18.00       5     3 Small planktivorous fish     2
#> 60     18.00       5     4 Small planktivorous fish     2
#> 61     18.00       5     5 Small planktivorous fish     2
#> 62     18.00       5     6 Small planktivorous fish     2
#> 63     68.00       1     0 Small planktivorous fish     3
#> 64     68.00       1     1 Small planktivorous fish     3
#> 65     68.00       1     2 Small planktivorous fish     3
#> 66     68.00       1     3 Small planktivorous fish     3
#> 67     68.00       1     6 Small planktivorous fish     3
#> 68     68.00       2     0 Small planktivorous fish     3
#> 69     68.00       2     1 Small planktivorous fish     3
#> 70     68.00       2     2 Small planktivorous fish     3
#> 71     68.00       2     3 Small planktivorous fish     3
#> 72     68.00       2     4 Small planktivorous fish     3
#> 73     68.00       2     6 Small planktivorous fish     3
#> 74     68.00       3     0 Small planktivorous fish     3
#> 75     68.00       3     1 Small planktivorous fish     3
#> 76     68.00       3     2 Small planktivorous fish     3
#> 77     68.00       3     3 Small planktivorous fish     3
#> 78     68.00       3     4 Small planktivorous fish     3
#> 79     68.00       3     6 Small planktivorous fish     3
#> 80     68.00       4     0 Small planktivorous fish     3
#> 81     68.00       4     1 Small planktivorous fish     3
#> 82     68.00       4     2 Small planktivorous fish     3
#> 83     68.00       4     3 Small planktivorous fish     3
#> 84     68.00       4     4 Small planktivorous fish     3
#> 85     68.00       4     5 Small planktivorous fish     3
#> 86     68.00       4     6 Small planktivorous fish     3
#> 87     68.00       5     0 Small planktivorous fish     3
#> 88     68.00       5     1 Small planktivorous fish     3
#> 89     68.00       5     2 Small planktivorous fish     3
#> 90     68.00       5     3 Small planktivorous fish     3
#> 91     68.00       5     4 Small planktivorous fish     3
#> 92     68.00       5     5 Small planktivorous fish     3
#> 93     68.00       5     6 Small planktivorous fish     3
#> 94    115.00       1     0 Small planktivorous fish     4
#> 95    115.00       1     1 Small planktivorous fish     4
#> 96    115.00       1     2 Small planktivorous fish     4
#> 97    115.00       1     3 Small planktivorous fish     4
#> 98    115.00       1     6 Small planktivorous fish     4
#> 99    115.00       2     0 Small planktivorous fish     4
#> 100   115.00       2     1 Small planktivorous fish     4
#> 101   115.00       2     2 Small planktivorous fish     4
#> 102   115.00       2     3 Small planktivorous fish     4
#> 103   115.00       2     4 Small planktivorous fish     4
#> 104   115.00       2     6 Small planktivorous fish     4
#> 105   115.00       3     0 Small planktivorous fish     4
#> 106   115.00       3     1 Small planktivorous fish     4
#> 107   115.00       3     2 Small planktivorous fish     4
#> 108   115.00       3     3 Small planktivorous fish     4
#> 109   115.00       3     4 Small planktivorous fish     4
#> 110   115.00       3     6 Small planktivorous fish     4
#> 111   115.00       4     0 Small planktivorous fish     4
#> 112   115.00       4     1 Small planktivorous fish     4
#> 113   115.00       4     2 Small planktivorous fish     4
#> 114   115.00       4     3 Small planktivorous fish     4
#> 115   115.00       4     4 Small planktivorous fish     4
#> 116   115.00       4     5 Small planktivorous fish     4
#> 117   115.00       4     6 Small planktivorous fish     4
#> 118   115.00       5     0 Small planktivorous fish     4
#> 119   115.00       5     1 Small planktivorous fish     4
#> 120   115.00       5     2 Small planktivorous fish     4
#> 121   115.00       5     3 Small planktivorous fish     4
#> 122   115.00       5     4 Small planktivorous fish     4
#> 123   115.00       5     5 Small planktivorous fish     4
#> 124   115.00       5     6 Small planktivorous fish     4
#> 125   150.00       1     0 Small planktivorous fish     5
#> 126   150.00       1     1 Small planktivorous fish     5
#> 127   150.00       1     2 Small planktivorous fish     5
#> 128   150.00       1     3 Small planktivorous fish     5
#> 129   150.00       1     6 Small planktivorous fish     5
#> 130   150.00       2     0 Small planktivorous fish     5
#> 131   150.00       2     1 Small planktivorous fish     5
#> 132   150.00       2     2 Small planktivorous fish     5
#> 133   150.00       2     3 Small planktivorous fish     5
#> 134   150.00       2     4 Small planktivorous fish     5
#> 135   150.00       2     6 Small planktivorous fish     5
#> 136   150.00       3     0 Small planktivorous fish     5
#> 137   150.00       3     1 Small planktivorous fish     5
#> 138   150.00       3     2 Small planktivorous fish     5
#> 139   150.00       3     3 Small planktivorous fish     5
#> 140   150.00       3     4 Small planktivorous fish     5
#> 141   150.00       3     6 Small planktivorous fish     5
#> 142   150.00       4     0 Small planktivorous fish     5
#> 143   150.00       4     1 Small planktivorous fish     5
#> 144   150.00       4     2 Small planktivorous fish     5
#> 145   150.00       4     3 Small planktivorous fish     5
#> 146   150.00       4     4 Small planktivorous fish     5
#> 147   150.00       4     5 Small planktivorous fish     5
#> 148   150.00       4     6 Small planktivorous fish     5
#> 149   150.00       5     0 Small planktivorous fish     5
#> 150   150.00       5     1 Small planktivorous fish     5
#> 151   150.00       5     2 Small planktivorous fish     5
#> 152   150.00       5     3 Small planktivorous fish     5
#> 153   150.00       5     4 Small planktivorous fish     5
#> 154   150.00       5     5 Small planktivorous fish     5
#> 155   150.00       5     6 Small planktivorous fish     5
#> 156   170.00       1     0 Small planktivorous fish     6
#> 157   170.00       1     1 Small planktivorous fish     6
#> 158   170.00       1     2 Small planktivorous fish     6
#> 159   170.00       1     3 Small planktivorous fish     6
#> 160   170.00       1     6 Small planktivorous fish     6
#> 161   170.00       2     0 Small planktivorous fish     6
#> 162   170.00       2     1 Small planktivorous fish     6
#> 163   170.00       2     2 Small planktivorous fish     6
#> 164   170.00       2     3 Small planktivorous fish     6
#> 165   170.00       2     4 Small planktivorous fish     6
#> 166   170.00       2     6 Small planktivorous fish     6
#> 167   170.00       3     0 Small planktivorous fish     6
#> 168   170.00       3     1 Small planktivorous fish     6
#> 169   170.00       3     2 Small planktivorous fish     6
#> 170   170.00       3     3 Small planktivorous fish     6
#> 171   170.00       3     4 Small planktivorous fish     6
#> 172   170.00       3     6 Small planktivorous fish     6
#> 173   170.00       4     0 Small planktivorous fish     6
#> 174   170.00       4     1 Small planktivorous fish     6
#> 175   170.00       4     2 Small planktivorous fish     6
#> 176   170.00       4     3 Small planktivorous fish     6
#> 177   170.00       4     4 Small planktivorous fish     6
#> 178   170.00       4     5 Small planktivorous fish     6
#> 179   170.00       4     6 Small planktivorous fish     6
#> 180   170.00       5     0 Small planktivorous fish     6
#> 181   170.00       5     1 Small planktivorous fish     6
#> 182   170.00       5     2 Small planktivorous fish     6
#> 183   170.00       5     3 Small planktivorous fish     6
#> 184   170.00       5     4 Small planktivorous fish     6
#> 185   170.00       5     5 Small planktivorous fish     6
#> 186   170.00       5     6 Small planktivorous fish     6
#> 187   182.00       1     0 Small planktivorous fish     7
#> 188   182.00       1     1 Small planktivorous fish     7
#> 189   182.00       1     2 Small planktivorous fish     7
#> 190   182.00       1     3 Small planktivorous fish     7
#> 191   182.00       1     6 Small planktivorous fish     7
#> 192   182.00       2     0 Small planktivorous fish     7
#> 193   182.00       2     1 Small planktivorous fish     7
#> 194   182.00       2     2 Small planktivorous fish     7
#> 195   182.00       2     3 Small planktivorous fish     7
#> 196   182.00       2     4 Small planktivorous fish     7
#> 197   182.00       2     6 Small planktivorous fish     7
#> 198   182.00       3     0 Small planktivorous fish     7
#> 199   182.00       3     1 Small planktivorous fish     7
#> 200   182.00       3     2 Small planktivorous fish     7
#> 201   182.00       3     3 Small planktivorous fish     7
#> 202   182.00       3     4 Small planktivorous fish     7
#> 203   182.00       3     6 Small planktivorous fish     7
#> 204   182.00       4     0 Small planktivorous fish     7
#> 205   182.00       4     1 Small planktivorous fish     7
#> 206   182.00       4     2 Small planktivorous fish     7
#> 207   182.00       4     3 Small planktivorous fish     7
#> 208   182.00       4     4 Small planktivorous fish     7
#> 209   182.00       4     5 Small planktivorous fish     7
#> 210   182.00       4     6 Small planktivorous fish     7
#> 211   182.00       5     0 Small planktivorous fish     7
#> 212   182.00       5     1 Small planktivorous fish     7
#> 213   182.00       5     2 Small planktivorous fish     7
#> 214   182.00       5     3 Small planktivorous fish     7
#> 215   182.00       5     4 Small planktivorous fish     7
#> 216   182.00       5     5 Small planktivorous fish     7
#> 217   182.00       5     6 Small planktivorous fish     7
#> 218   189.00       1     0 Small planktivorous fish     8
#> 219   189.00       1     1 Small planktivorous fish     8
#> 220   189.00       1     2 Small planktivorous fish     8
#> 221   189.00       1     3 Small planktivorous fish     8
#> 222   189.00       1     6 Small planktivorous fish     8
#> 223   189.00       2     0 Small planktivorous fish     8
#> 224   189.00       2     1 Small planktivorous fish     8
#> 225   189.00       2     2 Small planktivorous fish     8
#> 226   189.00       2     3 Small planktivorous fish     8
#> 227   189.00       2     4 Small planktivorous fish     8
#> 228   189.00       2     6 Small planktivorous fish     8
#> 229   189.00       3     0 Small planktivorous fish     8
#> 230   189.00       3     1 Small planktivorous fish     8
#> 231   189.00       3     2 Small planktivorous fish     8
#> 232   189.00       3     3 Small planktivorous fish     8
#> 233   189.00       3     4 Small planktivorous fish     8
#> 234   189.00       3     6 Small planktivorous fish     8
#> 235   189.00       4     0 Small planktivorous fish     8
#> 236   189.00       4     1 Small planktivorous fish     8
#> 237   189.00       4     2 Small planktivorous fish     8
#> 238   189.00       4     3 Small planktivorous fish     8
#> 239   189.00       4     4 Small planktivorous fish     8
#> 240   189.00       4     5 Small planktivorous fish     8
#> 241   189.00       4     6 Small planktivorous fish     8
#> 242   189.00       5     0 Small planktivorous fish     8
#> 243   189.00       5     1 Small planktivorous fish     8
#> 244   189.00       5     2 Small planktivorous fish     8
#> 245   189.00       5     3 Small planktivorous fish     8
#> 246   189.00       5     4 Small planktivorous fish     8
#> 247   189.00       5     5 Small planktivorous fish     8
#> 248   189.00       5     6 Small planktivorous fish     8
#> 249   193.00       1     0 Small planktivorous fish     9
#> 250   193.00       1     1 Small planktivorous fish     9
#> 251   193.00       1     2 Small planktivorous fish     9
#> 252   193.00       1     3 Small planktivorous fish     9
#> 253   193.00       1     6 Small planktivorous fish     9
#> 254   193.00       2     0 Small planktivorous fish     9
#> 255   193.00       2     1 Small planktivorous fish     9
#> 256   193.00       2     2 Small planktivorous fish     9
#> 257   193.00       2     3 Small planktivorous fish     9
#> 258   193.00       2     4 Small planktivorous fish     9
#> 259   193.00       2     6 Small planktivorous fish     9
#> 260   193.00       3     0 Small planktivorous fish     9
#> 261   193.00       3     1 Small planktivorous fish     9
#> 262   193.00       3     2 Small planktivorous fish     9
#> 263   193.00       3     3 Small planktivorous fish     9
#> 264   193.00       3     4 Small planktivorous fish     9
#> 265   193.00       3     6 Small planktivorous fish     9
#> 266   193.00       4     0 Small planktivorous fish     9
#> 267   193.00       4     1 Small planktivorous fish     9
#> 268   193.00       4     2 Small planktivorous fish     9
#> 269   193.00       4     3 Small planktivorous fish     9
#> 270   193.00       4     4 Small planktivorous fish     9
#> 271   193.00       4     5 Small planktivorous fish     9
#> 272   193.00       4     6 Small planktivorous fish     9
#> 273   193.00       5     0 Small planktivorous fish     9
#> 274   193.00       5     1 Small planktivorous fish     9
#> 275   193.00       5     2 Small planktivorous fish     9
#> 276   193.00       5     3 Small planktivorous fish     9
#> 277   193.00       5     4 Small planktivorous fish     9
#> 278   193.00       5     5 Small planktivorous fish     9
#> 279   193.00       5     6 Small planktivorous fish     9
#> 280   195.00       1     0 Small planktivorous fish    10
#> 281   195.00       1     1 Small planktivorous fish    10
#> 282   195.00       1     2 Small planktivorous fish    10
#> 283   195.00       1     3 Small planktivorous fish    10
#> 284   195.00       1     6 Small planktivorous fish    10
#> 285   195.00       2     0 Small planktivorous fish    10
#> 286   195.00       2     1 Small planktivorous fish    10
#> 287   195.00       2     2 Small planktivorous fish    10
#> 288   195.00       2     3 Small planktivorous fish    10
#> 289   195.00       2     4 Small planktivorous fish    10
#> 290   195.00       2     6 Small planktivorous fish    10
#> 291   195.00       3     0 Small planktivorous fish    10
#> 292   195.00       3     1 Small planktivorous fish    10
#> 293   195.00       3     2 Small planktivorous fish    10
#> 294   195.00       3     3 Small planktivorous fish    10
#> 295   195.00       3     4 Small planktivorous fish    10
#> 296   195.00       3     6 Small planktivorous fish    10
#> 297   195.00       4     0 Small planktivorous fish    10
#> 298   195.00       4     1 Small planktivorous fish    10
#> 299   195.00       4     2 Small planktivorous fish    10
#> 300   195.00       4     3 Small planktivorous fish    10
#> 301   195.00       4     4 Small planktivorous fish    10
#> 302   195.00       4     5 Small planktivorous fish    10
#> 303   195.00       4     6 Small planktivorous fish    10
#> 304   195.00       5     0 Small planktivorous fish    10
#> 305   195.00       5     1 Small planktivorous fish    10
#> 306   195.00       5     2 Small planktivorous fish    10
#> 307   195.00       5     3 Small planktivorous fish    10
#> 308   195.00       5     4 Small planktivorous fish    10
#> 309   195.00       5     5 Small planktivorous fish    10
#> 310   195.00       5     6 Small planktivorous fish    10

load_init_age(init = init, fgs = fgs, bboxes = bboxes, select_variable = "ResN")
#>     atoutput polygon layer                  species agecl
#> 1       0.25       1     0 Small planktivorous fish     1
#> 2       0.25       1     1 Small planktivorous fish     1
#> 3       0.25       1     2 Small planktivorous fish     1
#> 4       0.25       1     3 Small planktivorous fish     1
#> 5       0.25       1     6 Small planktivorous fish     1
#> 6       0.25       2     0 Small planktivorous fish     1
#> 7       0.25       2     1 Small planktivorous fish     1
#> 8       0.25       2     2 Small planktivorous fish     1
#> 9       0.25       2     3 Small planktivorous fish     1
#> 10      0.25       2     4 Small planktivorous fish     1
#> 11      0.25       2     6 Small planktivorous fish     1
#> 12      0.25       3     0 Small planktivorous fish     1
#> 13      0.25       3     1 Small planktivorous fish     1
#> 14      0.25       3     2 Small planktivorous fish     1
#> 15      0.25       3     3 Small planktivorous fish     1
#> 16      0.25       3     4 Small planktivorous fish     1
#> 17      0.25       3     6 Small planktivorous fish     1
#> 18      0.25       4     0 Small planktivorous fish     1
#> 19      0.25       4     1 Small planktivorous fish     1
#> 20      0.25       4     2 Small planktivorous fish     1
#> 21      0.25       4     3 Small planktivorous fish     1
#> 22      0.25       4     4 Small planktivorous fish     1
#> 23      0.25       4     5 Small planktivorous fish     1
#> 24      0.25       4     6 Small planktivorous fish     1
#> 25      0.25       5     0 Small planktivorous fish     1
#> 26      0.25       5     1 Small planktivorous fish     1
#> 27      0.25       5     2 Small planktivorous fish     1
#> 28      0.25       5     3 Small planktivorous fish     1
#> 29      0.25       5     4 Small planktivorous fish     1
#> 30      0.25       5     5 Small planktivorous fish     1
#> 31      0.25       5     6 Small planktivorous fish     1
#> 32     18.00       1     0 Small planktivorous fish     2
#> 33     18.00       1     1 Small planktivorous fish     2
#> 34     18.00       1     2 Small planktivorous fish     2
#> 35     18.00       1     3 Small planktivorous fish     2
#> 36     18.00       1     6 Small planktivorous fish     2
#> 37     18.00       2     0 Small planktivorous fish     2
#> 38     18.00       2     1 Small planktivorous fish     2
#> 39     18.00       2     2 Small planktivorous fish     2
#> 40     18.00       2     3 Small planktivorous fish     2
#> 41     18.00       2     4 Small planktivorous fish     2
#> 42     18.00       2     6 Small planktivorous fish     2
#> 43     18.00       3     0 Small planktivorous fish     2
#> 44     18.00       3     1 Small planktivorous fish     2
#> 45     18.00       3     2 Small planktivorous fish     2
#> 46     18.00       3     3 Small planktivorous fish     2
#> 47     18.00       3     4 Small planktivorous fish     2
#> 48     18.00       3     6 Small planktivorous fish     2
#> 49     18.00       4     0 Small planktivorous fish     2
#> 50     18.00       4     1 Small planktivorous fish     2
#> 51     18.00       4     2 Small planktivorous fish     2
#> 52     18.00       4     3 Small planktivorous fish     2
#> 53     18.00       4     4 Small planktivorous fish     2
#> 54     18.00       4     5 Small planktivorous fish     2
#> 55     18.00       4     6 Small planktivorous fish     2
#> 56     18.00       5     0 Small planktivorous fish     2
#> 57     18.00       5     1 Small planktivorous fish     2
#> 58     18.00       5     2 Small planktivorous fish     2
#> 59     18.00       5     3 Small planktivorous fish     2
#> 60     18.00       5     4 Small planktivorous fish     2
#> 61     18.00       5     5 Small planktivorous fish     2
#> 62     18.00       5     6 Small planktivorous fish     2
#> 63     68.00       1     0 Small planktivorous fish     3
#> 64     68.00       1     1 Small planktivorous fish     3
#> 65     68.00       1     2 Small planktivorous fish     3
#> 66     68.00       1     3 Small planktivorous fish     3
#> 67     68.00       1     6 Small planktivorous fish     3
#> 68     68.00       2     0 Small planktivorous fish     3
#> 69     68.00       2     1 Small planktivorous fish     3
#> 70     68.00       2     2 Small planktivorous fish     3
#> 71     68.00       2     3 Small planktivorous fish     3
#> 72     68.00       2     4 Small planktivorous fish     3
#> 73     68.00       2     6 Small planktivorous fish     3
#> 74     68.00       3     0 Small planktivorous fish     3
#> 75     68.00       3     1 Small planktivorous fish     3
#> 76     68.00       3     2 Small planktivorous fish     3
#> 77     68.00       3     3 Small planktivorous fish     3
#> 78     68.00       3     4 Small planktivorous fish     3
#> 79     68.00       3     6 Small planktivorous fish     3
#> 80     68.00       4     0 Small planktivorous fish     3
#> 81     68.00       4     1 Small planktivorous fish     3
#> 82     68.00       4     2 Small planktivorous fish     3
#> 83     68.00       4     3 Small planktivorous fish     3
#> 84     68.00       4     4 Small planktivorous fish     3
#> 85     68.00       4     5 Small planktivorous fish     3
#> 86     68.00       4     6 Small planktivorous fish     3
#> 87     68.00       5     0 Small planktivorous fish     3
#> 88     68.00       5     1 Small planktivorous fish     3
#> 89     68.00       5     2 Small planktivorous fish     3
#> 90     68.00       5     3 Small planktivorous fish     3
#> 91     68.00       5     4 Small planktivorous fish     3
#> 92     68.00       5     5 Small planktivorous fish     3
#> 93     68.00       5     6 Small planktivorous fish     3
#> 94    115.00       1     0 Small planktivorous fish     4
#> 95    115.00       1     1 Small planktivorous fish     4
#> 96    115.00       1     2 Small planktivorous fish     4
#> 97    115.00       1     3 Small planktivorous fish     4
#> 98    115.00       1     6 Small planktivorous fish     4
#> 99    115.00       2     0 Small planktivorous fish     4
#> 100   115.00       2     1 Small planktivorous fish     4
#> 101   115.00       2     2 Small planktivorous fish     4
#> 102   115.00       2     3 Small planktivorous fish     4
#> 103   115.00       2     4 Small planktivorous fish     4
#> 104   115.00       2     6 Small planktivorous fish     4
#> 105   115.00       3     0 Small planktivorous fish     4
#> 106   115.00       3     1 Small planktivorous fish     4
#> 107   115.00       3     2 Small planktivorous fish     4
#> 108   115.00       3     3 Small planktivorous fish     4
#> 109   115.00       3     4 Small planktivorous fish     4
#> 110   115.00       3     6 Small planktivorous fish     4
#> 111   115.00       4     0 Small planktivorous fish     4
#> 112   115.00       4     1 Small planktivorous fish     4
#> 113   115.00       4     2 Small planktivorous fish     4
#> 114   115.00       4     3 Small planktivorous fish     4
#> 115   115.00       4     4 Small planktivorous fish     4
#> 116   115.00       4     5 Small planktivorous fish     4
#> 117   115.00       4     6 Small planktivorous fish     4
#> 118   115.00       5     0 Small planktivorous fish     4
#> 119   115.00       5     1 Small planktivorous fish     4
#> 120   115.00       5     2 Small planktivorous fish     4
#> 121   115.00       5     3 Small planktivorous fish     4
#> 122   115.00       5     4 Small planktivorous fish     4
#> 123   115.00       5     5 Small planktivorous fish     4
#> 124   115.00       5     6 Small planktivorous fish     4
#> 125   150.00       1     0 Small planktivorous fish     5
#> 126   150.00       1     1 Small planktivorous fish     5
#> 127   150.00       1     2 Small planktivorous fish     5
#> 128   150.00       1     3 Small planktivorous fish     5
#> 129   150.00       1     6 Small planktivorous fish     5
#> 130   150.00       2     0 Small planktivorous fish     5
#> 131   150.00       2     1 Small planktivorous fish     5
#> 132   150.00       2     2 Small planktivorous fish     5
#> 133   150.00       2     3 Small planktivorous fish     5
#> 134   150.00       2     4 Small planktivorous fish     5
#> 135   150.00       2     6 Small planktivorous fish     5
#> 136   150.00       3     0 Small planktivorous fish     5
#> 137   150.00       3     1 Small planktivorous fish     5
#> 138   150.00       3     2 Small planktivorous fish     5
#> 139   150.00       3     3 Small planktivorous fish     5
#> 140   150.00       3     4 Small planktivorous fish     5
#> 141   150.00       3     6 Small planktivorous fish     5
#> 142   150.00       4     0 Small planktivorous fish     5
#> 143   150.00       4     1 Small planktivorous fish     5
#> 144   150.00       4     2 Small planktivorous fish     5
#> 145   150.00       4     3 Small planktivorous fish     5
#> 146   150.00       4     4 Small planktivorous fish     5
#> 147   150.00       4     5 Small planktivorous fish     5
#> 148   150.00       4     6 Small planktivorous fish     5
#> 149   150.00       5     0 Small planktivorous fish     5
#> 150   150.00       5     1 Small planktivorous fish     5
#> 151   150.00       5     2 Small planktivorous fish     5
#> 152   150.00       5     3 Small planktivorous fish     5
#> 153   150.00       5     4 Small planktivorous fish     5
#> 154   150.00       5     5 Small planktivorous fish     5
#> 155   150.00       5     6 Small planktivorous fish     5
#> 156   170.00       1     0 Small planktivorous fish     6
#> 157   170.00       1     1 Small planktivorous fish     6
#> 158   170.00       1     2 Small planktivorous fish     6
#> 159   170.00       1     3 Small planktivorous fish     6
#> 160   170.00       1     6 Small planktivorous fish     6
#> 161   170.00       2     0 Small planktivorous fish     6
#> 162   170.00       2     1 Small planktivorous fish     6
#> 163   170.00       2     2 Small planktivorous fish     6
#> 164   170.00       2     3 Small planktivorous fish     6
#> 165   170.00       2     4 Small planktivorous fish     6
#> 166   170.00       2     6 Small planktivorous fish     6
#> 167   170.00       3     0 Small planktivorous fish     6
#> 168   170.00       3     1 Small planktivorous fish     6
#> 169   170.00       3     2 Small planktivorous fish     6
#> 170   170.00       3     3 Small planktivorous fish     6
#> 171   170.00       3     4 Small planktivorous fish     6
#> 172   170.00       3     6 Small planktivorous fish     6
#> 173   170.00       4     0 Small planktivorous fish     6
#> 174   170.00       4     1 Small planktivorous fish     6
#> 175   170.00       4     2 Small planktivorous fish     6
#> 176   170.00       4     3 Small planktivorous fish     6
#> 177   170.00       4     4 Small planktivorous fish     6
#> 178   170.00       4     5 Small planktivorous fish     6
#> 179   170.00       4     6 Small planktivorous fish     6
#> 180   170.00       5     0 Small planktivorous fish     6
#> 181   170.00       5     1 Small planktivorous fish     6
#> 182   170.00       5     2 Small planktivorous fish     6
#> 183   170.00       5     3 Small planktivorous fish     6
#> 184   170.00       5     4 Small planktivorous fish     6
#> 185   170.00       5     5 Small planktivorous fish     6
#> 186   170.00       5     6 Small planktivorous fish     6
#> 187   182.00       1     0 Small planktivorous fish     7
#> 188   182.00       1     1 Small planktivorous fish     7
#> 189   182.00       1     2 Small planktivorous fish     7
#> 190   182.00       1     3 Small planktivorous fish     7
#> 191   182.00       1     6 Small planktivorous fish     7
#> 192   182.00       2     0 Small planktivorous fish     7
#> 193   182.00       2     1 Small planktivorous fish     7
#> 194   182.00       2     2 Small planktivorous fish     7
#> 195   182.00       2     3 Small planktivorous fish     7
#> 196   182.00       2     4 Small planktivorous fish     7
#> 197   182.00       2     6 Small planktivorous fish     7
#> 198   182.00       3     0 Small planktivorous fish     7
#> 199   182.00       3     1 Small planktivorous fish     7
#> 200   182.00       3     2 Small planktivorous fish     7
#> 201   182.00       3     3 Small planktivorous fish     7
#> 202   182.00       3     4 Small planktivorous fish     7
#> 203   182.00       3     6 Small planktivorous fish     7
#> 204   182.00       4     0 Small planktivorous fish     7
#> 205   182.00       4     1 Small planktivorous fish     7
#> 206   182.00       4     2 Small planktivorous fish     7
#> 207   182.00       4     3 Small planktivorous fish     7
#> 208   182.00       4     4 Small planktivorous fish     7
#> 209   182.00       4     5 Small planktivorous fish     7
#> 210   182.00       4     6 Small planktivorous fish     7
#> 211   182.00       5     0 Small planktivorous fish     7
#> 212   182.00       5     1 Small planktivorous fish     7
#> 213   182.00       5     2 Small planktivorous fish     7
#> 214   182.00       5     3 Small planktivorous fish     7
#> 215   182.00       5     4 Small planktivorous fish     7
#> 216   182.00       5     5 Small planktivorous fish     7
#> 217   182.00       5     6 Small planktivorous fish     7
#> 218   189.00       1     0 Small planktivorous fish     8
#> 219   189.00       1     1 Small planktivorous fish     8
#> 220   189.00       1     2 Small planktivorous fish     8
#> 221   189.00       1     3 Small planktivorous fish     8
#> 222   189.00       1     6 Small planktivorous fish     8
#> 223   189.00       2     0 Small planktivorous fish     8
#> 224   189.00       2     1 Small planktivorous fish     8
#> 225   189.00       2     2 Small planktivorous fish     8
#> 226   189.00       2     3 Small planktivorous fish     8
#> 227   189.00       2     4 Small planktivorous fish     8
#> 228   189.00       2     6 Small planktivorous fish     8
#> 229   189.00       3     0 Small planktivorous fish     8
#> 230   189.00       3     1 Small planktivorous fish     8
#> 231   189.00       3     2 Small planktivorous fish     8
#> 232   189.00       3     3 Small planktivorous fish     8
#> 233   189.00       3     4 Small planktivorous fish     8
#> 234   189.00       3     6 Small planktivorous fish     8
#> 235   189.00       4     0 Small planktivorous fish     8
#> 236   189.00       4     1 Small planktivorous fish     8
#> 237   189.00       4     2 Small planktivorous fish     8
#> 238   189.00       4     3 Small planktivorous fish     8
#> 239   189.00       4     4 Small planktivorous fish     8
#> 240   189.00       4     5 Small planktivorous fish     8
#> 241   189.00       4     6 Small planktivorous fish     8
#> 242   189.00       5     0 Small planktivorous fish     8
#> 243   189.00       5     1 Small planktivorous fish     8
#> 244   189.00       5     2 Small planktivorous fish     8
#> 245   189.00       5     3 Small planktivorous fish     8
#> 246   189.00       5     4 Small planktivorous fish     8
#> 247   189.00       5     5 Small planktivorous fish     8
#> 248   189.00       5     6 Small planktivorous fish     8
#> 249   193.00       1     0 Small planktivorous fish     9
#> 250   193.00       1     1 Small planktivorous fish     9
#> 251   193.00       1     2 Small planktivorous fish     9
#> 252   193.00       1     3 Small planktivorous fish     9
#> 253   193.00       1     6 Small planktivorous fish     9
#> 254   193.00       2     0 Small planktivorous fish     9
#> 255   193.00       2     1 Small planktivorous fish     9
#> 256   193.00       2     2 Small planktivorous fish     9
#> 257   193.00       2     3 Small planktivorous fish     9
#> 258   193.00       2     4 Small planktivorous fish     9
#> 259   193.00       2     6 Small planktivorous fish     9
#> 260   193.00       3     0 Small planktivorous fish     9
#> 261   193.00       3     1 Small planktivorous fish     9
#> 262   193.00       3     2 Small planktivorous fish     9
#> 263   193.00       3     3 Small planktivorous fish     9
#> 264   193.00       3     4 Small planktivorous fish     9
#> 265   193.00       3     6 Small planktivorous fish     9
#> 266   193.00       4     0 Small planktivorous fish     9
#> 267   193.00       4     1 Small planktivorous fish     9
#> 268   193.00       4     2 Small planktivorous fish     9
#> 269   193.00       4     3 Small planktivorous fish     9
#> 270   193.00       4     4 Small planktivorous fish     9
#> 271   193.00       4     5 Small planktivorous fish     9
#> 272   193.00       4     6 Small planktivorous fish     9
#> 273   193.00       5     0 Small planktivorous fish     9
#> 274   193.00       5     1 Small planktivorous fish     9
#> 275   193.00       5     2 Small planktivorous fish     9
#> 276   193.00       5     3 Small planktivorous fish     9
#> 277   193.00       5     4 Small planktivorous fish     9
#> 278   193.00       5     5 Small planktivorous fish     9
#> 279   193.00       5     6 Small planktivorous fish     9
#> 280   195.00       1     0 Small planktivorous fish    10
#> 281   195.00       1     1 Small planktivorous fish    10
#> 282   195.00       1     2 Small planktivorous fish    10
#> 283   195.00       1     3 Small planktivorous fish    10
#> 284   195.00       1     6 Small planktivorous fish    10
#> 285   195.00       2     0 Small planktivorous fish    10
#> 286   195.00       2     1 Small planktivorous fish    10
#> 287   195.00       2     2 Small planktivorous fish    10
#> 288   195.00       2     3 Small planktivorous fish    10
#> 289   195.00       2     4 Small planktivorous fish    10
#> 290   195.00       2     6 Small planktivorous fish    10
#> 291   195.00       3     0 Small planktivorous fish    10
#> 292   195.00       3     1 Small planktivorous fish    10
#> 293   195.00       3     2 Small planktivorous fish    10
#> 294   195.00       3     3 Small planktivorous fish    10
#> 295   195.00       3     4 Small planktivorous fish    10
#> 296   195.00       3     6 Small planktivorous fish    10
#> 297   195.00       4     0 Small planktivorous fish    10
#> 298   195.00       4     1 Small planktivorous fish    10
#> 299   195.00       4     2 Small planktivorous fish    10
#> 300   195.00       4     3 Small planktivorous fish    10
#> 301   195.00       4     4 Small planktivorous fish    10
#> 302   195.00       4     5 Small planktivorous fish    10
#> 303   195.00       4     6 Small planktivorous fish    10
#> 304   195.00       5     0 Small planktivorous fish    10
#> 305   195.00       5     1 Small planktivorous fish    10
#> 306   195.00       5     2 Small planktivorous fish    10
#> 307   195.00       5     3 Small planktivorous fish    10
#> 308   195.00       5     4 Small planktivorous fish    10
#> 309   195.00       5     5 Small planktivorous fish    10
#> 310   195.00       5     6 Small planktivorous fish    10
#> 311  9837.00       1     0 Shallow piscivorous fish     1
#> 312  9837.00       1     1 Shallow piscivorous fish     1
#> 313  9837.00       1     2 Shallow piscivorous fish     1
#> 314  9837.00       1     3 Shallow piscivorous fish     1
#> 315  9837.00       1     6 Shallow piscivorous fish     1
#> 316  9837.00       2     0 Shallow piscivorous fish     1
#> 317  9837.00       2     1 Shallow piscivorous fish     1
#> 318  9837.00       2     2 Shallow piscivorous fish     1
#> 319  9837.00       2     3 Shallow piscivorous fish     1
#> 320  9837.00       2     4 Shallow piscivorous fish     1
#> 321  9837.00       2     6 Shallow piscivorous fish     1
#> 322  9837.00       3     0 Shallow piscivorous fish     1
#> 323  9837.00       3     1 Shallow piscivorous fish     1
#> 324  9837.00       3     2 Shallow piscivorous fish     1
#> 325  9837.00       3     3 Shallow piscivorous fish     1
#> 326  9837.00       3     4 Shallow piscivorous fish     1
#> 327  9837.00       3     6 Shallow piscivorous fish     1
#> 328  9837.00       4     0 Shallow piscivorous fish     1
#> 329  9837.00       4     1 Shallow piscivorous fish     1
#> 330  9837.00       4     2 Shallow piscivorous fish     1
#> 331  9837.00       4     3 Shallow piscivorous fish     1
#> 332  9837.00       4     4 Shallow piscivorous fish     1
#> 333  9837.00       4     5 Shallow piscivorous fish     1
#> 334  9837.00       4     6 Shallow piscivorous fish     1
#> 335  9837.00       5     0 Shallow piscivorous fish     1
#> 336  9837.00       5     1 Shallow piscivorous fish     1
#> 337  9837.00       5     2 Shallow piscivorous fish     1
#> 338  9837.00       5     3 Shallow piscivorous fish     1
#> 339  9837.00       5     4 Shallow piscivorous fish     1
#> 340  9837.00       5     5 Shallow piscivorous fish     1
#> 341  9837.00       5     6 Shallow piscivorous fish     1
#> 342 20113.00       1     0 Shallow piscivorous fish     2
#> 343 20113.00       1     1 Shallow piscivorous fish     2
#> 344 20113.00       1     2 Shallow piscivorous fish     2
#> 345 20113.00       1     3 Shallow piscivorous fish     2
#> 346 20113.00       1     6 Shallow piscivorous fish     2
#> 347 20113.00       2     0 Shallow piscivorous fish     2
#> 348 20113.00       2     1 Shallow piscivorous fish     2
#> 349 20113.00       2     2 Shallow piscivorous fish     2
#> 350 20113.00       2     3 Shallow piscivorous fish     2
#> 351 20113.00       2     4 Shallow piscivorous fish     2
#> 352 20113.00       2     6 Shallow piscivorous fish     2
#> 353 20113.00       3     0 Shallow piscivorous fish     2
#> 354 20113.00       3     1 Shallow piscivorous fish     2
#> 355 20113.00       3     2 Shallow piscivorous fish     2
#> 356 20113.00       3     3 Shallow piscivorous fish     2
#> 357 20113.00       3     4 Shallow piscivorous fish     2
#> 358 20113.00       3     6 Shallow piscivorous fish     2
#> 359 20113.00       4     0 Shallow piscivorous fish     2
#> 360 20113.00       4     1 Shallow piscivorous fish     2
#> 361 20113.00       4     2 Shallow piscivorous fish     2
#> 362 20113.00       4     3 Shallow piscivorous fish     2
#> 363 20113.00       4     4 Shallow piscivorous fish     2
#> 364 20113.00       4     5 Shallow piscivorous fish     2
#> 365 20113.00       4     6 Shallow piscivorous fish     2
#> 366 20113.00       5     0 Shallow piscivorous fish     2
#> 367 20113.00       5     1 Shallow piscivorous fish     2
#> 368 20113.00       5     2 Shallow piscivorous fish     2
#> 369 20113.00       5     3 Shallow piscivorous fish     2
#> 370 20113.00       5     4 Shallow piscivorous fish     2
#> 371 20113.00       5     5 Shallow piscivorous fish     2
#> 372 20113.00       5     6 Shallow piscivorous fish     2
#> 373 24083.00       1     0 Shallow piscivorous fish     3
#> 374 24083.00       1     1 Shallow piscivorous fish     3
#> 375 24083.00       1     2 Shallow piscivorous fish     3
#> 376 24083.00       1     3 Shallow piscivorous fish     3
#> 377 24083.00       1     6 Shallow piscivorous fish     3
#> 378 24083.00       2     0 Shallow piscivorous fish     3
#> 379 24083.00       2     1 Shallow piscivorous fish     3
#> 380 24083.00       2     2 Shallow piscivorous fish     3
#> 381 24083.00       2     3 Shallow piscivorous fish     3
#> 382 24083.00       2     4 Shallow piscivorous fish     3
#> 383 24083.00       2     6 Shallow piscivorous fish     3
#> 384 24083.00       3     0 Shallow piscivorous fish     3
#> 385 24083.00       3     1 Shallow piscivorous fish     3
#> 386 24083.00       3     2 Shallow piscivorous fish     3
#> 387 24083.00       3     3 Shallow piscivorous fish     3
#> 388 24083.00       3     4 Shallow piscivorous fish     3
#> 389 24083.00       3     6 Shallow piscivorous fish     3
#> 390 24083.00       4     0 Shallow piscivorous fish     3
#> 391 24083.00       4     1 Shallow piscivorous fish     3
#> 392 24083.00       4     2 Shallow piscivorous fish     3
#> 393 24083.00       4     3 Shallow piscivorous fish     3
#> 394 24083.00       4     4 Shallow piscivorous fish     3
#> 395 24083.00       4     5 Shallow piscivorous fish     3
#> 396 24083.00       4     6 Shallow piscivorous fish     3
#> 397 24083.00       5     0 Shallow piscivorous fish     3
#> 398 24083.00       5     1 Shallow piscivorous fish     3
#> 399 24083.00       5     2 Shallow piscivorous fish     3
#> 400 24083.00       5     3 Shallow piscivorous fish     3
#> 401 24083.00       5     4 Shallow piscivorous fish     3
#> 402 24083.00       5     5 Shallow piscivorous fish     3
#> 403 24083.00       5     6 Shallow piscivorous fish     3
#> 404 25177.00       1     0 Shallow piscivorous fish     4
#> 405 25177.00       1     1 Shallow piscivorous fish     4
#> 406 25177.00       1     2 Shallow piscivorous fish     4
#> 407 25177.00       1     3 Shallow piscivorous fish     4
#> 408 25177.00       1     6 Shallow piscivorous fish     4
#> 409 25177.00       2     0 Shallow piscivorous fish     4
#> 410 25177.00       2     1 Shallow piscivorous fish     4
#> 411 25177.00       2     2 Shallow piscivorous fish     4
#> 412 25177.00       2     3 Shallow piscivorous fish     4
#> 413 25177.00       2     4 Shallow piscivorous fish     4
#> 414 25177.00       2     6 Shallow piscivorous fish     4
#> 415 25177.00       3     0 Shallow piscivorous fish     4
#> 416 25177.00       3     1 Shallow piscivorous fish     4
#> 417 25177.00       3     2 Shallow piscivorous fish     4
#> 418 25177.00       3     3 Shallow piscivorous fish     4
#> 419 25177.00       3     4 Shallow piscivorous fish     4
#> 420 25177.00       3     6 Shallow piscivorous fish     4
#> 421 25177.00       4     0 Shallow piscivorous fish     4
#> 422 25177.00       4     1 Shallow piscivorous fish     4
#> 423 25177.00       4     2 Shallow piscivorous fish     4
#> 424 25177.00       4     3 Shallow piscivorous fish     4
#> 425 25177.00       4     4 Shallow piscivorous fish     4
#> 426 25177.00       4     5 Shallow piscivorous fish     4
#> 427 25177.00       4     6 Shallow piscivorous fish     4
#> 428 25177.00       5     0 Shallow piscivorous fish     4
#> 429 25177.00       5     1 Shallow piscivorous fish     4
#> 430 25177.00       5     2 Shallow piscivorous fish     4
#> 431 25177.00       5     3 Shallow piscivorous fish     4
#> 432 25177.00       5     4 Shallow piscivorous fish     4
#> 433 25177.00       5     5 Shallow piscivorous fish     4
#> 434 25177.00       5     6 Shallow piscivorous fish     4
#> 435 25472.00       1     0 Shallow piscivorous fish     5
#> 436 25472.00       1     1 Shallow piscivorous fish     5
#> 437 25472.00       1     2 Shallow piscivorous fish     5
#> 438 25472.00       1     3 Shallow piscivorous fish     5
#> 439 25472.00       1     6 Shallow piscivorous fish     5
#> 440 25472.00       2     0 Shallow piscivorous fish     5
#> 441 25472.00       2     1 Shallow piscivorous fish     5
#> 442 25472.00       2     2 Shallow piscivorous fish     5
#> 443 25472.00       2     3 Shallow piscivorous fish     5
#> 444 25472.00       2     4 Shallow piscivorous fish     5
#> 445 25472.00       2     6 Shallow piscivorous fish     5
#> 446 25472.00       3     0 Shallow piscivorous fish     5
#> 447 25472.00       3     1 Shallow piscivorous fish     5
#> 448 25472.00       3     2 Shallow piscivorous fish     5
#> 449 25472.00       3     3 Shallow piscivorous fish     5
#> 450 25472.00       3     4 Shallow piscivorous fish     5
#> 451 25472.00       3     6 Shallow piscivorous fish     5
#> 452 25472.00       4     0 Shallow piscivorous fish     5
#> 453 25472.00       4     1 Shallow piscivorous fish     5
#> 454 25472.00       4     2 Shallow piscivorous fish     5
#> 455 25472.00       4     3 Shallow piscivorous fish     5
#> 456 25472.00       4     4 Shallow piscivorous fish     5
#> 457 25472.00       4     5 Shallow piscivorous fish     5
#> 458 25472.00       4     6 Shallow piscivorous fish     5
#> 459 25472.00       5     0 Shallow piscivorous fish     5
#> 460 25472.00       5     1 Shallow piscivorous fish     5
#> 461 25472.00       5     2 Shallow piscivorous fish     5
#> 462 25472.00       5     3 Shallow piscivorous fish     5
#> 463 25472.00       5     4 Shallow piscivorous fish     5
#> 464 25472.00       5     5 Shallow piscivorous fish     5
#> 465 25472.00       5     6 Shallow piscivorous fish     5
#> 466 25613.00       1     0 Shallow piscivorous fish     6
#> 467 25613.00       1     1 Shallow piscivorous fish     6
#> 468 25613.00       1     2 Shallow piscivorous fish     6
#> 469 25613.00       1     3 Shallow piscivorous fish     6
#> 470 25613.00       1     6 Shallow piscivorous fish     6
#> 471 25613.00       2     0 Shallow piscivorous fish     6
#> 472 25613.00       2     1 Shallow piscivorous fish     6
#> 473 25613.00       2     2 Shallow piscivorous fish     6
#> 474 25613.00       2     3 Shallow piscivorous fish     6
#> 475 25613.00       2     4 Shallow piscivorous fish     6
#> 476 25613.00       2     6 Shallow piscivorous fish     6
#> 477 25613.00       3     0 Shallow piscivorous fish     6
#> 478 25613.00       3     1 Shallow piscivorous fish     6
#> 479 25613.00       3     2 Shallow piscivorous fish     6
#> 480 25613.00       3     3 Shallow piscivorous fish     6
#> 481 25613.00       3     4 Shallow piscivorous fish     6
#> 482 25613.00       3     6 Shallow piscivorous fish     6
#> 483 25613.00       4     0 Shallow piscivorous fish     6
#> 484 25613.00       4     1 Shallow piscivorous fish     6
#> 485 25613.00       4     2 Shallow piscivorous fish     6
#> 486 25613.00       4     3 Shallow piscivorous fish     6
#> 487 25613.00       4     4 Shallow piscivorous fish     6
#> 488 25613.00       4     5 Shallow piscivorous fish     6
#> 489 25613.00       4     6 Shallow piscivorous fish     6
#> 490 25613.00       5     0 Shallow piscivorous fish     6
#> 491 25613.00       5     1 Shallow piscivorous fish     6
#> 492 25613.00       5     2 Shallow piscivorous fish     6
#> 493 25613.00       5     3 Shallow piscivorous fish     6
#> 494 25613.00       5     4 Shallow piscivorous fish     6
#> 495 25613.00       5     5 Shallow piscivorous fish     6
#> 496 25613.00       5     6 Shallow piscivorous fish     6
#> 497 25653.00       1     0 Shallow piscivorous fish     7
#> 498 25653.00       1     1 Shallow piscivorous fish     7
#> 499 25653.00       1     2 Shallow piscivorous fish     7
#> 500 25653.00       1     3 Shallow piscivorous fish     7
#> 501 25653.00       1     6 Shallow piscivorous fish     7
#> 502 25653.00       2     0 Shallow piscivorous fish     7
#> 503 25653.00       2     1 Shallow piscivorous fish     7
#> 504 25653.00       2     2 Shallow piscivorous fish     7
#> 505 25653.00       2     3 Shallow piscivorous fish     7
#> 506 25653.00       2     4 Shallow piscivorous fish     7
#> 507 25653.00       2     6 Shallow piscivorous fish     7
#> 508 25653.00       3     0 Shallow piscivorous fish     7
#> 509 25653.00       3     1 Shallow piscivorous fish     7
#> 510 25653.00       3     2 Shallow piscivorous fish     7
#> 511 25653.00       3     3 Shallow piscivorous fish     7
#> 512 25653.00       3     4 Shallow piscivorous fish     7
#> 513 25653.00       3     6 Shallow piscivorous fish     7
#> 514 25653.00       4     0 Shallow piscivorous fish     7
#> 515 25653.00       4     1 Shallow piscivorous fish     7
#> 516 25653.00       4     2 Shallow piscivorous fish     7
#> 517 25653.00       4     3 Shallow piscivorous fish     7
#> 518 25653.00       4     4 Shallow piscivorous fish     7
#> 519 25653.00       4     5 Shallow piscivorous fish     7
#> 520 25653.00       4     6 Shallow piscivorous fish     7
#> 521 25653.00       5     0 Shallow piscivorous fish     7
#> 522 25653.00       5     1 Shallow piscivorous fish     7
#> 523 25653.00       5     2 Shallow piscivorous fish     7
#> 524 25653.00       5     3 Shallow piscivorous fish     7
#> 525 25653.00       5     4 Shallow piscivorous fish     7
#> 526 25653.00       5     5 Shallow piscivorous fish     7
#> 527 25653.00       5     6 Shallow piscivorous fish     7
#> 528 25665.00       1     0 Shallow piscivorous fish     8
#> 529 25665.00       1     1 Shallow piscivorous fish     8
#> 530 25665.00       1     2 Shallow piscivorous fish     8
#> 531 25665.00       1     3 Shallow piscivorous fish     8
#> 532 25665.00       1     6 Shallow piscivorous fish     8
#> 533 25665.00       2     0 Shallow piscivorous fish     8
#> 534 25665.00       2     1 Shallow piscivorous fish     8
#> 535 25665.00       2     2 Shallow piscivorous fish     8
#> 536 25665.00       2     3 Shallow piscivorous fish     8
#> 537 25665.00       2     4 Shallow piscivorous fish     8
#> 538 25665.00       2     6 Shallow piscivorous fish     8
#> 539 25665.00       3     0 Shallow piscivorous fish     8
#> 540 25665.00       3     1 Shallow piscivorous fish     8
#> 541 25665.00       3     2 Shallow piscivorous fish     8
#> 542 25665.00       3     3 Shallow piscivorous fish     8
#> 543 25665.00       3     4 Shallow piscivorous fish     8
#> 544 25665.00       3     6 Shallow piscivorous fish     8
#> 545 25665.00       4     0 Shallow piscivorous fish     8
#> 546 25665.00       4     1 Shallow piscivorous fish     8
#> 547 25665.00       4     2 Shallow piscivorous fish     8
#> 548 25665.00       4     3 Shallow piscivorous fish     8
#> 549 25665.00       4     4 Shallow piscivorous fish     8
#> 550 25665.00       4     5 Shallow piscivorous fish     8
#> 551 25665.00       4     6 Shallow piscivorous fish     8
#> 552 25665.00       5     0 Shallow piscivorous fish     8
#> 553 25665.00       5     1 Shallow piscivorous fish     8
#> 554 25665.00       5     2 Shallow piscivorous fish     8
#> 555 25665.00       5     3 Shallow piscivorous fish     8
#> 556 25665.00       5     4 Shallow piscivorous fish     8
#> 557 25665.00       5     5 Shallow piscivorous fish     8
#> 558 25665.00       5     6 Shallow piscivorous fish     8
#> 559 25668.00       1     0 Shallow piscivorous fish     9
#> 560 25668.00       1     1 Shallow piscivorous fish     9
#> 561 25668.00       1     2 Shallow piscivorous fish     9
#> 562 25668.00       1     3 Shallow piscivorous fish     9
#> 563 25668.00       1     6 Shallow piscivorous fish     9
#> 564 25668.00       2     0 Shallow piscivorous fish     9
#> 565 25668.00       2     1 Shallow piscivorous fish     9
#> 566 25668.00       2     2 Shallow piscivorous fish     9
#> 567 25668.00       2     3 Shallow piscivorous fish     9
#> 568 25668.00       2     4 Shallow piscivorous fish     9
#> 569 25668.00       2     6 Shallow piscivorous fish     9
#> 570 25668.00       3     0 Shallow piscivorous fish     9
#> 571 25668.00       3     1 Shallow piscivorous fish     9
#> 572 25668.00       3     2 Shallow piscivorous fish     9
#> 573 25668.00       3     3 Shallow piscivorous fish     9
#> 574 25668.00       3     4 Shallow piscivorous fish     9
#> 575 25668.00       3     6 Shallow piscivorous fish     9
#> 576 25668.00       4     0 Shallow piscivorous fish     9
#> 577 25668.00       4     1 Shallow piscivorous fish     9
#> 578 25668.00       4     2 Shallow piscivorous fish     9
#> 579 25668.00       4     3 Shallow piscivorous fish     9
#> 580 25668.00       4     4 Shallow piscivorous fish     9
#> 581 25668.00       4     5 Shallow piscivorous fish     9
#> 582 25668.00       4     6 Shallow piscivorous fish     9
#> 583 25668.00       5     0 Shallow piscivorous fish     9
#> 584 25668.00       5     1 Shallow piscivorous fish     9
#> 585 25668.00       5     2 Shallow piscivorous fish     9
#> 586 25668.00       5     3 Shallow piscivorous fish     9
#> 587 25668.00       5     4 Shallow piscivorous fish     9
#> 588 25668.00       5     5 Shallow piscivorous fish     9
#> 589 25668.00       5     6 Shallow piscivorous fish     9
#> 590 25669.00       1     0 Shallow piscivorous fish    10
#> 591 25669.00       1     1 Shallow piscivorous fish    10
#> 592 25669.00       1     2 Shallow piscivorous fish    10
#> 593 25669.00       1     3 Shallow piscivorous fish    10
#> 594 25669.00       1     6 Shallow piscivorous fish    10
#> 595 25669.00       2     0 Shallow piscivorous fish    10
#> 596 25669.00       2     1 Shallow piscivorous fish    10
#> 597 25669.00       2     2 Shallow piscivorous fish    10
#> 598 25669.00       2     3 Shallow piscivorous fish    10
#> 599 25669.00       2     4 Shallow piscivorous fish    10
#> 600 25669.00       2     6 Shallow piscivorous fish    10
#> 601 25669.00       3     0 Shallow piscivorous fish    10
#> 602 25669.00       3     1 Shallow piscivorous fish    10
#> 603 25669.00       3     2 Shallow piscivorous fish    10
#> 604 25669.00       3     3 Shallow piscivorous fish    10
#> 605 25669.00       3     4 Shallow piscivorous fish    10
#> 606 25669.00       3     6 Shallow piscivorous fish    10
#> 607 25669.00       4     0 Shallow piscivorous fish    10
#> 608 25669.00       4     1 Shallow piscivorous fish    10
#> 609 25669.00       4     2 Shallow piscivorous fish    10
#> 610 25669.00       4     3 Shallow piscivorous fish    10
#> 611 25669.00       4     4 Shallow piscivorous fish    10
#> 612 25669.00       4     5 Shallow piscivorous fish    10
#> 613 25669.00       4     6 Shallow piscivorous fish    10
#> 614 25669.00       5     0 Shallow piscivorous fish    10
#> 615 25669.00       5     1 Shallow piscivorous fish    10
#> 616 25669.00       5     2 Shallow piscivorous fish    10
#> 617 25669.00       5     3 Shallow piscivorous fish    10
#> 618 25669.00       5     4 Shallow piscivorous fish    10
#> 619 25669.00       5     5 Shallow piscivorous fish    10
#> 620 25669.00       5     6 Shallow piscivorous fish    10
load_init_stanza(init = init, fgs = fgs, bboxes = bboxes)
#>    atoutput polygon layer    species agecl
#> 1        NA       1     0 Cephalopod     1
#> 2        NA       1     1 Cephalopod     1
#> 3        NA       1     2 Cephalopod     1
#> 4        NA       1     3 Cephalopod     1
#> 5        NA       1     6 Cephalopod     1
#> 6        NA       2     0 Cephalopod     1
#> 7        NA       2     1 Cephalopod     1
#> 8        NA       2     2 Cephalopod     1
#> 9        NA       2     3 Cephalopod     1
#> 10       NA       2     4 Cephalopod     1
#> 11       NA       2     6 Cephalopod     1
#> 12       NA       3     0 Cephalopod     1
#> 13       NA       3     1 Cephalopod     1
#> 14       NA       3     2 Cephalopod     1
#> 15       NA       3     3 Cephalopod     1
#> 16       NA       3     4 Cephalopod     1
#> 17       NA       3     6 Cephalopod     1
#> 18       NA       4     0 Cephalopod     1
#> 19       NA       4     1 Cephalopod     1
#> 20       NA       4     2 Cephalopod     1
#> 21       NA       4     3 Cephalopod     1
#> 22       NA       4     4 Cephalopod     1
#> 23       NA       4     5 Cephalopod     1
#> 24       NA       4     6 Cephalopod     1
#> 25       NA       5     0 Cephalopod     1
#> 26       NA       5     1 Cephalopod     1
#> 27       NA       5     2 Cephalopod     1
#> 28       NA       5     3 Cephalopod     1
#> 29       NA       5     4 Cephalopod     1
#> 30       NA       5     5 Cephalopod     1
#> 31       NA       5     6 Cephalopod     1
#> 32       NA       1     0 Cephalopod     2
#> 33       NA       1     1 Cephalopod     2
#> 34       NA       1     2 Cephalopod     2
#> 35       NA       1     3 Cephalopod     2
#> 36       NA       1     6 Cephalopod     2
#> 37       NA       2     0 Cephalopod     2
#> 38       NA       2     1 Cephalopod     2
#> 39       NA       2     2 Cephalopod     2
#> 40       NA       2     3 Cephalopod     2
#> 41       NA       2     4 Cephalopod     2
#> 42       NA       2     6 Cephalopod     2
#> 43       NA       3     0 Cephalopod     2
#> 44       NA       3     1 Cephalopod     2
#> 45       NA       3     2 Cephalopod     2
#> 46       NA       3     3 Cephalopod     2
#> 47       NA       3     4 Cephalopod     2
#> 48       NA       3     6 Cephalopod     2
#> 49       NA       4     0 Cephalopod     2
#> 50       NA       4     1 Cephalopod     2
#> 51       NA       4     2 Cephalopod     2
#> 52       NA       4     3 Cephalopod     2
#> 53       NA       4     4 Cephalopod     2
#> 54       NA       4     5 Cephalopod     2
#> 55       NA       4     6 Cephalopod     2
#> 56       NA       5     0 Cephalopod     2
#> 57       NA       5     1 Cephalopod     2
#> 58       NA       5     2 Cephalopod     2
#> 59       NA       5     3 Cephalopod     2
#> 60       NA       5     4 Cephalopod     2
#> 61       NA       5     5 Cephalopod     2
#> 62       NA       5     6 Cephalopod     2
load_init_weight(init = init, fgs = fgs, bboxes = bboxes)
#>                     species agecl       sn       rn
#> 1  Small planktivorous fish     1    0.028     0.25
#> 2  Small planktivorous fish     2    7.000    18.00
#> 3  Small planktivorous fish     3   25.500    68.00
#> 4  Small planktivorous fish     4   44.000   115.00
#> 5  Small planktivorous fish     5   56.000   150.00
#> 6  Small planktivorous fish     6   64.000   170.00
#> 7  Small planktivorous fish     7   69.000   182.00
#> 8  Small planktivorous fish     8   72.000   189.00
#> 9  Small planktivorous fish     9   73.000   193.00
#> 10 Small planktivorous fish    10   74.000   195.00
#> 11 Shallow piscivorous fish     1 2077.000  9837.00
#> 12 Shallow piscivorous fish     2 6759.000 20113.00
#> 13 Shallow piscivorous fish     3 8786.000 24083.00
#> 14 Shallow piscivorous fish     4 9425.000 25177.00
#> 15 Shallow piscivorous fish     5 9612.000 25472.00
#> 16 Shallow piscivorous fish     6 9666.000 25613.00
#> 17 Shallow piscivorous fish     7 9681.000 25653.00
#> 18 Shallow piscivorous fish     8 9685.000 25665.00
#> 19 Shallow piscivorous fish     9 9686.000 25668.00
#> 20 Shallow piscivorous fish    10 9687.000 25669.00
```
