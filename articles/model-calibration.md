# model-calibration

NOTE: This vigentte is optimised for longer simulation runs. Therefore
the output is not as pleasant due to the fact that the dummy setas file
have a running time of 5 years.

In order to use this vignette make sure to render `model-preprocess.Rmd`
first. Either save the resulting list of dataframes as shown in
`data-raw/data-vignette-model-preprocess.R` or render both vignettes
`model-preprocess.Rmd` and `model-calibration.Rmd` in the same
R-instance. Of course, you can also use a personalised version of
`mode-preprocess.Rmd`. Please make sure to add all resulting dataframes
to the list of dataframes at the end of the preprocess vignette and
change `model-calibration.Rmd` accordingly.

``` r

library("atlantistools")
library("ggplot2")
library("gridExtra")
library("magrittr")

fig_height2 <- 11
gen_labels <- list(x = "Time [years]", y = "Biomass [t]")

# You should be able to build the vignette either by clicking on "Knit PDF" in RStudio or with
# rmarkdown::render("model-calibration.Rmd")
```

### User Input

This section is used to read in the SETAS dummy files. Please change
this accordingly.

``` r

result <- preprocess

d <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")

# External recruitment data
ex_rec_ssb <- read.csv(file.path(d, "setas-ssb-rec.csv"), stringsAsFactors = FALSE)

# External biomass data
ex_bio <- read.csv(file.path(d, "setas-bench.csv"), stringsAsFactors = FALSE)

# bgm file
bgm <- file.path(d, "VMPA_setas.bgm")
```

## Whole system plots!

### Overall biomass

``` r

df_bio <- combine_groups(result$biomass, group_col = "species", combine_thresh = 10)
```

    ## Joining with `by = join_by(species)`

``` r

plot <- plot_bar(df_bio)
update_labels(plot, labels = gen_labels)
```

![](model-calibration_files/figure-html/unnamed-chunk-3-1.png)

### Biomass timeseries

``` r

plot <- plot_line(result$biomass)
update_labels(plot, labels = gen_labels)
```

![](model-calibration_files/figure-html/unnamed-chunk-4-1.png)

### <Biomass@age> timeseries

``` r

plot <- plot_line(result$biomass_age, col = "agecl")
update_labels(p = plot, labels = c(gen_labels, list(colour = "Ageclass")))
```

![](model-calibration_files/figure-html/unnamed-chunk-5-1.png)

### Number timeseries

``` r

plot <- plot_line(result$nums)
update_labels(p = plot, labels = list(x = "Time [years]", y = "Numbers"))
```

![](model-calibration_files/figure-html/unnamed-chunk-6-1.png)

### <Number@age> timeseries

``` r

plot <- plot_line(result$nums_age, col = "agecl")
update_labels(p = plot, labels = list(x = "Time [years]", y = "Numbers", colour = "Ageclass"))
```

![](model-calibration_files/figure-html/unnamed-chunk-7-1.png)

### SSB & Recruitment

``` r

plot_rec(result$ssb_rec, ex_data = ex_rec_ssb)
```

![](model-calibration_files/figure-html/unnamed-chunk-8-1.png)

### Biomass benchmark

``` r

names(ex_bio)[names(ex_bio) == "biomass"] <- "atoutput"

data <- result$biomass
data$model <- "atlantis"
comp <- rbind(ex_bio, data, stringsAsFactors = FALSE)

# Show atlantis as first factor!
comp$model <- factor(comp$model, levels = c("atlantis", sort(unique(comp$model))[sort(unique(comp$model)) != "atlantis"]))

# Create plot
plot <- plot_line(comp, col = "model")
update_labels(plot, gen_labels)
```

![](model-calibration_files/figure-html/unnamed-chunk-9-1.png)

### Biomass benchmark 2

``` r

plot <- plot_line(result$biomass) %>% update_labels(labels = gen_labels)
plot_add_range(plot, ex_bio)
```

![](model-calibration_files/figure-html/unnamed-chunk-10-1.png)

### Physics

``` r

plot <- plot_line(result$physics, wrap = NULL)
custom_grid(plot, grid_x = "polygon", grid_y = "variable")
```

![](model-calibration_files/figure-html/unnamed-chunk-11-1.png)

## Physics

``` r

physics <- result$physics %>%
  flip_layers() %>%
  split(., .$variable)

plots <- lapply(physics, plot_line, wrap = NULL) %>% 
  lapply(., custom_grid, grid_x = "polygon", grid_y = "layer")

for (i in seq_along(plots)) {
  cat(paste0("## ", names(plots)[i]), sep = "\n")
  plot <- update_labels(plots[[i]], labels = list(y = names(plots)[i]))
  print(plot)
  cat("\n\n")
}
```

### Chl_a

![](model-calibration_files/figure-html/unnamed-chunk-12-1.png)

### Denitrifiction

![](model-calibration_files/figure-html/unnamed-chunk-12-2.png)

### NH3

![](model-calibration_files/figure-html/unnamed-chunk-12-3.png)

### NO3

![](model-calibration_files/figure-html/unnamed-chunk-12-4.png)

### salt

![](model-calibration_files/figure-html/unnamed-chunk-12-5.png)

### Temp

![](model-calibration_files/figure-html/unnamed-chunk-12-6.png)

### Fluxes 1

``` r

plot <- flip_layers(result$flux) %>% 
  plot_line(wrap = NULL, col = "variable")
custom_grid(plot, grid_x = "polygon", grid_y = "layer")
```

![](model-calibration_files/figure-html/unnamed-chunk-13-1.png)

### Fluxes 2

``` r

plot <- flip_layers(result$sink) %>% 
  plot_line(wrap = NULL, col = "variable")
custom_grid(plot, grid_x = "polygon", grid_y = "layer")
```

![](model-calibration_files/figure-html/unnamed-chunk-14-1.png)

### Relative change of water column height compared to nominal_dz

``` r

check_dz <- result$dz %>% 
  dplyr::left_join(result$nominal_dz, by = c("polygon", "layer")) %>% 
  dplyr::mutate(check_dz = atoutput.x / atoutput.y) %>% 
  dplyr::filter(!is.na(check_dz)) # remove sediment layer

plot <- plot_line(check_dz, x = "time", y = "check_dz", wrap = "polygon", col = "layer")
update_labels(plot, list(x = "Time [years]", y = expression(dz/nominal_dz)))
```

![](model-calibration_files/figure-html/unnamed-chunk-15-1.png)

## Calibration plots

### Structural nitrogen

``` r

df_rel <- convert_relative_initial(result$structn_age)
plot <- plot_line(df_rel, col = "agecl")
plot <- update_labels(plot, list(x = "Time [years]", y = expression(SN/SN[init])))
plot_add_box(plot)
```

![](model-calibration_files/figure-html/unnamed-chunk-16-1.png)

### Reserve nitrogen

``` r

df_rel <- convert_relative_initial(result$resn_age)
plot <- plot_line(df_rel, col = "agecl")
plot <- update_labels(plot, list(x = "Time [years]", y = expression(RN/RN[init])))
plot_add_box(plot)
```

![](model-calibration_files/figure-html/unnamed-chunk-17-1.png)

### Biomass per ageclass

``` r

df_rel <- convert_relative_initial(result$biomass_age)
plot <- plot_line(df_rel, col = "agecl")
plot <- update_labels(plot, list(x = "Time [years]", y = expression(Biomass/Biomass[init])))
plot_add_box(plot)
```

![](model-calibration_files/figure-html/unnamed-chunk-18-1.png)

### Eat per ageclass

``` r

df_rel <- convert_relative_initial(result$eat_age)
plot <- plot_line(df_rel, col = "agecl")
plot <- update_labels(plot, list(x = "Time [years]", y = expression(Cons./Cons.[init])))
plot_add_box(plot)
```

![](model-calibration_files/figure-html/unnamed-chunk-19-1.png)

### Growth per ageclass

``` r

df_rel <- convert_relative_initial(result$growth_age)
plot <- plot_line(df_rel, col = "agecl")
plot <- update_labels(plot, list(x = "Time [years]", y = expression(Growth/Growth[init])))
plot_add_box(plot)
```

![](model-calibration_files/figure-html/unnamed-chunk-20-1.png)

### Growth in relation to initial conditions

``` r

plot <- plot_line(result$growth_rel_init, y = "gr_rel", col = "agecl")
update_labels(plot, list(y = expression((Growth - Growth[req])/Growth[req])))
```

![](model-calibration_files/figure-html/unnamed-chunk-21-1.png)

### Numbers

``` r

df_rel <- convert_relative_initial(result$nums_age)
plot <- plot_line(df_rel, col = "agecl")
plot <- update_labels(plot, list(x = "Time [years]", y = expression(Numbers/Numbers[init])))
plot_add_box(plot)
```

![](model-calibration_files/figure-html/unnamed-chunk-22-1.png)

### Biomass

``` r

df_rel <- convert_relative_initial(result$biomass)
plot <- plot_line(df_rel)
plot <- update_labels(plot, list(x = "Time [years]", y = expression(Biomass/Biomass[init])))
plot_add_box(plot)
```

![](model-calibration_files/figure-html/unnamed-chunk-23-1.png)

## Distribution plots

### Numbers @ age

``` r

df <- agg_perc(result$nums_age, groups = c("time", "species"))
plot <- plot_bar(df, fill = "agecl", wrap = "species")
update_labels(plot, labels = list(x = "Time [years]", y = "Numbers [%]"))
```

![](model-calibration_files/figure-html/unnamed-chunk-24-1.png)

### Biomass @ age

``` r

df <- agg_perc(result$biomass_age, groups = c("time", "species"))
plot <- plot_bar(df, fill = "agecl", wrap = "species")
update_labels(plot, labels = list(x = "Time [years]", y = "Biomass [%]"))
```

![](model-calibration_files/figure-html/unnamed-chunk-25-1.png)

## Diet Plots

    ## Joining with `by = join_by(time, pred, agecl, prey)`
    ## Joining with `by = join_by(time, pred, agecl, prey)`

### Diet plot 1: Cephalopod

![](model-calibration_files/figure-html/unnamed-chunk-26-1.png)

### Diet plot 2: Diatom

![](model-calibration_files/figure-html/unnamed-chunk-26-2.png)

### Diet plot 3: Labile detritus

![](model-calibration_files/figure-html/unnamed-chunk-26-3.png)

### Diet plot 4: Megazoobenthos

![](model-calibration_files/figure-html/unnamed-chunk-26-4.png)

### Diet plot 5: Refractory detritus

![](model-calibration_files/figure-html/unnamed-chunk-26-5.png)

### Diet plot 6: Shallow piscivorous fish

![](model-calibration_files/figure-html/unnamed-chunk-26-6.png)

### Diet plot 7: Small planktivorous fish

![](model-calibration_files/figure-html/unnamed-chunk-26-7.png)

## Spatial Plots 1

    ## Joining with `by = join_by(polygon)`
    ## Coordinate system already present. ℹ Adding new coordinate system, which will
    ## replace the existing one.
    ## Coordinate system already present. ℹ Adding new coordinate system, which will
    ## replace the existing one.
    ## Coordinate system already present. ℹ Adding new coordinate system, which will
    ## replace the existing one.
    ## Coordinate system already present. ℹ Adding new coordinate system, which will
    ## replace the existing one.
    ## Coordinate system already present. ℹ Adding new coordinate system, which will
    ## replace the existing one.
    ## Coordinate system already present. ℹ Adding new coordinate system, which will
    ## replace the existing one.
    ## Coordinate system already present. ℹ Adding new coordinate system, which will
    ## replace the existing one.
    ## Coordinate system already present. ℹ Adding new coordinate system, which will
    ## replace the existing one.
    ## Coordinate system already present. ℹ Adding new coordinate system, which will
    ## replace the existing one.
    ## Coordinate system already present. ℹ Adding new coordinate system, which will
    ## replace the existing one.

### Spatial Plot 1: Carrion3 1

![](model-calibration_files/figure-html/unnamed-chunk-27-1.png)

### Spatial Plot 2: Cephalopod 1

![](model-calibration_files/figure-html/unnamed-chunk-27-2.png)

### Spatial Plot 3: Diatom 1

![](model-calibration_files/figure-html/unnamed-chunk-27-3.png)

### Spatial Plot 4: Labile detritus 1

![](model-calibration_files/figure-html/unnamed-chunk-27-4.png)

### Spatial Plot 5: Megazoobenthos 1

![](model-calibration_files/figure-html/unnamed-chunk-27-5.png)

### Spatial Plot 6: Refractory detritus 1

![](model-calibration_files/figure-html/unnamed-chunk-27-6.png)

### Spatial Plot 7: Shallow piscivorous fish 1

![](model-calibration_files/figure-html/unnamed-chunk-27-7.png)

### Spatial Plot 8: Shallow piscivorous fish 2

![](model-calibration_files/figure-html/unnamed-chunk-27-8.png)

### Spatial Plot 9: Small planktivorous fish 1

![](model-calibration_files/figure-html/unnamed-chunk-27-9.png)

### Spatial Plot 10: Small planktivorous fish 2

![](model-calibration_files/figure-html/unnamed-chunk-27-10.png)

## Spatial Plots 2

    ## Joining with `by = join_by(time, polygon)`
    ## `geom_line()`: Each group consists of only one observation. ℹ Do you need to
    ## adjust the group aesthetic?
    ## `geom_line()`: Each group consists of only one observation. ℹ Do you need to
    ## adjust the group aesthetic?

### Spatial Plot 1: Carrion3

![](model-calibration_files/figure-html/unnamed-chunk-28-1.png)

### Spatial Plot 2: Cephalopod

![](model-calibration_files/figure-html/unnamed-chunk-28-2.png)

### Spatial Plot 3: Diatom

![](model-calibration_files/figure-html/unnamed-chunk-28-3.png)

### Spatial Plot 4: Labile detritus

![](model-calibration_files/figure-html/unnamed-chunk-28-4.png)

### Spatial Plot 5: Megazoobenthos

![](model-calibration_files/figure-html/unnamed-chunk-28-5.png)

### Spatial Plot 6: Refractory detritus

![](model-calibration_files/figure-html/unnamed-chunk-28-6.png)

### Spatial Plot 7: Shallow piscivorous fish

![](model-calibration_files/figure-html/unnamed-chunk-28-7.png)

### Spatial Plot 8: Small planktivorous fish

![](model-calibration_files/figure-html/unnamed-chunk-28-8.png)
