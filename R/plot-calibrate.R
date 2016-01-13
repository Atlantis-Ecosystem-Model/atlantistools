dir <- system.file("extdata", "setas-model-new-trunk", package = "atlantistools")
bps <- load_bps(dir = dir, fgs = "functionalGroups.csv", init = "INIT_VMPA_Jan2015.nc")
bboxes <- get_boundary(boxinfo = load_box(dir = dir, bgm = "VMPA_setas.bgm"))
select_groups <- c("Planktiv_S_Fish", "Cephalopod", "Diatom")
select_variable <- "ResN"
modelstart <- "1991-01-01"
prm_run <- "VMPA_setas_run_fishing_F_Trunk.prm"
fgs <- "functionalGroups.csv"
nc <- "outputSETAS.nc"
check_acronyms <- TRUE

test <- load_nc(dir = dir, nc = "outputSETAS.nc",
  bps = bps,
  fgs = "functionalGroups.csv",
  select_groups = c("Planktiv_S_Fish", "Cephalopod", "Diatom"),
  select_variable = "ResN",
  bboxes = bboxes,
  check_acronyms = TRUE)
str(test)


plot_calibrate <- function(dir,
                           nc,
                           bps,
                           fgs,
                           select_groups,
                           select_variable,
                           bboxes,
                           check_acronyms,
                           modelstart,
                           prm_run) {
  data <- load_nc(dir = dir,
                  nc = nc,
                  bps = bps,
                  fgs = fgs,
                  select_groups = select_groups,
                  select_variable = select_variable,
                  bboxes = bboxes,
                  check_acronyms = check_acronyms)


  # Aggregate data over layers and polygons!
  if (select_variable == "Nums") {
    data <- agg_sum(data = data, vars = c("species", "time", "agecl"))
  } else {
    data <- agg_mean(data = data, vars = c("species", "time", "agecl"))
  }

  # Convert time given as integer to actual time! This is done as
  # as possible to simplify coding/execution-time.
  data <- convert_time(dir = dir, prm_run = prm_run, data = data, modelstart = modelstart)

  # Divide values by reference value (time = min(time))
  min_time <- min(data$time)
  ref <- data %>%
    dplyr::filter(time == min_time)
  ref$time <- NULL
  names(ref)[names(ref) == "atoutput"] <- "atoutput_ref"
  result <- data %>%
    dplyr::left_join(ref) %>%
    dplyr::mutate(atoutput = atoutput / atoutput_ref) %>%
    dplyr::mutate(agecl = factor(agecl))
  result$atoutput[result$atoutput_ref == 0] <- 0
  # Strangely ordering gets lost due to standardisation. This was because of the use of left_join.
  #   result <- order_data(data = result)

  anno <- c(min(data$time), max(data$time))

  plot <- ggplot2::ggplot(data = result, ggplot2::aes_string(x = "time", y = "atoutput", colour = "agecl")) +
    ggplot2::annotate("rect", xmin = anno[1], xmax = anno[2], ymin = 0.5, ymax = 1.5, alpha = 0.1) +
    ggplot2::annotate("rect", xmin = anno[1], xmax = anno[2], ymin = 0.8, ymax = 1.2, alpha = 0.3) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 1, linetype = "dotted") +
    ggplot2::facet_wrap(~species, scales = "free_y", ncol = 8)

    theme_standard(scale_font = 0.8)

}




