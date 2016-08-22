#' This function is used to check the individual growth per group over time.
#'
#' @param df
#' @return Output of lm fit
#' @export
#' @family check functions
#'
#' @examples

check_df_names(df, expect = c("species", "time", "agecl", "atoutput"))

df <- preprocess_setas$resn_age

ref <- df[df$time == min(df$time), ]
ref$time <- NULL
names(ref)[names(ref) == "atoutput"] <- "atoutput_ref"
result <- df %>%
  dplyr::left_join(ref) %>%
  dplyr::mutate_(atoutput = ~atoutput / atoutput_ref)
result$atoutput[result$atoutput_ref == 0] <- 0

dfs <- split(df, df$species)
dfs <- lapply(dfs, function(x) split(x, x$age))


fit_lm <- function(ls) {
  lm <- lm(formula = atoutput ~ time, data = result)

  str(lm)

  slope <- lm$coefficients[2]
  pvalue <- summary(lm)$coefficients[2, 4]

}
