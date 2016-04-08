#' This function is used to combine model output from different simulations!
#'
#' @param outs List of preprocessed Atlantis simulations. Each entry is a list generated with
#' `preprocess()`.
#' @param runs Vector of character strings giving the name of each simulation settings.
#' @return Named list with the the same format as in `preprocess()`. Each dataframe has
#' an additional column run.
#' @export
#' @family combine functions
#'
#' @examples
#' outs <- list(preprocess_setas, preprocess_setas)
#' runs <- c("run1", "run2")
#' test <- combine_runs(outs, runs)
#' names(test[[1]])
#' head(test[[1]])

combine_runs <- function(outs, runs) {
  if (length(outs) != length(runs)) {
    stop("Number of outs and runs does not match.")
  }

  add_run <- function(out, run) {
    for (i in seq_along(out)) {
      out[[i]]$run <- run
    }
    return(out)
  }

  # Add the appropriate runname to the dataframes!
  outs_run <- Map(add_run, outs, runs)

  # Combine the dataframes from different runs
  dim_outs <- unique(sapply(outs_run, length))
  if (length(dim_outs) != 1) stop("Different number of dataframes in *.Rda files.")
  result <- list()
  for (i in 1:dim_outs) {
    # Combine the i-th dataframe from each upper level list entry!
    result[[i]] <- do.call(rbind, lapply(outs_run, function(x) x[[i]]))
  }

  names(result) <- names(outs_run[[1]])
  return(result)
}


