#' Scan character vector for specific flag!
#'
#' @param chars Vector of character strings
#' @param variable Character string giving the flag to search for.
#' @export

# Extract position of variable in a Vector of character strings.
scan_prm <- function(chars, variable){
  pos <- grep(pattern = variable, x = chars)
  if (length(pos) == 0) {
    stop(paste("Variable", variable, "not found."))
  } else {
    if (length(pos) >= 1) {
      # Check if some lines are outcommented and remove those!
      # Some modelers tend to add multiple lines for each flag
      # and comment the ones they do not use out... ;)
      pos_com <- which(substr(chars[pos], 1, 1) == "#")
      if (length(pos_com) == length(pos)) {
        stop(paste("Variable", variable, "always outcommented."))
      } else {
        if (length(pos_com) >= 1) pos <- pos[-pos_com]
      }
    }
    # Final check for uniqueness of flag!
    if (length(pos) == 1) {
      return(pos)
    } else {
      # mL and mQ are also found in jmL and jmQ... We need to add an exception
      # here! The order of the exceptions have to match the order of the
      # second item found (e.g. "mL_" --> "jmL_")
      if (length(pos) == 2 & any(sapply(c("mL_", "mQ_", "mum_"), grepl, x = variable))) {
        # Remove juveniale mortality
        ex <- c("jmL_", "jmQ_", "crit_mum")
        for (i in seq_along(ex)) {
          if (any(grepl(pattern = ex[i], x = chars[pos]))) pos <- pos[!grepl(pattern = ex[i], x = chars[pos])]
        }
        return(pos)
      } else {
        stop(paste("Variable", variable, "found multiple times."))
      }
    }
  }
}

#' @export
#' @rdname scan_prm
# Extract value for a specific parameter from a Vector of character strings.
extract_prm <- function(chars, variable){
  pos <- scan_prm(chars = chars, variable = variable)
  result <- chars[pos]
  result <- str_split_twice(char = result)
  return(result)
}
