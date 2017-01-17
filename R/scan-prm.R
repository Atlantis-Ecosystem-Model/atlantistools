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
      # check if the variable is part of another variable and exclude those!
      # We check for equality of the first two characters!
      check <- substr(chars[pos], start = 1, stop = 2) %in% substr(variable, start = 1, stop = 2)
      if (sum(check) == 1) { # apply the fix
        pos <- pos[check]
        return(pos)
      } else {
        stop(paste("Variable", variable, "found multiple times."))
      }
    }
  }
}

