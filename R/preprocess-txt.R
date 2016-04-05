#' Preprocess dataframes loaded in with \code{load_txt()}
#'
#' \code{sep_col} is split into multiple columns given by \code{into}.
#' If column ageclass is present and values start with 0 one is added
#' to align with agestructure in other functions. Columns without any informations
#'  (\code{length(unique()) == 1}) are droppped. If the first time step only
#'  has zeros as values remove these values. remove zeros overall!
#'
#' @param df_txt Dataframe read in with \code{load_txt()}.
#' @param sep_col Column to seperate into multiple columns.
#' @param into Character vector given the columns to split sep_col in.
#' @return Tidy dataframe.
#' @export
#'
#' @examples
#' d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
#' df <- load_txt(dir = d, file = "outputSETASSpecificPredMort.txt")
#' df <- preprocess_txt(df_txt = df, into = c("pred", "agecl", "empty_col1", "prey", "empty_col2"))
#' head(df)
#'
#' df <- load_txt(dir = d, file = "outputSETASSpecificMort.txt")
#' df <- preprocess_txt(df_txt = df, into = c("species", "agecl", "empty_col", "mort"))
#' head(df)


preprocess_txt <- function(df_txt, sep_col = "code", into) {
  df_txt <- tidyr::separate_(df_txt, col = sep_col, into = into, convert = TRUE)
  # df_txt$agecl <- df_txt$agecl + 1

  # check uniqueness of columns.
  cun <- sapply(df_txt, unique)

  # Get column with ageclassses and add 1 if necessary!
  id_agecl <- vapply(cun, function(x) all(is.element(x, 0:9)), FUN.VALUE = logical(1))
  if (sum(id_agecl) == 1) {
    df_txt[, id_agecl] <- df_txt[, id_agecl] + 1
  } else {
    if (sum(id_agecl) > 1) stop("Multiple ageclass columns found.")
  }

  # Remove columns without data!
  id_data <- vapply(cun, function(x) length(x) != 1, FUN.VALUE = logical(1))
  df_txt <- df_txt[, id_data]

  # Only zeros in first timestep? Remove values!
  if (all(df_txt$time[df_txt$time == 0] == 0)) {
    df_txt <- df_txt[df_txt$time != 0, ]
  }

  # Remove zeros
  df_txt <- df_txt[df_txt$atoutput != 0, ]

  return(df_txt)
}
