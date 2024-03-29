% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocess-txt.R
\name{preprocess_txt}
\alias{preprocess_txt}
\title{Preprocess dataframes loaded in with \code{load_txt()}}
\usage{
preprocess_txt(df_txt, sep_col = "code", into, removeZeros = TRUE)
}
\arguments{
\item{df_txt}{Dataframe read in with \code{load_txt()}.}

\item{sep_col}{Column to separate into multiple columns. Default is \code{"code"}.}

\item{into}{Character vector given the columns to split sep_col in.}

\item{removeZeros}{Boolean. Remove all zeros. (Default = T)}
}
\value{
Tidy dataframe.
}
\description{
\code{sep_col} is split into multiple columns given by \code{into}.
If column ageclass is present and values start with 0 one is added
to align with agestructure in other functions. Columns without any informations
 (\code{length(unique()) == 1}) are dropped. If the first time step only
 has zeros as values remove these values. remove zeros overall!
}
\examples{
d <- system.file("extdata", "setas-model-new-becdev", package = "atlantistools")
df <- load_txt(file = file.path(d, "outputSETASSpecificPredMort.txt"))
df <- preprocess_txt(df_txt = df, into = c("pred", "agecl", "empty_col1", "prey", "empty_col2"))
head(df)

df <- load_txt(file = file.path(d, "outputSETASSpecificMort.txt"))
df <- preprocess_txt(df_txt = df, into = c("species", "agecl", "empty_col", "mort"))
head(df)
}
