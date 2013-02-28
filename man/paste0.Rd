\name{paste0}
\alias{paste0}
\title{Concatenate Strings}
\usage{
  paste0(..., collapse = NULL)
}
\arguments{
  \item{collapse}{an optional character string to separate
  the results.  Not \code{\link{NA_character_}}.}
}
\description{
  concatenate vectors after converting to character.
}
\section{Details}{
  paste converts its arguments (via as.character) to
  character strings, and concatenates them (separating them
  by the string given by sep). If the arguments are
  vectors, they are concatenated term-by-term to give a
  character vector result. Vector arguments are recycled as
  needed, with zero-length arguments being recycled to "".

  Note that paste() coerces NA_character_, the character
  missing value, to "NA" which may seem undesirable, e.g.,
  when pasting two character vectors, or very desirable,
  e.g. in paste("the value of p is ", p).

  paste0(..., collapse) is equivalent to paste(..., sep =
  "", collapse), slightly more efficiently.

  If a value is specified for collapse, the values in the
  result are then concatenated into a single string, with
  the elements being separated by the value of collapse.
}

\section{Value}{
  A character vector of the concatenated values. This will
  be of length zero if all the objects are, unless collapse
  is non-NULL in which case it is a single empty string.

  If any input into an element of the result is in UTF-8
  (and none are declared with encoding "bytes"), that
  element will be in UTF-8, otherwise in the current
  encoding in which case the encoding of the element is
  declared if the current locale is either Latin-1 or
  UTF-8, at least one of the corresponding inputs
  (including separators) had a declared encoding and all
  inputs were either ASCII or declared.

  If an input into an element is declared with encoding
  "bytes", no translation will be done of any of the
  elements and the resulting element will have encoding
  "bytes". If collapse is non-NULL, this applies also to
  the second, collapsing, phase, but some translation may
  have been done in pasting object together in the first
  phase.
}
\examples{
stopifnot(identical(paste ("A", 1:6, sep = ""), paste0("A", 1:6)))
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  The New S Language. Wadsworth & Brooks/Cole.
}
\seealso{
  String manipulation with
  \code{\link[base]{as.character}},
  \code{\link[base]{substr}}, \code{\link[base]{nchar}},
  \code{\link[base]{strsplit}}; further,
  \code{\link[base]{cat}} which concatenates and writes to
  a file, and \code{\link[base]{sprintf}} for c linke
  string construction.

  `\code{\link[grDevices]{plotmath}}' for the use of paste
  in plot annotation.
}

