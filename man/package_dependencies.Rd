\name{package_dependencies}
\alias{package_dependencies}
\title{Computations on the Dependency Hierarchy of Packages}
\usage{
  package_dependencies(packages = NULL, db,
    which = c("Depends", "Imports", "LinkingTo"),
    recursive = FALSE, reverse = FALSE)
}
\arguments{
  \item{packages}{a character vector of package names.}

  \item{db}{character matrix as from available.packages(),
  or data frame variants thereof.  Alternatively, a package
  database like the one available from
  http://cran.R-project.org/web/packages/packages.rds}

  \item{which}{a character vector listing the types of
  dependencies, a subset of c("Depends", "Imports",
  "LinkingTo", "Suggests", "Enhances"). Character string
  "all" is shorthand for that vector, character string
  "most" for the same vector without "Enhances".}

  \item{recursive}{logical: should (reverse) dependencies
  of (reverse) dependencies (and so on) be included?}

  \item{reverse}{logical: if FALSE (default), regular
  dependencies are calculated, otherwise reverse
  dependencies.}
}
\description{
  Find (recursively) dependencies or reverse dependencies
  of packages.
}
\section{Value}{
  Named list with one element for each package in argument
  packages, each consists of a character vector naming the
  (recursive) (reverse) dependencies of that package.

  For given packages which are not found in the db, NULL
  entries are returned, as opposed to character(0) entries
  which indicate no dependencies.
}
\examples{
\dontrun{
pdb <- available.packages()
deps <- package_dependencies(packages = "MASS", pdb,
                            which = c("Depends", "Imports", "LinkingTo", "Suggests"),
                            recursive = TRUE, reverse = TRUE)
length(deps$MASS)
}
}
\seealso{
  dependsOnPkgs, and package.dependencies for checking
  dependencies
}

