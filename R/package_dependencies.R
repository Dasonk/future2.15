#' Computations on the Dependency Hierarchy of Packages
#' 
#' Find (recursively) dependencies or reverse dependencies of 
#' packages.
#' 
#' @param packages a character vector of package names.
#' @param db character matrix as from available.packages(), 
#' or data frame variants thereof.  Alternatively, a 
#' package database like the one available from 
#' http://cran.R-project.org/web/packages/packages.rds
#' @param which a character vector listing the types 
#' of dependencies, a subset of c("Depends", "Imports", 
#' "LinkingTo", "Suggests", "Enhances"). Character string 
#' "all" is shorthand for that vector, character string "most" 
#' for the same vector without "Enhances".
#' @param recursive logical: should (reverse) 
#' dependencies of (reverse) dependencies (and so on) 
#' be included?
#' @param reverse logical: if FALSE (default), 
#' regular dependencies are calculated, otherwise 
#' reverse dependencies.
#' 
#' @section Value:
#' Named list with one element for each package in argument
#' packages, each consists of a character vector naming the
#' (recursive) (reverse) dependencies of that package.
#'
#' For given packages which are not found in the db, NULL entries
#' are returned, as opposed to character(0) entries which indicate
#' no dependencies.
#' 
#' @seealso dependsOnPkgs, and package.dependencies for checking 
#' dependencies
#' 
#' @examples
#' \dontrun{
#' pdb <- available.packages()
#' deps <- package_dependencies(packages = "MASS", pdb,
#'                             which = c("Depends", "Imports", "LinkingTo", "Suggests"),
#'                             recursive = TRUE, reverse = TRUE)
#' length(deps$MASS)
#' }
#' 
#'  @export 
package_dependencies <- function (packages = NULL, 
                                  db, 
                                  which = c("Depends", "Imports", "LinkingTo"), 
                                  recursive = FALSE, 
                                  reverse = FALSE) 
{
    out_of_db_packages <- character()
    if (!recursive && !reverse) {
        if (!is.null(packages)) {
            ind <- match(packages, db[, "Package"], nomatch = 0L)
            db <- db[ind, , drop = FALSE]
            out_of_db_packages <- packages[ind == 0L]
        }
    }
    if (identical(which, "all")) 
        which <- c("Depends", "Imports", "LinkingTo", "Suggests", 
                   "Enhances")
    else if (identical(which, "most")) 
        which <- c("Depends", "Imports", "LinkingTo", "Suggests")
    depends <- do.call(Map, c(list("c"), lapply(which, function(f) {
        if (is.list(d <- db[, f])) d else lapply(d, .extract_dependency_package_names)
    }), list(USE.NAMES = FALSE)))
    depends <- lapply(depends, unique)
    if (!recursive && !reverse) {
        names(depends) <- db[, "Package"]
        if (length(out_of_db_packages)) {
            depends <- c(depends, structure(vector("list", length(out_of_db_packages)), 
                                            names = out_of_db_packages))
        }
        return(depends)
    }
    all_packages <- sort(unique(c(db[, "Package"], unlist(depends))))
    if (!recursive) {
        depends <- split(rep.int(db[, "Package"], sapply(depends, 
                                                         length)), factor(unlist(depends), levels = all_packages))
        if (!is.null(packages)) {
            depends <- depends[match(packages, names(depends))]
            names(depends) <- packages
        }
        return(depends)
    }
    tab <- if (reverse) 
        split(match(rep.int(db[, "Package"], sapply(depends, 
                                                    length)), all_packages), factor(match(unlist(depends), 
                                                                                          all_packages), levels = seq_along(all_packages)))
    else split(match(unlist(depends), all_packages), factor(match(rep.int(db[, 
                                                                             "Package"], sapply(depends, length)), all_packages), 
                                                            levels = seq_along(all_packages)))
    if (is.null(packages)) {
        if (reverse) {
            packages <- all_packages
            p_L <- seq_along(all_packages)
        }
        else {
            packages <- db[, "Package"]
            p_L <- match(packages, all_packages)
        }
    }
    else {
        p_L <- match(packages, all_packages, nomatch = 0L)
        if (any(ind <- (p_L == 0L))) {
            out_of_db_packages <- packages[ind]
            packages <- packages[!ind]
            p_L <- p_L[!ind]
        }
    }
    p_R <- tab[p_L]
    pos <- cbind(rep.int(p_L, sapply(p_R, length)), unlist(p_R))
    ctr <- 1L
    verbose <- getOption("verbose")
    repeat {
        if (verbose) 
            cat("Cycle:", ctr)
        p_L <- split(pos[, 1L], pos[, 2L])
        new <- do.call(rbind, Map(function(i, k) cbind(rep.int(i, 
                                                               length(k)), rep(k, each = length(i))), p_L, tab[as.integer(names(p_L))]))
        npos <- unique(rbind(pos, new))
        nnew <- nrow(npos) - nrow(pos)
        if (verbose) 
            cat(" NNew:", nnew, "\n")
        if (!nnew) 
            break
        pos <- npos
        ctr <- ctr + 1L
    }
    depends <- split(all_packages[pos[, 2L]], factor(all_packages[pos[, 
                                                                      1L]], levels = packages))
    if (length(out_of_db_packages)) {
        depends <- c(depends, structure(vector("list", length(out_of_db_packages)), 
                                        names = out_of_db_packages))
    }
    depends
}

.extract_dependency_package_names <- function(x){ 
    if (is.na(x)) 
        return(character())
    x <- unlist(strsplit(x, ",[[:space:]]*"))
    x <- sub("[[:space:]]*([[:alnum:].]+).*", "\\1", x)
    x[nzchar(x) & (x != "R")]
}
