### Roxygen-inzable documentation
#' 
#' STUART: Subtests Using ACO Rummaging Techniques
#' 
#' The STUART-Package automates the generation of subtests from
#' a given set of items within the confines of confirmatory factor analysis.
#' 
#' @section Functionality:
#' 
#' Using this package subtests can be generated in two different ways: using a pseudo-random approach rooted in Ant-Colony-Optimization via the \code{\link{mmas}}-function or using a brute-force approach via the aptly named \code{\link{bruteforce}}-function.
#' 
#' Addtionally, there are some convenience functions which are more or less useful. The \code{\link{combinations}}-function can be used to determine the number of possible subtests to inform a decision on which selection approach to use. The \code{\link{crossvalidate}}-function can be used to evaluate the quality of a selection in a different (sub-)sample. The \code{\link{heuristics}}-function can be used to extract the formatting of heurstic matrices which can be provided to the \code{\link{mmas}}-function.
#' 
#' The package also provides two datasets: the \code{\link{sups}} dataset and the \code{\link{fairplayer}} dataset.
#' 
#' @docType package
"_PACKAGE"
#> [1] "_PACKAGE"