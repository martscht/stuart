### Roxygen-izable Documentation ----
#' Compute the number of possible subtest combinations
#' 
#' Used to compute the number of possible subtest constellations prior to running either \code{\link{mmas}} or \code{\link{bruteforce}}.
#' 
#' 
#' @author Martin Schultze
#' 
#' @seealso \code{\link{mmas}} \code{\link{bruteforce}}
#' 
#' @concept ACO subtests
#' 
### Inputs ----
#' @param data A data.frame containing all relevant data.
#' @param number.of.subtests  A vector containing the number of subtests per construct. This must be in the same order as the \code{factor.structure} provided. If a single number, it is applied to all constructs. The default is to construct 1 subtest per construct.
#' @param factor.structure  A list linking factors to items. The names of the list elements correspond to the factor names. Each list element must contain a character-vector of item names that are indicators of this factor.
#' @param items.per.subtest A list containing the number of items per subtest. This must be in the same order as the \code{factor.structure} provided. If a single number, it is applied to all subtests. If \code{NULL} all items are evenly distributed among the subtests.
#' @param repeated.measures A list linking factors that are repeated measures of each other. Repeated factors must be in one element of the list - other sets of factors in other elements of the list. When this is \code{NULL} (the default) a cross-sectional model is estimated.
#' @param mtmm A list linking factors that are measurements of the same construct with different methods. Measurements of the same construct must be in one element of the list - other sets of methods in other elements of the list. When this is \code{NULL} (the default) a single method model is estimated.
#' @param use.order A logical indicating whether or not to take the selection order of the items into account. Defaults to \code{FALSE}.
#' 
### Outputs ---- 
#' @return Returns the number of possible subtest constellations.
#' 
### Examples ----
#' @export


### Function definition ----
combinations <-
function(
  data, factor.structure, items.per.subtest=NULL, number.of.subtests=1, #subtest settings
  repeated.measures=NULL, mtmm=NULL, use.order=FALSE
) {#function begin

  #arguments
  args <- as.list(match.call())[-1]
  args <- c(args,formals()[!names(formals())%in%c(names(args),'...')])
  args <- c(args,formals(data.prep)[!names(formals(data.prep))%in%c(names(args),'...')])
  
  #sanity check
  fitness.func <- NULL
  do.call('sanitycheck',mget(names(formals(sanitycheck))))
  
  #data preparation
  prepared <- do.call('data.prep',args)

  combs <- do.call('compute.combinations', c(prepared[names(prepared)%in%names(formals(compute.combinations))],use.order))

  return(combs)
}
