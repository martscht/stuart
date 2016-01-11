### Roxygen-izable Documentation ----
#' Subtest construction using a brute-force approach
#' 
#' Construct subtests from a given pool of items using a brute-force approach (i.e. by estimating all possible combinations).
#' 
#' @author Martin Schultze
#' 
#' @seealso \code{\link{mmas}} \code{\link{combinations}}
#' 
#' @concept ACO subtests
#' 
#' 
### Inputs ----
#' @param data A data.frame containing all relevant data.
#' @param factor.structure  A list linking factors to items. The names of the list elements correspond to the factor names. Each list element must contain a character-vector of item names that are indicators of this factor.
#' @param items.per.subtest A list containing the number of items per subtest. This must be in the same order as the \code{factor.structure} provided. If a single number, it is applied to all subtests. If \code{NULL} all items are evenly distributed among the subtests.
#' @param number.of.subtests  A vector containing the number of subtests per construct. This must be in the same order as the \code{factor.structure} provided. If a single number, it is applied to all constructs. The default is to construct 1 subtest per construct.
#' @param invariance A character vector of length 1 or the same length as \code{factor.structure} containing the desired invariance levels between subtests pertaining to the same construct. Currently there are five options: 'congeneric', 'ess.equivalent', 'ess.parallel', 'equivalent', and 'parallel', the last being the default.
#' @param item.invariance A character vector of length 1 or the same length as \code{factor.structure} containing the desired invariance levels between items pertaining to the same subtest. Currently there are five options: 'congeneric', 'ess.equivalent', 'ess.parallel', 'equivalent', and 'parallel', the first being the default.
#' @param repeated.measures A list linking factors that are repeated measures of each other. Repeated factors must be in one element of the list - other sets of factors in other elements of the list. When this is \code{NULL} (the default) a cross-sectional model is estimated.
#' @param long.invariance A character vector of length 1 or the same length as \code{repeated.measures} containing the longitudinal invariance level of repeated subtests. Currently there are four options: 'congeneric', 'weak', 'strong', and 'strict'. Defaults to 'strict'. When \code{repeated.measures=NULL} this argument is ignored.
#' @param item.long.invariance A character vector of length 1 or the same length as \code{repeated.measures} containing the longitudinal invariance level of repeated items. Currently there are four options: 'congeneric', 'weak', 'strong', and 'strict'. Defaults to 'strict'. When \code{repeated.measures=NULL} this argument is ignored.
#' @param grouping The name of the grouping variable. The grouping variable must be part of \code{data} provided and must be a numeric variable.
#' @param group.invariance A single value describing the assumed invariance of subtests across groups. Currently there are four options: 'congeneric', 'weak', 'strong', and 'strict'. Defaults to 'strict'. When \code{grouping=NULL} this argument is ignored.
#' @param item.group.invariance A single value describing the assumed invariance of items across groups. Currently there are four options: 'congeneric', 'weak', 'strong', and 'strict'. Defaults to 'strict'. When \code{grouping=NULL} this argument is ignored.
#' @param auxiliary The names of auxiliary variables in \code{data}. These can be used in additional modeling steps that may be provided in \code{analysis.options$model}.
#' @param software The name of the estimation software. Can currently be 'lavaan' (the default), 'Mplus', or 'Mplus Demo'. Each option requires the software to be installed.
#' @param cores The number of cores to be used in parallel processing. If \code{NULL} (the default) the result of \code{\link[parallel]{detectCores}} will be used. On Unix-y machines parallel processing is implemented via \code{\link[parallel]{mclapply}}, on Windows machines it is realized via \code{\link[parallel]{parLapply}}.
#' @param fitness.func A function that converts the results of model estimation into a pheromone. If none is provided the default function \code{fitness} is used. This can be examined with \code{body(fitness)}.
#' @param ignore.errors A logical indicating whether or not to ignore estimation problems (such as non positive-definite latent covariance matrices). Defaults to \code{FALSE}.
#' @param analysis.options A list additional arguments to be passed to the estimation software. The names of list elements must correspond to the arguments changed in the respective estimation software. E.g. \code{analysis.options$model} can contain additional modeling commands - such as regressions on auxiliary variables.
#' @param suppress.model A logical indicating whether to suppress the default model generation. If \code{TRUE} a model must be provided in \code{analysis.options$model}.
#' @param request.override The maximum number of combinations for which the estimation is performed immediately, without an additional override request.
#' @param filename The stem of the filenames used to save inputs, outputs, and data files when \code{software='Mplus'}. Dafaults to "stuart".
#' 
#' 
### Outputs ---- 
#' @return Returns an object of the class \code{stuartOutput} for which specific \code{summary} and \code{plot} methods are available. The results are a list.
#' \item{call }{The called function.}
#' \item{software}{The software used to fit the CFA models.}
#' \item{parameters}{A list of the ACO parameters used.}
#' \item{timer}{An object of the class \code{proc_time} which contains the time used for the analysis.}
#' \item{log}{A \code{data.frame} containing the estimation history.}
#' \item{solution}{\code{NULL}}
#' \item{pheromones}{\code{NULL}}
#' \item{subtests}{A list containing the names of the selected items and their respective subtests.}
#' \item{final}{The results of the estimation of the global-best solution.}
#' 
#' 
### Examples ----
#' @examples
#' # Obtaining a 4-item short-version with strict
#' # longitudinal invariance
#' data(fairplayer)
#' 
#' fs <- list(SI1=names(fairplayer)[2:11],
#'   SI2=names(fairplayer[12:21]))
#' 
#' repme <- list(SI=c('SI1','SI2'))
#' \donttest{
#' longitudinal <- bruteforce(fairplayer, fs, 1, 4, repeated.measures=repme)
#' summary(longitudinal)
#' }
#' @export


### Function definition ----
bruteforce <-
function(
  data, factor.structure, items.per.subtest=NULL, number.of.subtests=1, #subtest settings

  invariance='parallel', item.invariance='congeneric',                  #cross invariance

  repeated.measures=NULL, long.invariance='strict', item.long.invariance='strict', #long structure

  mtmm=NULL, mtmm.invariance='congeneric', item.mtmm.invariance='congeneric', #MTMM structure
  
  grouping=NULL, group.invariance='strict', item.group.invariance='strict', #grouping structure

  auxiliary=NULL,

  software='lavaan', cores=NULL,                                        #run settings

  fitness.func=fitness, ignore.errors=FALSE,                            #fitness specs
  
  analysis.options=NULL, suppress.model=FALSE,                          #modeling specs

  request.override=10000,
  
  filename='bruteforce',
  
  ...
) {#function begin

  #sanity checks
  if (any(duplicated(names(factor.structure)))) {
    stop('You have provided duplicates in the name of factor.structure.')
  }
  
  timer <- proc.time()

  #combine arguments
  args <- as.list(match.call())[-1]
  args <- c(args,formals()[!names(formals())%in%c(names(args),'...')])

  #check for software
  args$cores <- software.check(software,cores)

  #data preparation
  prepared <- do.call('data.prep',args)

  #Feedbacking number of combinations
  combs <- do.call('compute.combinations', prepared[names(prepared)%in%names(formals(compute.combinations))])

  message(paste('There are',combs,'combinations that need to be tested.'))

  #ask for override if the estimation is not feasible
  if (combs>request.override) {
    override <- NA
    while(is.na(override)) {
      cat('\nWarning: Due to the number of possible combinations estimation may take very long or not be possible at all. Do you wish to continue?\n
 Enter y to continue or n to abort.\t'
      )
      override <- scan('',what='character',nmax=1,quiet=TRUE)
    }
    
    if (override!='y' & override!='Y') {
      stop('You aborted the estimation process.\n',call.=FALSE)
    }
  }

  #combine arguments
  args <- c(prepared,args[!names(args)%in%names(prepared)])

  solution <- do.call('stuart.bruteforce',args)

  args$data <- data
  args$output.model <- TRUE
  args$selected.items <- solution$selected.items
  args$selected <- solution$selected.gb

  tmp <- formals(paste('run',software,sep='.'))
  args <- args[names(args)%in%names(tmp)]
  args <- c(args,tmp[!names(tmp)%in%c(names(args))])

  final.model <- do.call(paste('run',software,sep='.'),args)

  #generating output
  output <- list(call=match.call()[1])  
  output$software <- software
  output$parameters <- c(solution$parameters)
  output$timer <- proc.time() - timer
  output$log <- solution$log
  output$solution <- NULL
  output$pheromones <- NULL
  output$subtests <- solution$selected.items
  output$final <- final.model

  class(output) <- 'stuartOutput'
  return(output)

}
