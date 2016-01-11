### Roxygen-izable Documentation ----
#' Subtest construction using the Max-Min-Ant-System
#' 
#' Construct subtests from a given pool of items using the classical Max-Min Ant-System (Stützle, 1998). Allows for multiple constructs, occasions, and groups.
#' 
#' @author Martin Schultze
#' 
#' @seealso \code{\link{bruteforce}} \code{\link{heuristics}}
#' 
#' @concept ACO subtests
#' 
#' @references Stützle, T. (1998). Local search algorithms for combinatorial problems: Analysis, improvements, and new applications. Unpublished doctoral dissertation. Darmstadt: Fachbereich Informatik, Universität Darmstadt.
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
#' @param software The name of the estimation software. Can currently be 'lavaan' (the default) or 'Mplus'. Each option requires the software to be installed.
#' @param cores The number of cores to be used in parallel processing. If \code{NULL} (the default) the result of \code{\link[parallel]{detectCores}} will be used. On Unix-y machines parallel processing is implemented via \code{\link[parallel]{mclapply}}, on Windows machines it is realized via \code{\link[parallel]{parLapply}}.
#' @param fitness.func A function that converts the results of model estimation into a pheromone. If none is provided the default function \code{fitness} is used. This can be examined with \code{body(fitness)}.
#' @param ignore.errors A logical indicating whether or not to ignore estimation problems (such as non positive-definite latent covariance matrices). Defaults to \code{FALSE}.
#' @param ants The number of ants per colony to be estimated.
#' @param colonies The maximum number of colonies estimated since finding the latest global-best solution before aborting the process.
#' @param evaporation The evaporation coefficient. 
#' @param alpha The nonlinearity coefficient of the pheromone-trail's contribution to determining selection probabilities. Defaults to 1 (linear).
#' @param beta The nonlinearity coefficient of the heuristics' contribution to determining selection probabilities. Defaults to 1 (linear).
#' @param heuristics An object of the class \code{stuartHeuristic} as provided by \code{\link{heuristics}} which contains heuristic information to be used in determining selection probabilities. If \code{NULL} (the default) selection probabilities are determined solely by the pheromones.
#' @param deposit Which deposit rule to use. Can be either 'ib' (the default) for an iteration-best deposit rule, or 'gb' for a global-best deposit rule.
#' @param deposit.on Which parameterization to use when depositing pheromones. Can be either 'nodes' (the default) for depositing pheromones on selected nodes or 'arcs' for depositing on selection arcs.
#' @param pbest The desired overall probability of constructing the global-best solution when the algorithm converges. 
#' @param tolerance The tolerance of imprecision when comparing the pheromones to the upper and lower limits.
#' @param analysis.options A list additional arguments to be passed to the estimation software. The names of list elements must correspond to the arguments changed in the respective estimation software. E.g. \code{analysis.options$model} can contain additional modeling commands - such as regressions on auxiliary variables.
#' @param suppress.model A logical indicating whether to suppress the default model generation. If \code{TRUE} a model must be provided in \code{analysis.options$model}.
#' @param filename The stem of the filenames used to save inputs, outputs, and data files when \code{software='Mplus'}. Dafaults to "stuart".
#' 
#' 
#' 
### Outputs ---- 
#' @return Returns an object of the class \code{stuartOutput} for which specific \code{summary} and \code{plot} methods are available. The results are a list.
#' \item{call }{The called function.}
#' \item{software}{The software used to fit the CFA models.}
#' \item{parameters}{A list of the ACO parameters used.}
#' \item{timer}{An object of the class \code{proc_time} which contains the time used for the analysis.}
#' \item{log}{A \code{data.frame} containing the optimization history.}
#' \item{solution}{A list of matrices with the choices made in the global-best solution.}
#' \item{pheromones}{A list of matrices with the pheromones of each choice.}
#' \item{subtests}{A list containing the names of the selected items and their respective subtests.}
#' \item{final}{The results of the estimation of the global-best solution.}
#' 
#' 
### Examples ----
#' @examples
#' ###WARNING: Running these examples may take a very long time###
#' 
#' # Item selection: choosing 4 items per construct
#' # to generate short versions of two scales simultaneously
#' data(fairplayer)
#' names(fairplayer)
#' 
#' fs <- list(SI=names(fairplayer)[2:11],
#'   EM=names(fairplayer)[32:39])
#' \donttest{
#' item.selection <- mmas(fairplayer, fs, 1, 4, pbest=.1)
#' summary(item.selection)
#' summary(item.selection$FinalModel)
#' }  
#'  
#' @export


### Function definition ----
mmas <-
function(
  data, factor.structure, items.per.subtest=NULL, number.of.subtests=1, #subtest settings

  invariance='parallel', item.invariance='congeneric',                  #cross invariance

  repeated.measures=NULL, long.invariance='strict', item.long.invariance='strict', #long structure
  
  mtmm=NULL, mtmm.invariance='congeneric', item.mtmm.invariance='congeneric', #MTMM structure

  grouping=NULL, group.invariance='strict', item.group.invariance='strict', #grouping structure

  auxiliary=NULL,

  software='lavaan', cores=NULL,                                        #run settings

  fitness.func=fitness, ignore.errors=FALSE,                            #fitness specs

  ants=16, colonies=256, evaporation=.95,                               #general ACO specs
  alpha=1, beta=1, heuristics=NULL,                                     #general ACO specs
  deposit='ib', deposit.on='nodes', pbest=.005, tolerance=.001,         #MMAS specs
  
  analysis.options=NULL, suppress.model=FALSE,                          #modeling specs
  
  filename='stuart',                                                    #stem of filenames for Mplus
  
  ...
) { #begin function

  #sanity check
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

  #combine arguments
  args <- c(prepared,args[!names(args)%in%names(prepared)])

  #call the core function
  solution <- do.call('stuart.mmas',args)

  args$data <- data
  args$output.model <- TRUE
  args$selected.items <- solution$selected.items
  args$selected <- solution$selected.gb

  tmp <- formals(paste('run',software,sep='.'))
  args <- args[names(args)%in%names(tmp)]
  args <- c(args,tmp[!names(tmp)%in%c(names(args))])

  final.model <- do.call(paste('run',software,sep='.'),args)

  #generating output
  output <- list(call=match.call())  
  output$software <- software
  output$parameters <- c(solution$parameters)
  output$timer <- proc.time() - timer
  output$log <- solution$log
  output$solution <- solution$solution.gb
  output$pheromones <- solution$pheromones
  output$subtests <- solution$selected.items
  output$final <- final.model

  class(output) <- 'stuartOutput'
  return(output)
  
}
