### Roxygen-izable Documentation ----
#' Subtest construction using the Max-Min-Ant-System
#' 
#' Construct subtests from a given pool of items using the classical Max-Min Ant-System (Stützle, 1998). Allows for multiple constructs, occasions, and groups.
#' 
### Details ----
#' The pheromone function provided via \code{fitness.func} is used to assess the quality of the solutions. These functions can contain any combination of the fit indices provided by the estimation software. When using Mplus these fit indices are 'rmsea', 'srmr', 'cfi', 'tli', 'chisq' (with 'df' and 'pvalue'), 'aic', 'bic', and 'abic'. With lavaan any fit index provided by \code{\link[lavaan]{inspect}} can be used. Additionally 'crel' provides an aggregate of composite reliabilites, 'con' provides an aggregate consistency estimate for MTMM analyses, and 'lvcor' provides a list of the latent variable correlation matrices. Per default a pheromone function using 'crel', 'rmsea', and 'srmr' is used. Please be aware that the \code{fitness.func} must be a function with the required fit indices as (correctly named) arguments.
#' 
#' The scheduling of parameters is possible for the arguments \code{ants}, \code{colonies}, \code{evaporation}, \code{pbest}, \code{alpha}, \code{beta}, and \code{tolerance}. For all of these parameter scheduling is done when an array with two columns is provided. The first column of the array contains the timer, i.e. when to switch between parameter settings, the second column contains the values. The argument \code{schedule} can be used to switch between an absolute schedule pertaining to the total number of runs (the number of colonies investigated independent of colony counter resets) and a relative schedule pertaining to the number of colonies investigated since the last counter reset - the latter can be achieved with \code{schedule='colony'}.
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
#' @param mtmm A list linking factors that are measurements of the same construct with different methods. Measurements of the same construct must be in one element of the list - other sets of methods in other elements of the list. When this is \code{NULL} (the default) a single method model is estimated.
#' @param mtmm.invariance A character vector of length 1 or the same length as \code{mtmm} containing the invariance level of MTMM subtests. Currently there are five options: 'none', 'congeneric', 'weak', 'strong', and 'strict'. Defaults to 'congeneric'. With 'none' differing items are allowed for different methods. When \code{mtmm=NULL} this argument is ignored.
#' @param item.mtmm.invariance A character vector of length 1 or the same length as \code{mtmm} containing the invariance level of MTMM items. Currently there are five options: 'none', 'congeneric', 'weak', 'strong', and 'strict'. Defaults to 'congeneric'. With 'none' differing items are allowed for different methods. When \code{mtmm=NULL} this argument is ignored.
#' @param grouping The name of the grouping variable. The grouping variable must be part of \code{data} provided and must be a numeric variable.
#' @param group.invariance A single value describing the assumed invariance of subtests across groups. Currently there are four options: 'congeneric', 'weak', 'strong', and 'strict'. Defaults to 'strict'. When \code{grouping=NULL} this argument is ignored.
#' @param item.group.invariance A single value describing the assumed invariance of items across groups. Currently there are four options: 'congeneric', 'weak', 'strong', and 'strict'. Defaults to 'strict'. When \code{grouping=NULL} this argument is ignored.
#' @param auxiliary The names of auxiliary variables in \code{data}. These can be used in additional modeling steps that may be provided in \code{analysis.options$model}.
#' @param software The name of the estimation software. Can currently be 'lavaan' (the default) or 'Mplus'. Each option requires the software to be installed.
#' @param cores The number of cores to be used in parallel processing. If \code{NULL} (the default) the result of \code{\link[parallel]{detectCores}} will be used. On Unix-y machines parallel processing is implemented via \code{\link[parallel]{mclapply}}, on Windows machines it is realized via \code{\link[parallel]{parLapply}}.
#' @param fitness.func A function that converts the results of model estimation into a pheromone. See 'details' for... details.
#' @param ignore.errors A logical indicating whether or not to ignore estimation problems (such as non positive-definite latent covariance matrices). Defaults to \code{FALSE}.
#' @param ants The number of ants per colony to be estimated. Can either be a single value or an array with two columns for parameter scheduling. See 'details'.
#' @param colonies The maximum number of colonies estimated since finding the latest global-best solution before aborting the process. Can either be a single value or an array with two columns for parameter scheduling. See 'details'.
#' @param evaporation The evaporation coefficient. Can either be a single value or an array with two columns for parameter scheduling. See 'details'.
#' @param alpha The nonlinearity coefficient of the pheromone-trail's contribution to determining selection probabilities. Defaults to 1 (linear). Can either be a single value or an array with two columns for parameter scheduling. See 'details'.
#' @param beta The nonlinearity coefficient of the heuristics' contribution to determining selection probabilities. Defaults to 1 (linear). Can either be a single value or an array with two columns for parameter scheduling. See 'details'.
#' @param pheromones A list of pheromones as created by \code{\link{mmas}}. This can be used to continue previous runs of this function.
#' @param heuristics An object of the class \code{stuartHeuristic} as provided by \code{\link{heuristics}} which contains heuristic information to be used in determining selection probabilities. If \code{NULL} (the default) selection probabilities are determined solely by the pheromones.
#' @param deposit Which deposit rule to use. Can be either 'ib' (the default) for an iteration-best deposit rule, or 'gb' for a global-best deposit rule.
#' @param deposit.on Which parameterization to use when depositing pheromones. Can be either 'nodes' (the default) for depositing pheromones on selected nodes or 'arcs' for depositing on selection arcs.
#' @param pbest The desired overall probability of constructing the global-best solution when the algorithm convergels.  Can either be a single value or an array with two columns for parameter scheduling. See 'details'.
#' @param tolerance The tolerance of imprecision when comparing the pheromones to the upper and lower limits. Can either be a single value or an array with two columns for parameter scheduling. See 'details'.
#' @param schedule The counter which the scheduling of parameters pertains to. Can be either 'run' (the default), for a continuous schedule, or 'colony', for a schedule that is restarted every time a new global best is found.
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
#' item.selection <- mmas(fairplayer, fs, 4, pbest=.1)
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

  fitness.func=NULL, ignore.errors=FALSE,                               #fitness specs

  ants=16, colonies=256, evaporation=.95,                               #general ACO specs
  alpha=1, beta=1, pheromones=NULL, heuristics=NULL,                    #general ACO specs
  deposit='ib', deposit.on='nodes', pbest=.005, tolerance=.5,           #MMAS specs
  schedule='run',
  
  analysis.options=NULL, suppress.model=FALSE,                          #modeling specs
  
  filename='stuart'                                                     #stem of filenames for Mplus
) { #begin function

  #combine arguments
  args <- as.list(match.call())[-1]
  args <- c(args,formals()[!names(formals())%in%c(names(args),'...')])
  
  #sanity check
  do.call('sanitycheck',mget(names(formals(sanitycheck))))
  
  #multiple subtests warning
  if (any(unlist(number.of.subtests)>1)) {
    warning('The implementation of multiple subtests is currently experimental and may lead to expected results.')
  }
  
  timer <- proc.time()

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
