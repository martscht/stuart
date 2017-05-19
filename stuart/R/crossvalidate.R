### Roxygen-izable Documentation ----
#' Cross-Validate a Measurement Model
#' 
#' Cross-validate a measurement model obtained via either \code{\link{mmas}} or \code{\link{bruteforce}}.
#' 
#' @author Martin Schultze
#' 
#' @seealso \code{\link{mmas}}, \code{\link{bruteforce}}
#' 
### Inputs ----
#' @param selection An object of class \code{stuartOutput}.
#' @param old.data A \code{data.frame} of the calibration sample.
#' @param new.data A \code{data.frame} of the validation sample.
#' @param invariance The invariance between the calibration and the validation sample. Can be one of 'congeneric', 'weak', 'strong', 'strict', or 'full', with the first being the default. Currently 'full' is only functional when using Mplus.
#' @param objective A function that converts the results of model estimation into a pheromone. If none is provided the default function \code{fitness} is used. This can be examined with \code{body(fitness)}.
#' @param filename The stem of the filenames used to save inputs, outputs, and data files when using Mplus. Defaults to "stuart".
#' @param file.remove A logical indicating whether to remove the generated Mplus input and output files. Ignored if lavaan is used.
#' 
### Outputs ----
#' @return Returns a \code{data.frame} with 2 observations of 7 variables. The first observation is the fitness of the final model for the calibration sample. The second observation is the fitness of the model for the validation sample.
#' \item{pheromone}{The value of the pheromone function.}
#' \item{chisq}{The Chi-Square of the model.}
#' \item{df}{Degrees of freedom of the model.}
#' \item{p}{p-Value of the Chi-Square test of model fit.}
#' \item{rmsea}{Root-Mean-Square-Error of Approximation.}
#' \item{srmr}{Standardized Root Mean Residual.}
#' \item{crel}{A measure of composite reliability.}
#' 
#' @concept ACO subtests
#' 
#' @export

### Function definition ----
crossvalidate <- 
function(
  selection, old.data, new.data,
  invariance='congeneric',
  objective=NULL,
  filename='stuart',
  file.remove=TRUE
) { #begin function
  
  if (!invariance%in%c('congeneric','weak','strong','strict','full'))
    stop('invariance must be congeneric, weak, strong, strict, or full.')
  
  # check estimation software
  software <- selection$software
  # check fitness function
  if (is.null(objective)) objective <- selection$parameters$objective
  
  if (software=='Mplus' & is.null(old.data)) stop('When using Mplus the old.data is required.')
  
  # set arguments for software-specific runs
  args <- as.list(match.call())[-1]
  args <- c(args,formals()[!names(formals())%in%c(names(args),'...')])
  args <- args[names(args)%in%names(formals(paste('crossvalidate',software,sep='.')))]
  
  # run validation
  validated <- do.call(paste('crossvalidate',software,sep='.'),args)    
  fitness.options <- as.list(formals(fitness))
  fitness.options$solution.fit <- validated
  fitness.options$objective <- objective
  if ('con'%in%names(selection$log)) fitness.options$criteria <- c(as.character(fitness.options$criteria)[-1],'con')
  
  output <- do.call(fitness,fitness.options)
  output <- rbind(selection$log[which.max(selection$log$pheromone),names(selection$log)%in%names(output)],array(data=unlist(output)))
  rownames(output) <- c('calibration','validation')
  return(output)
  
} #end function