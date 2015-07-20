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
#' @param new.data A \code{data.frame} of the validation sample.
#' @param old.data A \code{data.frame} of the calibration sample. This is only necessary when using Mplus.
#' @param invariance The invariance between the calibration and the validation sample. Can be one of 'congeneric', 'weak', 'strong', 'strict', or 'full', with the first being the default. Currently 'full' is only functional when using Mplus.
#' @param fitness.func A function that converts the results of model estimation into a pheromone. If none is provided the default function \code{fitness} is used. This can be examined with \code{body(fitness)}.
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
#' @examples 
#' data(fairplayer)
#' calib <- fairplayer[1:80,]
#' valid <- fairplayer[81:135,]
#' fs <- list(SI1=paste0('SI',1:10,'t1'),
#'  EM1=paste0('EM',1:8,'t1'))
#'
#'\donttest{
#' item.selection <- mmas(calib,fs,1,3,ants=1,colonies=1)
#' crossvalidate(item.selection,valid)
#' }
#' @concept ACO subtests
#' 
#' @export

### Function definition ----
crossvalidate <- 
function(
  selection, new.data, old.data=NULL,
  invariance='congeneric',
  fitness.func=fitness,
  filename='stuart',
  file.remove=TRUE,
  ...
) { #begin function
  
  if (!invariance%in%c('congeneric','weak','strong','strict','full'))
    stop('invariance must be congeneric, weak, strong, strict, or full.')
  
  # check estimation software
  software <- selection$EstimationSoftware
  
  if (software=='Mplus' & is.null(old.data)) stop('When using Mplus the old.data is required.')
  
  # set arguments for software-specific runs
  args <- as.list(match.call())[-1]
  args <- c(args,formals()[!names(formals())%in%c(names(args),'...')])
  args <- args[names(args)%in%names(formals(paste('crossvalidate',software,sep='.')))]
  
  # run validation
  validated <- do.call(paste('crossvalidate',software,sep='.'),args)    
  
  output <- do.call(fitness.func,list(validated))
  output <- rbind(selection$Log[which.max(selection$Log$pheromone),names(selection$Log)%in%names(output)],array(data=unlist(output)))
  rownames(output) <- c('calibration','validation')
  return(output)
  
} #end function