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
#' 
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