### Roxygen-izable Documentation ----
#' Cross-Validate a Measurement Model
#' 
#' Can be used to cross-validate a measurement model obtained via either \code{\link{mmas}} or \code{\link{bruteforce}}.
#' 
#' @author Martin Schultze
#' 
#' @seealso \code{\link{mmas}, }
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
  output <- rbind(selection$Log[which.max(selection$Log$pheromone),-c(1:2)],array(data=unlist(output)))
  rownames(output) <- c('calibration','validation')
  return(output)
  
} #end function