sanitycheck <- function(factor.structure,repeated.measures,mtmm,fitness.func=NULL) {
  #sanity check
  if (any(duplicated(names(factor.structure)))) {
    stop('You have provided duplicates in the name of factor.structure.',call.=FALSE)
  }
  
  if (any(!unlist(repeated.measures)%in%names(factor.structure))) {
    stop(paste('One or more factors appearing in repeated.measures is not present the factor.structure:',
      paste(unlist(repeated.measures)[!unlist(repeated.measures)%in%names(factor.structure)],collapse=', ')),call.=FALSE)
  }
  
  if (any(!unlist(mtmm)%in%names(factor.structure))) {
    stop(paste('One or more factors appearing in mtmm is not present the factor.structure:',
      unlist(mtmm)[!unlist(mtmm)%in%names(factor.structure)]),call.=FALSE)
  }
  
  if (is.null(mtmm)&'con'%in%names(formals(fitness.func))) {
    stop('The fitness function you requested uses consistency in the pheromone computation but mtmm=NULL',call.=FALSE)
  }
  
}