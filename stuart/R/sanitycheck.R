sanitycheck <- function(factor.structure,repeated.measures,mtmm,...) {
  #sanity check
  if (any(duplicated(names(factor.structure)))) {
    stop('You have provided duplicates in the name of factor.structure.',call.=FALSE)
  }
  if (any(!unlist(repeated.measures)%in%names(factor.structure))) {
    stop(paste('One or more factors appearing in repeated.measures is not present the factor.structure:',
      unlist(repeated.measures)[!unlist(repeated.measures)%in%names(factor.structure)]),call.=FALSE)
  }
  if (any(!unlist(mtmm)%in%names(factor.structure))) {
    stop(paste('One or more factors appearing in mtmm is not present the factor.structure:',
      unlist(mtmm)[!unlist(mtmm)%in%names(factor.structure)]),call.=FALSE)
  }
  
}