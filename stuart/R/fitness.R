fitness <-
function(
  solution.fit, criteria=c('chisq','df','pvalue','rmsea','srmr','crel')
) { #begin function

  output <- list()

  if (!all(criteria%in%names(solution.fit))) {
    output[[1]] <- 0
    for (i in 1:length(criteria)) {
      output[[i+1]] <- NA
    }
    names(output) <- c('pheromone',unlist(criteria))
  }
  
  else {
    pheromone <- with(solution.fit, 1/(1+exp(4-10*(crel))) +
              .5*(1 - (1/(1+exp(5-100*rmsea)))) +
              .5*(1 - (1/(1+exp(5-100*srmr)))))
    
    output[[1]] <- pheromone
    for (i in 1:length(criteria)) {
      output[[i+1]] <- solution.fit[[unlist(criteria[i])]]
    }
    names(output) <- c('pheromone',unlist(criteria))
  }
  
  return(output)

}
