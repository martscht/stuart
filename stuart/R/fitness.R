fitness <-
function(fitness.func=NULL,
  solution.fit, criteria=c('chisq','df','pvalue','rmsea','srmr','crel')
) { #begin function

  # preset fitness function
  if (is.null(fitness.func)) {
    fitness.func <- function(crel,rmsea,srmr) {
      1/(1+exp(4-10*(crel))) +
      .5*(1 - (1/(1+exp(5-100*rmsea)))) +
      .5*(1 - (1/(1+exp(5-100*srmr))))
    }
  } else {
    criteria <- names(formals(fitness.func))
  }
  
  output <- list()
  
  if (!all(criteria%in%names(solution.fit))) {
    output[[1]] <- 0
    for (i in 1:length(criteria)) {
      output[[i+1]] <- NA
    }
    names(output) <- c('pheromone',unlist(criteria))
  }
  
  else {
    pheromone <- do.call(fitness.func,solution.fit[names(formals(fitness.func))])
    
    output[[1]] <- pheromone
    for (i in 1:length(criteria)) {
      output[[i+1]] <- solution.fit[[unlist(criteria[i])]]
    }
    names(output) <- c('pheromone',unlist(criteria))
  }
  
  # remove matrices from output
  output$lvcor <- NULL
  output$lambda <- NULL
  output$theta <- NULL
  output$psi <- NULL
  output$alpha <- NULL
  
  return(output)

}
