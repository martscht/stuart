init.pheromones <-
function(
  short.factor.structure, number.of.subtests, deposit.on='arcs'
) { #begin function

  pheromones <- list(NA)

  #initialize when depositing on arcs
  if (deposit.on=='arcs') {
    #initialize all pheromones to 1e+100, diagonals to 0
    for (i in 1:length(short.factor.structure)) {
      pheromones[[i]] <- list(NA)
      pheromones[[i]] <- matrix(1e+100,length(short.factor.structure[[i]]),length(short.factor.structure[[i]]))
      diag(pheromones[[i]]) <- 0
      dimnames(pheromones[[i]]) <- list(short.factor.structure[[i]],short.factor.structure[[i]])
    }
  }

  #initialize when depositing on nodes
  if (deposit.on=='nodes') {
    #initialize all pheromones to 1e+100
    for (i in 1:length(short.factor.structure)) {
      pheromones[[i]] <- list(NA)
      pheromones[[i]] <- matrix(1e+100,number.of.subtests[[i]],length(short.factor.structure[[i]]))
      dimnames(pheromones[[i]])[[2]] <- short.factor.structure[[i]]
    }
  }

  if (!deposit.on%in%c('arcs','nodes')) stop('The deposit must be either on arcs or nodes.')

  names(pheromones) <- names(short.factor.structure)

  return(pheromones)

}
