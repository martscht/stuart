construction.nodes <-
function(
  pheromones, number.of.items, #made in stuart.mmas
  use.order,
  alpha, beta, heuristics  
) { #begin function

  solution <- lapply(pheromones,function(x) x<0)

  #initialize chosen vectors
  selected <- list(NA)

  for (i in 1:length(number.of.items)) { #for each factor
    #initialize chosen vectors for factor
    selected[[i]] <- list(NA)
    #create a pool of possible choices
    pool <- 1:ncol(solution[[i]])

    for (j in 1:length(number.of.items[[i]])) { #for each subtest
      #compute selection probabilities
      tmp.phe <- pheromones[[i]][j,pool]^alpha
      tmp.heu <- heuristics[[i]][j,pool]^beta
      probs <-  tmp.phe * tmp.heu / sum(tmp.phe*tmp.heu)

      #select items
      selected[[i]][[j]] <- sample(pool,number.of.items[[i]][j],FALSE,probs)

      #update pool to exclude choice
      pool <- pool[!is.element(pool,selected[[i]][[j]])]

      solution[[i]][j,selected[[i]][[j]]] <- TRUE
    }
  }

  if (!use.order) {
    selected <- lapply(selected,function(x) lapply(x,sort))
  }

  return(list(selected=selected,solution=solution))

}
