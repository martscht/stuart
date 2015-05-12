construction.arcs <-
function(
  pheromones, number.of.items, #made in stuart.mmas
  alpha, beta, heuristics
) { #begin function

  #initialize a choice matrix
  solution <- lapply(pheromones,function(x) x<0)

  #initialize chosen vectors
  selected <- list(NA)

  for (i in 1:length(number.of.items)) { #for each factor
    #initialize chosen vectors for factor
    selected[[i]] <- list(NA)
    #create a pool of possible choices
    pool <- 1:ncol(solution[[i]])

    for (j in 1:length(number.of.items[[i]])) { #for each subtest
      #randomly select starting position from pool
      selected[[i]][[j]] <- sample(pool,1)
      #update pool to exclude starting location
      pool <- pool[!is.element(pool,selected[[i]][[j]])]

      for (k in 2:number.of.items[[i]][[j]]) {  #for each item
        #compute selection probabilities
        tmp.phe <- pheromones[[i]][selected[[i]][[j]][k-1],pool]^alpha
        tmp.heu <- heuristics[[i]][selected[[i]][[j]][k-1],pool]^beta
        probs <-  tmp.phe * tmp.heu / sum(tmp.phe*tmp.heu)
        
        #select item (complicated due to sample()-convenience feature)
        if (length(pool)==1) {
          selected[[i]][[j]][k] <- pool
        }
        else {
          selected[[i]][[j]][k] <- sample(pool,1,FALSE,probs)
        }
  
        #update solution
        solution[[i]][selected[[i]][[j]][k-1],selected[[i]][[j]][k]] <- TRUE 
        #update pool to exclude choice
        pool <- pool[!is.element(pool,selected[[i]][[j]])]
      }
    }
  }

  return(list(selected=selected,solution=solution))

}
