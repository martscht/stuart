mmas.update <-
function(
  pheromones, phe.min, phe.max, evaporation,
  phe, solution
) { #begin function

      tmp <- list(NA)
      for (i in 1:length(pheromones)) {
        tmp[[i]] <- phe*solution[[i]]
        tmp[[i]] <- evaporation*pheromones[[i]] + tmp[[i]]
        for (j in 1:nrow(tmp[[i]])) {
          for (k in 1:ncol(tmp[[i]])) {
            if (pheromones[[i]][j,k]!=0) {
              tmp[[i]][j,k] <- max(phe.min, min(phe.max,tmp[[i]][j,k]))
            }
          }
        }
      }

  return(tmp)

}
