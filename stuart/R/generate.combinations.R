generate.combinations <-
function(
  short.factor.structure, number.of.items
) {#begin function
  combi <- list()
  for (i in 1:length(short.factor.structure)) {
    tmp <- rep(list(1:length(short.factor.structure[[i]])),sum(number.of.items[[i]]))
    tmp <- expand.grid(tmp)
    tmp <- tmp[colSums(apply(tmp,1,duplicated))==0,]
    combi[[i]] <- tmp
  }

  filter <- expand.grid(lapply(lapply(combi,nrow),function(x) return(1:x)))

  return(list(combi=combi,filter=filter))

}
