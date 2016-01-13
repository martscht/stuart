generate.combinations <-
function(
  short.factor.structure, number.of.items, 
  full=TRUE, n=1000
) {#begin function
  combi <- list()

  if (full) {
    for (i in 1:length(short.factor.structure)) {
      tmp <- rep(list(1:length(short.factor.structure[[i]])),sum(number.of.items[[i]]))
      tmp <- expand.grid(tmp)
      tmp <- tmp[colSums(apply(tmp,1,duplicated))==0,]
      combi[[i]] <- tmp
    }
  } else {
    for (i in 1:length(short.factor.structure)) {
      tmp <- rep(list(1:length(short.factor.structure[[i]])),sum(number.of.items[[i]]))
      combi[[i]] <- sapply(tmp,sample,n,replace=TRUE)
      combi[[i]] <- combi[[i]][apply(combi[[i]],1,function(x) !any(duplicated(x))),]
      while (nrow(combi[[i]])<n) {
        combi[[i]] <- rbind(combi[[i]],sapply(tmp,sample,n-nrow(combi[[i]]),replace=TRUE))
        combi[[i]] <- combi[[i]][apply(combi[[i]],1,function(x) !any(duplicated(x))),]
      }
    }
  }

  filter <- expand.grid(lapply(lapply(combi,nrow),function(x) return(1:x)))

  return(list(combi=combi,filter=filter))

}
