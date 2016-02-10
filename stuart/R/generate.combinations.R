generate.combinations <-
function(
  short.factor.structure, number.of.items, use.order,
  full=TRUE, n=1000
) {#begin function
  combi <- list()
  
  n_max <- do.call('compute.combinations',mget(names(formals(compute.combinations))))
  
  if (full) {
    if (use.order) {
      for (i in 1:length(short.factor.structure)) {
        tmp <- rep(list(1:length(short.factor.structure[[i]])),sum(number.of.items[[i]]))
        tmp <- expand.grid(tmp)
        tmp <- tmp[colSums(apply(tmp,1,duplicated))==0,]
        combi[[i]] <- tmp
      }
    } else {
      for (i in 1:length(short.factor.structure)) {
        combi[[i]] <- t(combn(length(short.factor.structure[[i]]),number.of.items[[i]]))
      }  
    }
    filter <- expand.grid(lapply(lapply(combi,nrow),function(x) return(1:x)))
    
  } else {
    if (n > n_max) warning('The number of random samples is larger than the number of possible combinations.')
    
    for (i in 1:length(short.factor.structure)) {
      tmp <- matrix(NA,nrow=n,ncol=number.of.items[[i]])
      tmp <- t(apply(tmp,1,function(x) sample(1:length(short.factor.structure[[i]]),number.of.items[[i]])))
      combi[[i]] <- tmp
    }
    if (!use.order) {
      combi <- lapply(combi,function(x) t(apply(x,1,sort)))
    }
    
    filter <- as.data.frame(matrix(1:n,nrow=n,ncol=length(short.factor.structure)))
  }

  return(list(combi=combi,filter=filter))

}
