invariance.implementation <-
function(
  data,                                                         #data
  factor.structure, short.factor.structure, repeated.measures,  #data structure
  number.of.some, 

  invariance, long.invariance, group.invariance,                #invariances

  grouping,                                                     #additional data
  label.change=FALSE                                            #replace label names?
) { #begin function

  #longitudinal invariance parameters
  long.equal <- rep(list(list(lam=NA,alp=NA,eps=NA)),length(factor.structure))

  for (i in 1:length(factor.structure)) {
    if (is.element(names(factor.structure)[i],names(short.factor.structure))) {
      
      locate <- which(names(short.factor.structure)==names(factor.structure)[i])

      if (invariance[[locate]]=='congeneric') {
      long.equal[[i]]$lam <- paste('lam',i,1:number.of.some[[locate]],sep='') 
      }
      else {
      long.equal[[i]]$lam <- rep(paste('lam',i,sep=''),number.of.some[[locate]]) 
      }
      
      if (invariance[[locate]]%in%c('equivalent','parallel')) {
        long.equal[[i]]$alp <- rep(paste('alp',i,sep=''),number.of.some[[locate]]) 
      }
      else {            
        long.equal[[i]]$alp <- paste('alp',i,1:number.of.some[[locate]],sep='') 
      }

      if (invariance[[locate]]%in%c('ess.parallel','parallel')) {
        long.equal[[i]]$eps <- rep(paste('eps',i,sep=''),number.of.some[[locate]]) 
      }
      else {
        long.equal[[i]]$eps <- paste('eps',i,1:number.of.some[[locate]],sep='') 
      }
    }

    else {

      locate <- which(unlist(lapply(repeated.measures,
        function(x) is.element(names(factor.structure)[i],x)),use.names=FALSE))
      long.equal[[i]] <- long.equal[[i-1]]

      if (long.invariance[[locate]]!='strict') {
        long.equal[[i]]$eps <- paste(long.equal[[i]]$eps,letters[i],sep='')
      }

      if (long.invariance[[locate]]%in%c('weak','congeneric')) {
        long.equal[[i]]$alp <- paste(long.equal[[i]]$alp,letters[i],sep='')
      }


      if (long.invariance[[locate]]=='congeneric') {
        long.equal[[i]]$lam <- paste(long.equal[[i]]$lam,letters[i],sep='')
      }

    }
  }
  
  #implementing group invariance
  if (!is.null(grouping)) {
    group <- as.factor(data[,grouping])
    long.equal <- list(long.equal,long.equal)

    for (i in 2:length(levels(group))) {
      long.equal[[i]] <- long.equal[[1]] }

    #add variable residuals
    if (group.invariance!='strict') {
      for (i in 2:length(levels(group))) {
        for (j in 1:length(factor.structure)) {
          long.equal[[i]][[j]]$eps <- paste(long.equal[[i]][[j]]$eps,'g',i,sep='')
        }
      }
    }
    
    #add variable intercepts
    if (group.invariance%in%c('weak','congeneric')) {
      for (i in 2:length(levels(group))) {
        for (j in 1:length(factor.structure)) {
          long.equal[[i]][[j]]$alp <- paste(long.equal[[i]][[j]]$alp,'g',i,sep='')
        }
      }
    }

    #add variable loadings
    if (group.invariance=='congeneric') {
      for (i in 2:length(levels(group))) {
        for (j in 1:length(factor.structure)) {
          long.equal[[i]][[j]]$lam <- paste(long.equal[[i]][[j]]$lam,'g',i,sep='')
        }
      }
    }
  }
  
  if (label.change) {
    tmp <- as.relistable(long.equal)
    tmp <- unlist(tmp)
    tmp <- gsub('lam','gam',tmp)
    tmp <- gsub('alp','mu',tmp)
    tmp <- gsub('eps','zet',tmp)
    long.equal <- relist(tmp)
  }

  return(long.equal)
}
