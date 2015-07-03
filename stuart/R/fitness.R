fitness <-
function(
  solution.fit
) { #begin function


  if (all(is.na(solution.fit))) {
    return(list(pheromone=0,chisq=NA,df=NA,p=NA,rmsea=NA,srmr=NA,crel=NA))
  }

  else {
    pheromone <- with(solution.fit, 1/(1+exp(4-10*(crel))) +
              .5*(1 - (1/(1+exp(5-100*rmsea)))) +
              .5*(1 - (1/(1+exp(5-100*srmr))))) 
    return(list(pheromone=pheromone,chisq=solution.fit$chisq,df=solution.fit$df,p=solution.fit$pvalue,rmsea=solution.fit$rmsea,srmr=solution.fit$srmr,crel=solution.fit$crel))
  }

}
