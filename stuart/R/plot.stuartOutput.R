#' @export

plot.stuartOutput <-
function(x,remove.errors=TRUE,...) {
  
  phe <- x$Log$pheromone
  run <- x$Log$run
  
  if (remove.errors) {
    run <- run[phe!=0]
    phe <- phe[phe!=0]
  }
  
  best <- phe==max(phe)

  plot(phe~run,col=rgb(.06,.31,.55,.2),pch=16,
       xlab='Run',ylab='Pheromone')
  points(phe[best]~run[best],col=rgb(.55,.41,.08),pch=16)
  legend('bottomright',c('Alternative','Final Solution'),pch=16,col=c(rgb(.06,.31,.55),rgb(.55,.41,.08)))
  lines(lowess(phe~run))
}
