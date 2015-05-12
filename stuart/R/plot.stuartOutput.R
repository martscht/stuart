plot.stuartOutput <-
function(x,...) {
  best <- x$Log$pheromone==max(x$Log$pheromone)

  plot(x$Log$pheromone~x$Log$run,col=best+1,pch=16,
       xlab='Run',ylab='Pheromone')
  legend('bottomright',c('Alternative','Final Solution'),pch=16,col=c(1,2))
}
