#' @export

plot.stuartOutput <-
function(x,remove.errors=TRUE,...) {
  
  phe <- x$log$pheromone
  run <- x$log$run
  
  if (remove.errors) {
    run <- run[phe!=0]
    phe <- phe[phe!=0]
  }
  
  best <- phe==max(phe)
  args <- as.list(match.call()[-1])
  args$x <- run
  args$y <- phe 
  args$xlab <- 'Run'
  args$ylab <- 'Pheromone'
  args$col <- as.numeric(best)+1
  if (is.null(args$pch)) args$pch <- 16

  do.call(plot,args)
  
  args$x <- lowess(phe~run)
  args$y <- NULL
  do.call(lines,args)
  
  legend('bottomright',c('Alternative','Final Solution'),pch=args$pch,col=unique(args$col))
}
