#' @export

summary.stuartOutput <-
function(x,...) {
  Time <- as.numeric(x$Timer[3])
  Models <- nrow(x$Log)
  Replications <- sum(x$Log$pheromone==max(x$Log$pheromone))
  Results <- x$Log[x$Log$pheromone==cummax(x$Log$pheromone),]
  Results <- Results[!duplicated(Results[,3:ncol(Results)]),]

  Out <- list(Subtests=x$Subtests,Results=Results,Time=Time,Models=nrow(x$Log),
              Replications=Replications,Type=paste(x$Call),Software=x$EstimationSoftware)
  class(Out) <- 'summary.stuartOutput'
  return(Out)
}
