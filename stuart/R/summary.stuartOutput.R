#' @export

summary.stuartOutput <-
function(x,...) {
  Time <- as.numeric(x$timer[3])
  Models <- nrow(x$log)
  Replications <- sum(x$log$pheromone==max(x$log$pheromone))
  Results <- x$log[x$log$pheromone==cummax(x$log$pheromone),]
  Results <- Results[!duplicated(Results[,3:ncol(Results)]),]

  Out <- list(Subtests=x$subtests,Results=Results,Time=Time,Models=Models,
              Replications=Replications,Type=paste(x$call),Software=x$software)
  class(Out) <- 'summary.stuartOutput'
  return(Out)
}
