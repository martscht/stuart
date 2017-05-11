bf.cycle <-
function(run,
  filter, combi,
  data, auxi,                                                   #data and selection coding
  number.of.items,number.of.subtests,
  long.equal, item.long.equal,                                  #invariance labels
  factor.structure, repeated.measures, mtmm, grouping,          #basic requirements
  short.factor.structure, short,
  invariance, long.invariance, mtmm.invariance, group.invariance, #invariance settings
  item.invariance, item.long.invariance, item.mtmm.invariance,
  item.group.invariance,                                        #item invariance settings
  analysis.options, suppress.model,                             #additional analysis options
  fitness.func,                                                 #fitness function to call
  software,output.model=FALSE,
  ignore.errors=FALSE,
  filename,cores,
  ...
) {#function begin

  selected <- list()
  for (j in 1:length(combi)) {
    selected[[j]] <- list()
    for (k in 1:length(number.of.items[[j]])) {
      if (k > 1) ll <- cumsum(number.of.items[[j]])[k-1]+1
      else ll <- 1
      
      ul <- cumsum(number.of.items[[j]])[k]

      selected[[j]][[k]] <- as.numeric(combi[[j]][filter[run,j],ll:ul])
    }
  }
  selected.items <- translate.selection(selected,factor.structure,short)
  
  #specify modeling options
  output.model=FALSE
  run.options <- names(formals(paste('run',software,sep='.')))
  solution.fit <- do.call(paste('run',software,sep='.'),as.list(mget(run.options)))

  #compute pheromone
  fitness.options <- as.list(formals(fitness))
  fitness.options$solution.fit <- solution.fit
  fitness.options$fitness.func <- fitness.func
  if (!is.null(mtmm)) fitness.options$criteria <- c(as.character(fitness.options$criteria)[-1],'con')
  solution.phe <- do.call(fitness,fitness.options)
  if (!is.null(fitness.func)) {
    if ('rel'%in%names(formals(fitness.func))) {
      if (all(is.na(solution.phe$rel))) solution.phe$rel <- rep(NA,length(factor.structure)*max(c(1,length(unique(data[,grouping])))))
    }
  }
  
  return(list(selected=selected,solution.phe=solution.phe))

}
