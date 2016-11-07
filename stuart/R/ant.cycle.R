ant.cycle <-
function(
  deposit.on='arcs',                                            #type of solution construction
  data, auxi, use.order, pheromones,                            #data and selection coding
  alpha, beta, heuristics,
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
  filename,cores
) { #begin function

  #construct solution
  tmp <- mget(names(formals(paste('construction',deposit.on,sep='.'))))
  constructed <- do.call(paste('construction',deposit.on,sep='.'),tmp)

  solution <- constructed$solution
  selected <- constructed$selected
  
  #translate selection to names
  tmp <- mget(names(formals(translate.selection)))
  selected.items <- do.call(translate.selection,tmp)

  #specify modeling options
  run.options <- names(formals(paste('run',software,sep='.')))
  solution.fit <- do.call(paste('run',software,sep='.'),as.list(mget(run.options)))
  
  #compute pheromone
  fitness.options <- as.list(formals(fitness))
  fitness.options$solution.fit <- solution.fit
  fitness.options$fitness.func <- fitness.func
  if (!is.null(mtmm)) fitness.options$criteria <- c(as.character(fitness.options$criteria)[-1],'con')
  solution.phe <- do.call(fitness,fitness.options)
  if ('rel'%in%names(formals(fitness.func))) {
    if (all(is.na(solution.phe$rel))) solution.phe$rel <- rep(NA,length(factor.structure))
  }

  return(list(solution=solution,selected=selected,solution.phe=solution.phe))

}
