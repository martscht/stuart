ant.cycle <-
function(
  deposit.on='arcs',                                            #type of solution construction
  data, auxi, pheromones,                                       #data and selection coding
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
  filename,cores,
  ...
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
  fitness.options <- as.list(formals(fitness.func))
  fitness.options$solution.fit <- solution.fit
  if (!is.null(mtmm)) fitness.options$criteria <- c(as.character(fitness.options$criteria)[-1],'con')
  solution.phe <- do.call(fitness.func,fitness.options)

  return(list(solution=solution,selected=selected,solution.phe=solution.phe))

}
