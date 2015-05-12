ant.cycle <-
function(
  deposit.on='arcs',                                            #type of solution construction
  data, auxi, pheromones,                                       #data and selection coding
  alpha, beta, heuristics,
  number.of.items,number.of.subtests,
  long.equal, item.long.equal,                                  #invariance labels
  factor.structure, repeated.measures, grouping,                #basic requirements
  short.factor.structure,
  invariance, long.invariance, group.invariance,                #invariance settings
  item.invariance, item.long.invariance, item.group.invariance, #item invariance settings
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
  selected.items <- translate.selection(selected,factor.structure,repeated.measures)

  #specify modeling options
  run.options <- names(formals(paste('run',software,sep='.')))
  solution.fit <- do.call(paste('run',software,sep='.'),as.list(mget(run.options)))
  
  #compute pheromone
  solution.phe <- do.call(fitness.func,list(solution.fit))

  return(list(solution=solution,selected=selected,solution.phe=solution.phe))

}
