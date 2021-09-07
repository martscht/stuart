objectivematrices <- 
  function(
    data, factor.structure, capacity=NULL, #number.of.subtests=1, #subtest settings
    matrices = c('lvcor'), 
    #invariance='parallel',
    item.invariance='congeneric',                  #cross invariance
    
    repeated.measures=NULL, long.invariance='strict', #long structure
    mtmm=NULL, mtmm.invariance='configural', #MTMM structure
    grouping=NULL, group.invariance='strict', #grouping structure
    comparisons=NULL,
    auxiliary=NULL, use.order=FALSE,
    software='lavaan', cores=NULL,                                        #run settings
    objective=NULL, ignore.errors=FALSE,                      #fitness specs
    analysis.options=NULL, suppress.model=FALSE,                          #modeling specs
    ...
  ) {
    
    if (is.null(objective)) {
      objective <- fixedobjective()
    }
    filt <-  matrices
    filt <- setdiff(filt,  c(eval(objective$call$criteria), eval(objective$call$add)))
    
    objective$call$add <- c(eval(objective$call$add), filt)
    
    addage <- vector('list', length(filt))
    names(addage) <- filt
    formals(objective$func) <- c(formals(objective$func), addage)
    
    args <- as.list(match.call())[-1]
    args <- c(args,formals()[!names(formals())%in%c(names(args),'...')])
    args$n <- 1
    args$objective <- objective
    args$matrices <- NULL
    
    attempts <- 0
    worked <- FALSE
    message('Attempting to extract matrices from a random subset...')
    while (!worked & attempts < 10) {
      invisible(capture.output(suppressMessages(single <- do.call(randomsamples, args))))
      attempts <- attempts + 1
      if (single$log$pheromone != 0) {
        worked <- TRUE
        message('\b done!')
      }
    }
    if (!worked) {
      stop('Was not able to extract matrices in 10 attempts. This may indicate a problem with the model, but may also be resolved by simply trying again.', call. = FALSE)
    }
    
    tmp <- single$log_mat
    out <- vector('list', length(matrices))
    names(out) <- matrices
    types <- c('use', 'mean', 'sd', 'side', 'skew', 'scale')
    for (i in matrices) {
      out[[i]] <- vector('list', length(types))
      names(out[[i]]) <- types
      for (j in types) {
        out[[i]][[j]] <- extractmatrices(tmp[[i]][[1]], j)
      }
    }
    
    class(out) <- 'stuartObjectiveMatrices'
    return(out)

  }


extractmatrices <- function(x, type = 'use') {
  if (is.list(x)) {
    lapply(x, extractmatrices, type = type)
  }
  else {
    x <- unclass(x)
    if (type == 'use') {out <- x != 0}
    if (type == 'mean') {x[x != Inf] <- 0; out <- x}
    if (type == 'sd') {x[x != Inf] <- 1; out <- x}
    if (type == 'side') {x[x != Inf] <- 'center'; out <- x}
    if (type == 'skew') {out <- x == Inf}
    if (type == 'scale') {x[x != Inf] <- 1; out <- x}
    return(out)
  }
}
