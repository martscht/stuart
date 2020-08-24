kfold <- function(type, k = 5,
  max.invariance = 'strict',
  ...) {

  # unpack ellipses
  args <- list(...)
  
  # split data
  folded <- list()
  
  hold_args <- args[names(args) %in% names(formals(holdout))]
  hold_args$data <- args$data
  hold_args$data$.determined_internal <- NA
  hold_args$prop <- 1/k
  hold_args$determined <- '.determined_internal'
  
  for (i in 1:k) {
    folded[[i]] <- do.call('holdout', hold_args)
    hold_args$data <- do.call('rbind', folded[[i]])
    hold_args$data$.determined_internal[1:nrow(folded[[i]]$calibrate)] <- 'validate'
  }
  
  # Run searches
  searches <- list()
  
  for (i in 1:k) {
    args$data <- folded[[i]]
    message(paste0('\nRunning fold number ', i, ' of ', k, '.\n'))
    searches[[i]] <- do.call(type, args)
  }

  # Run crossvalidation
  message('\nRunning cross-validation.\n')
  cv <- list()
  for (i in 1:k) {
    selection <- searches[[i]]
    old.data <- folded[[i]]
    cv[[i]] <- try(crossvalidate(selection, old.data), silent = TRUE)
    if ('try-error' %in% class(cv[[i]])) cv[[i]] <- NA
  }
  
  # Reorganize solutions
  solu <- searches[[1]][['solution']]
  for (i in seq_along(solu)) {
    solu[[i]] <- do.call('rbind', lapply(lapply(searches, `[[`, 'solution'), `[[`, i))
  }
  
  # identify best solution
  phe <- sapply(cv, function(x) x[['comparison']][max.invariance, 'pheromone'])
  best <- searches[[which.max(phe)]]
  
  out <- list()
  out$subtests <- best$subtests
  out$solution <- best$solution
  out$final <- cv[[which.max(phe)]][['models']][[max.invariance]]
  out$validation <- cv[[which.max(phe)]][['comparison']]
  out$frequencies <- lapply(solu, colMeans)
  out$full <- searches
  out$crossvalidations <- cv
  
  class(out) <- 'stuartKfold'
  
  return(out)

}