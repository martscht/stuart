kfold <- function(type, k = 5,
  max.invariance = 'strict',
  ...,
  remove.details = TRUE) {

  # unpack ellipses
  args <- list(...)
  
  # check for multiple groups
  if ('grouping' %in% names(args)) stop('Multiple groups are not yet supported in k-folds crossvalidation.', call. = FALSE)
  
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
    searches[[i]] <- try(do.call(type, args))
  }
  
  check <- sapply(searches, function(x) 'try-error' %in% class(x))
  if (all(check)) stop('None of the folds resulted in viable solutions. This may be the result of the sample being to small for the number of folds.', call. = FALSE)

  # Run crossvalidation
  message('\nRunning cross-validation.\n')
  cv <- list()
  for (i in 1:k) {
    selection <- searches[[i]]
    old.data <- folded[[i]]
    cv[[i]] <- try(crossvalidate(selection, old.data, max.invariance = max.invariance), silent = TRUE)
    if ('try-error' %in% class(cv[[i]])) cv[[i]] <- list(comparison = NULL, models = NULL)
  }
  
  # Reorganize solutions
  solu <- searches[[1]][['solution']]
  for (i in seq_along(solu)) {
    solu[[i]] <- do.call('rbind', lapply(lapply(searches, `[[`, 'solution'), `[[`, i))
  }
  
  # identify best solution
  phe <- sapply(cv, function(x) {
    if (is.null(x[['comparison']])) return(NA)
    else return(x[['comparison']][max.invariance, 'pheromone'])})
  
  best <- searches[[which.max(phe)]]
  best_cv <- cv[[which.max(phe)]]
  
  # remove models and data for non-best
  if (remove.details) {
    searches <- lapply(searches, function(x) x[!(names(x) %in% c('final', 'call'))])
    cv <- lapply(cv, `[[`, 'comparison')
  }
  
  dats <- do.call(rbind, lapply(folded, `[[`, 'calibrate'))
  dats$stuartKfold <- unlist(sapply(1:k, function(x) rep(x, nrow(folded[[x]][['calibrate']]))))
  rownames(dats) <- NULL
  
  out <- list(call = match.call())
  out$subtests <- best$subtests
  out$solution <- best$solution
  out$final <- best_cv[['models']][[max.invariance]]
  out$validation <- best_cv[['comparison']]
  out$frequencies <- lapply(solu, colMeans)
  out$full <- searches
  out$crossvalidations <- cv
  out$data <- dats
  
  class(out) <- 'stuartKfold'
  
  return(out)

}