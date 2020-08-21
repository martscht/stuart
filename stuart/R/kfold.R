kfold <- function(type, k = 5,
  ...) {

  # unpack ellipses
  args <- list(...)
  
  # split data
  folded <- list()
  
  hold_args <- args[names(args) %in% names(formals(holdout))]
  hold_args$data <- data
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
  cv <- list()
  for (i in 1:k) {
    selection <- searches[[i]]
    old.data <- folded[[i]]
    cv[[i]] <- try(crossvalidate(selection, old.data), silent = TRUE)
    if ('try-error' %in% class(cv[[i]])) cv[[i]] <- NA
  }
  
  out <- list(searches, cv)
  
  return(out)

}