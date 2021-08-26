### Combine objectives ----
empiricalobjective <- function(
  criteria = c('rmsea', 'srmr', 'crel'), 
  add = c('chisq', 'df', 'pvalue'),
  x = NULL,
  n = 50,
  side = NULL,
  skew = FALSE,
  scale = 1,
  fixed = NULL,
  comparisons = NULL,
  ...
) {

  called <- as.list(match.call())[-1]
  tmp <- as.list(formals(empiricalobjective))
  tmp <- tmp[setdiff(names(tmp), names(called))]
  called <- c(called, tmp)
  called <- called[names(called)!='x']
  
  if (is.null(x)) {
    warning('No empirical values provided to empiricalobjective. Attempting to return defaults.', call. = FALSE)
    out <- do.call(defaultobjective, called)
    class(out) <- 'stuartEmpiricalObjective'
    return(out)
  }
  
  tmp <- sapply(x, `[[`, 'solution.phe')
  tmp <- tmp[, !duplicated(t(tmp))]
  emp_values <- lapply(criteria, function(x) unlist(tmp[x, ]))
  names(emp_values) <- criteria
  
  # predefined sets for typical criteria
  tops <- c('^(delta\\.)?([^c]+)rel', '^(delta\\.)?crel', '^(delta\\.)?cfi', '^(delta\\.)?tli', '^(delta\\.)?nnfi', '^(delta\\.)?rfi',
    '^(delta\\.)?nfi', '^(delta\\.)?pnfi', '^(delta\\.)?ifi', '^(delta\\.)?rni', '^(delta\\.)?logl', '^(delta\\.)?gfi', '^(delta\\.)?agfi', '^(delta\\.)?pgfi',
    '^(delta\\.)?mfi', '^(delta\\.)?ecvi', '^(delta\\.)?pvalue')
  tops <- paste0(tops, collapse = '|')
  bottoms <- c('^(delta\\.)?chisq', '^(delta\\.)?aic', '^(delta\\.)?bic', '^(delta\\.)?bic2', '^(delta\\.)?rmsea', '^(delta\\.)?rmr', '^(delta\\.)?srmr',
    '^(delta\\.)?crmr')
  bottoms <- paste0(bottoms, collapse = '|')
  mids <- c('^(delta\\.)?lvcor', '^(delta\\.)?beta', '^(delta\\.)?con')
  mids <- paste0(mids, collapse = '|')
  
  
  # check for and expand arguments
  end_reason <- NULL
  if (length(n) %in% c(1, length(criteria))) {
    n <- rep(n, length.out = length(criteria))
  } else {
    end_reason <- c(end_reason, 'n')
  }
  if (length(side) %in% c(1, length(criteria)) | is.null(side)) {
    if (is.null(side)) {
      side <- rep(NA, length(criteria))
      side[grep(tops, criteria)] <- 'top'
      side[grep(bottoms, criteria)] <- 'bottom'
      side[grep(mids, criteria)] <- 'center'
    }
    side <- rep(side, length.out = length(criteria))
  } else {
    end_reason <- c(end_reason, 'side')
  }
  if (length(skew) %in% c(1, length(criteria))) {
    skew <- rep(skew, length.out = length(criteria))
  } else {
    end_reason <- c(end_reason, 'skew')
  }
  if (length(scale) %in% c(1, length(criteria))) {
    scale <- rep(scale, length.out = length(criteria))
  } else {
    end_reason <- c(end_reason, 'scale')
  }
  if (!is.null(end_reason)) {
    addendum <- NULL
    if (any(end_reason=='scale') & !is.null(fixed)) {
      addendum <- ' Please scale components provided to \"fixed\" locally.'
    }
    stop(paste('Could not determine empirical objectives because arguments did not match the number of criteria. Problems with:', paste(end_reason, collapse = ', '), '.', addendum))
  }
  
  arguments <- data.frame(criteria, n, side, skew, scale)
  
  obj_list <- vector('list', length(criteria))
  names(obj_list) <- criteria
  for (i in criteria) {
    args <- as.list(subset(arguments, criteria == i))
    args$x <- emp_values[[i]]
    tmp <- do.call(extractobjective, args)
    tmp$string <- gsub('x', i, tmp$string)
    obj_list[[i]] <- tmp
  }
  
  if (!is.null(fixed)) {
    if (class(fixed) == 'function') {
      fixed <- manualobjective(fixed)
    }
    obj_list[[length(obj_list) + 1]] <- fixed
    add <- union(add, eval(fixed$call$criteria))
    add <- union(add, eval(fixed$call$add))
  }
  
  
  tmp <- lapply(obj_list, `[[`, 'string')
  string <- paste0(tmp, collapse = ' + ')
  parsed <- parse(text = string)
  func <- function(...) eval(parsed)
  tmp <- vector('list', length(criteria) + length(add))
  names(tmp) <- c(criteria, add)
  formals(func) <- tmp
  
  out <- list(func = func, string = string, call = called)
  class(out) <- 'stuartEmpiricalObjective'
  return(out)
  
}
