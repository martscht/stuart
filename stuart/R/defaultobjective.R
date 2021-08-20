defaultobjective <- function(
  criteria = c('rmsea', 'srmr', 'crel'), 
  add = c('chisq', 'df', 'pvalue'),
  scale = 1,
  fixed = NULL,
  ...) {
  
  # predefined sets for typical criteria
  predef <- c('^([^c]+)rel', '^crel', '^cfi', '^tli', '^nnfi', '^rfi',
    '^nfi', '^pnfi', '^ifi', '^rni', '^gfi', '^agfi', '^pgfi',
    '^mfi', '^ecvi', '^pvalue', '^chisq', '^aic', '^bic', '^bic2', '^rmsea', '^rmr', '^srmr',
    '^crmr', 'lvcor', 'beta', 'con')
  predef_check <- paste0(predef, collapse = '|')
  
  if (any(!grepl(predef_check, criteria))) {
    end_reason <- criteria[!grepl(predef_check, criteria)]
    stop(paste0('Not all criteria provided to the objective function have defaults. Problem with: ', paste(end_reason, collapse = ', '), '. Consider using these as auxiliary information via \"add\" or including them manually via \"fixed\" instead.'))
  }
  
  defaults <- data.frame(criterion = predef, 
    side = c(rep('top', 16), rep('bottom', 8), rep('center', 3)),
    m = c(.7, .8, .95, .95, .95, .95, .95, .6, .95, .95, .95, .95, .5,
      .95, .4, .05, 0, 0, 0, 0, .05, .05, .05, .05, 0, 0, .7),
    s = c(.1, .075, .03, .03, .03, .03, .03, .1, .03, .03, .03, .03, .12,
      .03, .1, .1, 10, 10, 10, 10, .015, .02, .015, .02, .2, .2, .2),
    scale = 1)

  # Check for correct scaling
  if (length(scale) %in% c(1, length(criteria))) {
    scale <- rep(scale, length.out = length(criteria))
  } else {
    if (!is.null(fixed)) {
      addendum <- 'Please scale components provided to \"fixed\" locally.'
    }
    stop(paste('Could not determine empirical objectives because arguments did not match the number of criteria. Problems with: scale.', addendum))
  }
  
  tmp <- data.frame(criteria, scale)
  for (i in criteria) {
    filt <- sapply(defaults$criterion, grepl, x = i)
    defaults[filt, 'scale'] <- subset(tmp, criteria == i, select = scale)
  }
  
  
  obj_list <- vector('list', length = length(criteria))
  names(obj_list) <- criteria
  for (i in criteria) {
    filt <- sapply(defaults$criterion, grepl, x = i)
    cur_default <- defaults[filt, ]
    if (cur_default$side == 'top') {
      string <- paste0(cur_default$scale, ' * pnorm(x, ', cur_default$m, ', ', cur_default$s, ', lower.tail = TRUE)')
    }
    if (cur_default$side == 'bottom') {
      string <- paste0(cur_default$scale, ' * pnorm(x, ', cur_default$m, ', ', cur_default$s, ', lower.tail = FALSE)')
    }
    if (cur_default$side == 'center' | cur_default$side == 'centre') {
      string <- paste0(cur_default$scale, ' * 2 * ifelse(x > ', cur_default$m, ', pnorm(x, ', cur_default$m, ', ', cur_default$s, ', lower.tail = FALSE), pnorm(x, ', cur_default$m, ', ', cur_default$s, ', lower.tail = TRUE))')
    }
   
    string <- gsub('x', i, string)
    parsed <- parse(text = string)
    func <- function(x) eval(parsed)
    
    obj_list[[i]] <- list(func = func, string = string) 
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
  
  called <- as.list(match.call())[-1]
  tmp <- as.list(formals(empiricalobjective))
  tmp <- tmp[setdiff(names(tmp), names(called))]
  called <- c(called, tmp)
  called <- called[names(called)!='x']
  
  out <- list(func = func, string = string, call = called)
  class(out) <- 'stuartDefaultObjective'
  return(out)
}
