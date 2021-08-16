defaultobjective <- function(
  criteria = c('rmsea', 'srmr', 'crel'), 
  add = c('chisq', 'df', 'pvalue'),
  scale = 1,
  ...) {
  
  # predefined sets for typical criteria
  predef <- c('^rel', '^crel', '^cfi', '^tli', '^nnfi', '^rfi',
    '^nfi', '^pnfi', '^ifi', '^rni', '^gfi', '^agfi', '^pgfi',
    '^mfi', '^ecvi', '^pvalue', '^chisq', '^aic', '^bic', '^bic2', '^rmsea', '^rmr', '^srmr',
    '^crmr', '^lvcor', '^beta', '^con')
  predef_check <- paste0(predef, collapse = '|')
  
  if (any(!grepl(predef_check, criteria))) {
    end_reason <- criteria[!grepl(predef_check, criteria)]
    stop(paste0('Not all criteria provided to the objective function have defaults. Problem with: ', paste(end_reason, collapse = ', '), '. Consider using these as auxiliary information via \"add\" instead.'))
  }
  
  defaults <- data.frame(criterion = predef, 
    side = c(rep('top', 16), rep('bottom', 8), rep('middle', 3)),
    m = c(.7, .8, .95, .95, .95, .95, .95, .6, .95, .95, .95, .95, .5,
      .95, .4, .05, 0, 0, 0, 0, .05, .05, .05, .05, 0, 0, .7),
    s = c(.1, .075, .03, .03, .03, .03, .03, .1, .03, .03, .03, .03, .12,
      .03, .1, .1, 10, 10, 10, 10, .015, .02, .015, .02, .2, .2, .2),
    scale = 1)

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
    if (cur_default$side == 'center') {
      string <- paste0(cur_default$scale, ' * 2 * ifelse(x > ', cur_default$m, ', pnorm(x, ', cur_default$m, ', ', cur_default$s, ', lower.tail = FALSE), pnorm(x, ', cur_default$m, ', ', cur_default$s, ', lower.tail = TRUE))')
    }
   
    string <- gsub('x', i, string)
    parsed <- parse(text = string)
    func <- function(x) eval(parsed)
    
    obj_list[[i]] <- list(func = func, string = string) 
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
