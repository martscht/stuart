### Function definition ----
crossvalidate.lavaan <- 
function(
  selection, old.data, new.data, invariance, ...
) { #begin function
  
  # retrieve old results
  parameters <- lavaan::parTable(selection$final)

  if (!is.null(selection$call$grouping)) {
    if (!selection$call$grouping %in% names(new.data) | 
        !all(unique(new.data[,selection$call$grouping])%in%unique(old.data[,selection$call$grouping]))) {
      warning('The validation sample contained a value on the grouping variable not contained in the calibration sample.',
        'Parameters of the first group of the calibration sample will be used.')
      parameters <- parameters[parameters$group%in%c(0,1),]
      grouping <- NULL
    } else {
      shrd <- which(unique(na.omit(new.data[,selection$call$grouping]))%in%unique(na.omit(old.data[,selection$call$grouping])))
      parameters <- parameters[parameters$group%in%c(shrd,0),]
      grouping <- selection$call$grouping
      if (length(unique(new.data[,grouping]))==1) {
        grouping <- NULL
        parameters$group <- NULL
      }
    }
  } else {
    grouping  <- NULL
  }
  
  # select parameters to be constrained
  equality <- character()
  if (invariance%in%c('weak','strong','strict','full')) equality <- c(equality,'=~')
  if (invariance%in%c('strong','strict','full')) equality <- c(equality,'~1')
  if (invariance%in%c('strict','full')) equality <- c(equality,'~~')

  parameters$unco <- parameters$free
  parameters$free[parameters$op%in%equality] <- 0
  parameters$ustart[parameters$op%in%equality] <- parameters$est[parameters$op%in%equality]

  if (invariance!='full') {
    parameters$free[parameters$op%in%equality&parameters$label==''] <- 
      parameters$unco[parameters$op%in%equality&parameters$label=='']    
  }
  
  args <- list(data=new.data,selected.items=selection$subtests,
    grouping=grouping,auxi=new.data[,NULL],suppress.model=TRUE,
    analysis.options=list(model=parameters),ignore.errors=TRUE)
  
  output <- do.call('run.lavaan',args)
  
  return(output)
  
} #end function