### Function definition ----
crossvalidate.lavaan <- 
function(
  selection, new.data, invariance, ...
) { #begin function
  
  # retrieve old results
  parameters <- parTable(selection$FinalModel)
  parameters <- parameters[parameters$group!=0,]
  parameters$old <- parameterEstimates(selection$FinalModel)$est
  
  
  if (!is.null(selection$Call$grouping)) {
    if (!selection$Call$grouping %in% names(new.data) | 
        !all(unique(new.data[,selection$Call$grouping])%in%unique(parameters$group))) {
      warning('The validation sample contained a value on the grouping variable not contained in the calibration sample.',
        'Parameters of the first group of the calibration sample will be used.')
      parameters <- parameters[parameters$group==1,]
      grouping <- NULL
    } else {
      parameters <- parameters[parameters$group%in%unique(new.data[,selection$Call$grouping]),]
      grouping <- selection$Call$grouping
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

  parameters$free[parameters$op%in%equality] <- 0
  parameters$ustart[parameters$op%in%equality] <- parameters$old[parameters$op%in%equality]

  if (invariance!='full') {
    parameters$free[parameters$op%in%equality&parameters$label==''] <- 
      parameters$unco[parameters$op%in%equality&parameters$label=='']    
  }
  
  args <- list(data=new.data,selected.items=selection$Subtests,
    grouping=grouping,auxi=new.data[,NULL],suppress.model=TRUE,
    analysis.options=list(model=parameters))
  
  output <- do.call('run.lavaan',args)
  
  return(output)
  
} #end function