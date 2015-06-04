### Function definition ----
crossvalidate.lavaan <- 
function(
  selection, new.data, invariance, ...
) { #begin function
  
  # retrieve old results
  parameters <- parTable(selection$FinalModel)
  parameters$old <- parameterEstimates(selection$FinalModel)$est
  
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
    grouping=NULL,auxi=new.data[,NULL],suppress.model=TRUE,
    analysis.options=list(model=parameters))
  
  output <- do.call('run.lavaan',args)
  
  return(output)
  
} #end function