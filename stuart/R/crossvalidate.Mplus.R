### Function definition ----
crossvalidate.Mplus <-
function(
  selection,
  new.data, old.data,
  invariance, filename, ...
) { # begin function
  
  model <- selection$FinalModel
  model.begin <- grep('^\\s+Model:',model)
  model.end <- grep('^\\s+Output:',model) - 2
  
  model <- paste(model[model.begin:model.end],collapse='\n')
  out <- 'svalues'
  
  args <- list(data=old.data,selected.items=selection$Subtests,
    grouping=NULL,auxi=old.data[,NULL],suppress.model=TRUE,
    output.model=TRUE,
    filename=paste0(filename,'_calibration'),cores=NULL,
    analysis.options=list(model=model,output=out))
  
  calib <- do.call('run.Mplus',args)  
  
  model <- calib[(grep('USED AS STARTING VALUES',calib)+1):(grep('^\\s+Beginning Time',calib)-1)]
  
  # select parameters to be constrained
  equality <- character()
  if (invariance%in%c('weak','strong','strict')) equality <- c(equality,'(lam[0-9]+)')
  if (invariance%in%c('strong','strict')) equality <- c(equality,'(alp[0-9]+)')
  if (invariance%in%c('strict')) equality <- c(equality,'(eps[0-9]+)')
  
  filter <- grepl(paste(equality,collapse='|'),model)
  
  if (length(equality)>0 | invariance=='full') model[filter] <- gsub('\\*','@',model[filter])
  
  args$filename <- paste0(filename,'_validation')
  args$data <- new.data
  args$auxi <- new.data[,NULL]
  args$analysis.options <- list(model=paste(model,collapse='\n'))
  args$output.model <- FALSE

  output <- do.call('run.Mplus',args)  
  
  return(output)
  
} # end function