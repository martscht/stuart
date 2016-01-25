### Function definition ----
crossvalidate.Mplus <-
function(
  selection,
  new.data, old.data,
  invariance, filename, file.remove, ...
) { # begin function
  
  model <- selection$final
  model.begin <- grep('^\\s+Model:',model)
  model.end <- grep('^\\s+Output:',model) - 1
  
  model <- paste(model[model.begin:model.end],collapse='\n')
  out <- 'svalues'
  
  grouping <- selection$call$grouping
  
  args <- list(data=old.data,selected.items=selection$subtests,
    grouping=grouping,auxi=old.data[,NULL],suppress.model=TRUE,
    output.model=TRUE,
    filename=paste0(filename,'_calibration'),cores=NULL,
    analysis.options=list(model=model,output=out))
  
  calib <- do.call('run.Mplus',args)  
  
  model <- calib[(grep('USED AS STARTING VALUES',calib)+1):(grep('^TECHNICAL',calib)[1]-1)]
  
  # select parameters to be constrained
  equality <- character()
  if (invariance%in%c('weak','strong','strict')) equality <- c(equality,'(lam[0-9]+)')
  if (invariance%in%c('strong','strict')) equality <- c(equality,'(alp[0-9]+)')
  if (invariance%in%c('strict')) equality <- c(equality,'(eps[0-9]+)')
  
  filter <- grepl(paste(equality,collapse='|'),model)
  
  if (length(equality)>0 | invariance=='full') model[filter] <- gsub('\\*','@',model[filter])
  
  if (!is.null(grouping)) {
    tmp <- c(1,
      sapply(sort(na.omit(unique(old.data[,grouping]))), function(x) grep(paste0('MODEL\\s+',x,':'),model)),
      length(model))
    group.models <- list()
    for (i in 2:(length(tmp)-1)) {
      group.models[[i-1]] <- model[tmp[i]:(tmp[i+1]-1)]
    }
    names(group.models) <- sort(na.omit(unique(old.data[,grouping])))
    overall.model <-  model[tmp[1]:(tmp[2]-1)]
    
    if (!grouping %in% names(new.data) | 
        !all(unique(new.data[,grouping])%in%unique(old.data[,grouping]))) {
      warning('The validation sample contained a value on the grouping variable not contained in the calibration sample.',
              'Parameters of the first group of the calibration sample will be used.')
      model <-  paste(group.models[[1]][-1],collapse='\n')
      grouping <- NULL
    } else {
      if (length(unique(new.data[,grouping]))==1) {
        model <- paste(sapply(group.models[[unique(new.data[,grouping])]][-1],paste,collapse='\n'),collapse='\n')
        grouping <- NULL
      } else {
        model <- paste(c(paste(overall.model,collapse='\n'),
          sapply(group.models[names(group.models)%in%sort(na.omit(unique(new.data[,grouping])))],paste,collapse='\n')),collapse='\n')
      }
    }
  } else {
    grouping  <- NULL
  }
  
  args$filename <- paste0(filename,'_validation')
  args$data <- new.data
  args$auxi <- new.data[,NULL]
  args$analysis.options <- list(model=paste(model,collapse='\n'))
  args$output.model <- FALSE
  args['grouping'] <- list(grouping)
  args$ignore.errors <- TRUE

  output <- do.call('run.Mplus',args)
  
  if (file.remove) {
    file.remove(paste0(filename,'_calibration',c('.inp','.out')))
    file.remove(paste0(filename,'_validation',c('.inp','.out')))
    file.remove(paste0(filename,c('_calibration','_validation'),'_data.dat'))
  }
  
  return(output)
  
} # end function