run.Mplus <-
function(
  data, auxi, 
  number.of.items, number.of.subtests,
  selected, selected.items,
  long.equal, item.long.equal,
  factor.structure, repeated.measures, grouping,
  short.factor.structure,
  invariance, long.invariance, group.invariance,
  item.invariance, item.long.invariance, item.group.invariance,
  
  analysis.options=NULL, suppress.model=FALSE,
  
  output.model=FALSE,
  ignore.errors=FALSE,
  filename, cores
) { #begin function
  
  #prepare data for model fit
  model.data <- data[,unlist(selected.items)]
  model.data$group <- data[,grouping]
  model.data <- data.frame(model.data,auxi)
  
  #writing the data file
  write.table(model.data,paste(filename,'_data.dat',sep=''),
    col.names=FALSE,row.names=FALSE,na='-9999',
    sep='\t',dec='.')
  
  #define empty input
  input <- NULL

  #write Mplus "Title" section
  input <- paste0('Title: Subtest Construction using STUART \n',
    analysis.options[grepl('^titl*',names(analysis.options),ignore.case=TRUE)][[1]],'\n')
  
  #write Mplus "Data" section
  input <- paste0(input,'Data: file=',filename,'_data.dat; \n',
    analysis.options[grepl('^data*',names(analysis.options),ignore.case=TRUE)][[1]],'\n')
  
  #write Mplus "Variable" section
  input <- paste0(input,'Variable: \n\tnames=')
  
  input <- paste0(input,paste(names(model.data),collapse=c('\n\t\t')),';\n')
  
  input <- paste0(input,'\tmissing=ALL(-9999);\n',
    '\tusevariables=')
  
  input <- paste0(input,paste(unlist(selected.items),collapse=c('\n\t\t')),';\n')
  
  if (!is.null(grouping)) {
    input <- paste(input,paste0('grouping = group (',paste(unique(model.data$group),unique(model.data$group),sep='=',collapse=' '),')'),';\n')
  }
  
  input <- paste0(input,analysis.options[grepl('^vari*',names(analysis.options),ignore.case=TRUE)][[1]],'\n')
  
  #write Mplus "Analysis" section
  input <- paste0(input,'Analysis: \n\tprocessors=',cores,';\n')
  
  input <- paste0(input,analysis.options[grepl('^anal*',names(analysis.options),ignore.case=TRUE)][[1]],'\n')
  
  #write Mplus "Model" section
  input <- paste0(input,'Model:\n')
  
  #define mplus model syntax
  if (!suppress.model) {
    
    #write Mplus input (no groups)
    if (is.null(grouping)) {
      #write the (item) factor structure
      for (i in 1:length(selected.items)) { #over factors
        for (j in 1:length(selected.items[[i]])) { #over subtests
          #shorten the writing by creating tmp-data
          tmp.fil <- which(sapply(repeated.measures,is.element,el=names(selected.items)[i]))
          tmp.sel <- selected[[tmp.fil]][[j]]
          tmp.sit <- selected.items[[i]][[j]]
          
          #write the labels (no grouping)
          tmp.inv <- lapply(item.long.equal[[i]],function(x) return(x[tmp.sel]))
          
          #factor loadings
          input <- paste(input,'\n',
                         names(selected.items[[i]])[j],'by',
                         paste0(tmp.sit,' (',tmp.inv$lam,')',collapse='\n\t\t'),';\n')
          
          #residual variances
          input <- paste(input,
                         paste0(tmp.sit,' (',tmp.inv$eps,');',collapse='\n'),sep='\n')
          
          #intercepts
          input <- paste(input,
                         paste0('[',tmp.sit,'] (',tmp.inv$alp,');',collapse='\n'),'',sep='\n')
        }
      }
      
      #write the (subtest) factor structure
      for (i in 1:length(selected.items)) {
        if (number.of.subtests[sapply(repeated.measures,function(x) is.element(names(selected.items)[1], x))]>1) {
          tmp.fil <- which(sapply(repeated.measures,is.element,el=names(selected.items)[i]))
          tmp.sit <- names(selected.items[[i]])
          
          tmp.inv <- long.equal[[i]]
                    
          tmp.lin <- long.invariance[[tmp.fil]]
          
          #factor loadings
          input <- paste(input,'\n',
                         names(selected.items)[i],'by',
                         paste0(tmp.sit,' (',tmp.inv$lam,')',collapse='\n\t\t'),';\n')
          
          #residual variances
          input <- paste(input,
                         paste0(tmp.sit,' (',tmp.inv$eps,');',collapse='\n'),sep='\n')
          
          #intercepts
          #set latent means for all first occasion measures & if weak or less long inv.
          if (names(selected.items)[i]%in%names(short.factor.structure) | 
                tmp.lin%in%c('congeneric','weak')) {
            input <- paste(input,
                           paste0('[',tmp.sit,'@0];',collapse='\n'),sep='\n')
            input <- paste(input,
                           paste0('[',names(selected.items)[i],'@0];',collapse='\n'),'',sep='\n')
          }
          
          else {
            input <- paste(input,
                           paste0('[',tmp.sit,'] (',tmp.inv$alp,');',collapse='\n'),'',sep='\n')
            input <- paste(input,
                           paste0('[',names(selected.items)[i],'@0];',collapse='\n'),'',sep='\n')
          }
        }
      }
    }
    
    #write Mplus input (grouping)
    else {
      #write the (item) factor structure
      for (i in 1:length(selected.items)) { #over factors
        for (j in 1:length(selected.items[[i]])) { #over subtests
          
          #shorten the writing by creating tmp-data
          tmp.fil <- which(sapply(repeated.measures,is.element,el=names(selected.items)[i]))
          tmp.sel <- selected[[tmp.fil]][[j]]
          tmp.sit <- selected.items[[i]][[j]]
          
          #factor loadings (overall)
          input <- paste(input,'\n',
                         names(selected.items[[i]])[j],'by',
                         paste0(tmp.sit,collapse='\n\t\t'),';\n')
          
          #residual variances (overall)
          input <- paste(input,
                         paste0(tmp.sit,';',collapse='\n'),sep='\n')
          
          #intercepts (overall)
          input <- paste(input,
                         paste0('[',tmp.sit,'];',collapse='\n'),'',sep='\n')
        }
      }
          
      #write the (subtest) factor structure
      for (i in 1:length(selected.items)) {
        if (number.of.subtests[sapply(repeated.measures,function(x) is.element(names(selected.items)[1], x))]>1) {
          tmp.fil <- which(sapply(repeated.measures,is.element,el=names(selected.items)[i]))
          tmp.sit <- names(selected.items[[i]])
          
          tmp.inv <- long.equal[[i]]          
          tmp.lin <- long.invariance[[tmp.fil]]
          
          #factor loadings
          input <- paste(input,'\n',
                         names(selected.items)[i],'by',
                         paste0(tmp.sit,collapse='\n\t\t'),';\n')
          
          #residual variances
          input <- paste(input,
                         paste0(tmp.sit,';',collapse='\n'),sep='\n')
          
          #intercepts
          #set latent means for all first occasion measures & if weak or less long inv.
          if (names(selected.items)[i]%in%names(short.factor.structure) | 
                tmp.lin%in%c('congeneric','weak')) {
            input <- paste(input,
                           paste0('[',tmp.sit,'@0];',collapse='\n'),sep='\n')
            input <- paste(input,
                           paste0('[',names(selected.items)[i],'@0];',collapse='\n'),'',sep='\n')
          }
          
          else {
            input <- paste(input,
                           paste0('[',tmp.sit,'] (',tmp.inv$alp,');',collapse='\n'),'',sep='\n')
            input <- paste(input,
                           paste0('[',names(selected.items)[i],'@0];',collapse='\n'),'',sep='\n')
          }
        }
      }
      
      #group specific models
      for (k in 1:length(item.long.equal)) { #over groups

        #write grouping header
        input <- paste(input,'\n',
                       'Model',unique(model.data$group)[k],':\n')
        
        #write the (item) factor structure
        for (i in 1:length(selected.items)) { #over factors
          for (j in 1:length(selected.items[[i]])) { #over subtests
            #shorten the writing by creating tmp-data
            tmp.fil <- which(sapply(repeated.measures,is.element,el=names(selected.items)[i]))
            tmp.sel <- selected[[tmp.fil]][[j]]
            tmp.sit <- selected.items[[i]][[j]]

            tmp.inv <- lapply(item.long.equal[[k]][[i]],function(x) return(x[tmp.sel]))
            
            #factor loadings
            tmp.lam <- paste(names(selected.items[[i]])[j],'by',
                             paste0(tmp.sit[1],'@1\n\t\t'))
            tmp.lam <- paste(tmp.lam,
                             paste0(tmp.sit[-1],' (',tmp.inv$lam[-1],')',collapse='\n\t\t'),';\n')
            input <- paste(input,'\n',tmp.lam)
            
            #residual variances
            input <- paste(input,
                           paste0(tmp.sit,' (',tmp.inv$eps,');',collapse='\n'),sep='\n')
            
            #intercepts
            input <- paste(input,
                           paste0('[',tmp.sit,'] (',tmp.inv$alp,');',collapse='\n'),'',sep='\n')
          }
        }
        
        #write the (subtest) factor structure
        for (i in 1:length(selected.items)) {
          if (number.of.subtests[sapply(repeated.measures,function(x) is.element(names(selected.items)[1], x))]>1) {
            tmp.fil <- which(sapply(repeated.measures,is.element,el=names(selected.items)[i]))
            tmp.sit <- names(selected.items[[i]])
            
            tmp.inv <- long.equal[[k]][[i]]
            tmp.lin <- long.invariance[[tmp.fil]]
            
            #factor loadings
            input <- paste(input,'\n',
                           names(selected.items)[i],'by',
                           paste0(tmp.sit,' (',tmp.inv$lam,')',collapse='\n\t\t'),';\n')
            
            #residual variances
            input <- paste(input,
                           paste0(tmp.sit,' (',tmp.inv$eps,');',collapse='\n'),sep='\n')
            
            #intercepts
            #set latent means for all first occasion measures & if weak or less long inv.
            if (names(selected.items)[i]%in%names(short.factor.structure) | 
                  tmp.lin%in%c('congeneric','weak')) {
              input <- paste(input,
                             paste0('[',tmp.sit,'@0];',collapse='\n'),sep='\n')
              input <- paste(input,
                             paste0('[',names(selected.items)[i],'@0];',collapse='\n'),'',sep='\n')
            }
            
            else {
              input <- paste(input,
                             paste0('[',tmp.sit,'] (',tmp.inv$alp,');',collapse='\n'),'',sep='\n')
              input <- paste(input,
                             paste0('[',names(selected.items)[i],'@0];',collapse='\n'),'',sep='\n')
            }
          }
        }
      }          
    }
  }
  
  input <- paste0(input,analysis.options[grepl('^mode*',names(analysis.options),ignore.case=TRUE)][[1]],'\n')
  
  #write Mplus "Output" section
  if (!output.model) {
    input <- paste(input,'Output: STDYX NOSERROR\n',
                   analysis.options[grepl('^outp*',names(analysis.options),ignore.case=TRUE)][[1]],';\n')
  }
  
  else {
    input <- paste(input,'Output: STDYX \n',
                   analysis.options[grepl('^outp*',names(analysis.options),ignore.case=TRUE)][[1]],';\n')    
  }

  #create Mplus input file
  cat(input,file=paste0(filename,'.inp'))
  
  #run Mplus-Input (full mplus)
  if (Sys.info()[1]=='Windows') {
    system(paste('mplus ',filename,'.inp',sep=''),
      wait=TRUE,show.output.on.console=FALSE)
  }
  
  else {
    system(paste('mplus ',getwd(),'/',filename,'.inp ',getwd(),'/',filename,'.out',sep=''),
      wait=TRUE,ignore.stdout=TRUE)
  }
  
  #import Mplus output
  MplusOut <- readLines(paste0(filename,'.out'))
  
  if (!any(grepl('MODEL FIT',MplusOut))) {
    warning('The Mplus input file generated an error.\n',call.=FALSE)
    exclusion <- TRUE
  }
  
  #exclude non-positive and non-converged models
  #edit this for new Mplus versions!
  if (ignore.errors) {
    exclusion <- (any(grepl('NO CONVERGENCE',MplusOut))| 
        any(grepl('CHECK YOUR MODEL',MplusOut)))
  }
  
  else {
    exclusion <- (any(grepl('POSITIVE',MplusOut))|
        any(grepl('NO CONVERGENCE',MplusOut))| 
        any(grepl('CHECK YOUR MODEL',MplusOut)))
  }
  
  #return list of NA if errors occurred
  if (exclusion) {
    return(output=list(NA))
  }
  
  else {
    #extract the fit statistics reported by Mplus
    output <- list()
    
    locator <- c('^RMSEA','^SRMR',rep('CFI/TLI',2),
      rep('^Chi-Square Test of Model Fit$',3),
      rep('^Information Criteria',3))
    offset <- c(2,2,2,3,2,3,4,2,3,4)
    name <- c('rmsea','srmr','cfi','tli','chisq','df','pvalue','aic','bic','abic')
  
    for (i in 1:length(locator)) {
      tmp <- MplusOut[grep(locator[i],MplusOut)+offset[i]]
      output[name[i]] <- as.numeric(substr(tmp,nchar(tmp)-9,nchar(tmp)))
    }
    
    # compute Allen's composite reliability (overall, 1st occasion)
    tmp <- MplusOut[grep('^R-SQUARE',MplusOut):grep('^QUALITY OF NUMERICAL',MplusOut)]
    tmp <- gsub('\\s+',' ',tmp)
    tmp <- grep('\\.[0-9]{3}',tmp,value=TRUE)
    tmp <- as.numeric(sapply(strsplit(tmp,'\\s+'),rbind)[3,])
    rel <- suppressWarnings(data.frame(item=unlist(selected.items),rel=tmp))
    rel <- rel[rel$item%in%unlist(short.factor.structure),]
    rel <- aggregate(rel[,2],list(rel$item),mean)

    output$crel <- mean(sapply(rel$x, function(x) sum((x/(1-x)))/(1+sum((x/(1-x))))))

    # Export the latent variable correlation matrix
#    lvcor <- inspect(output,'cor.lv')
    
    return(output=output)
  }  
  
} #end function