run.Mplus <-
function(
  data, auxi, 
  number.of.items, number.of.subtests,
  selected, selected.items,
  long.equal, item.long.equal,
  factor.structure, repeated.measures, grouping,
  short.factor.structure, short, mtmm,
  invariance, long.invariance, mtmm.invariance, group.invariance,
  item.invariance, item.long.invariance, item.mtmm.invariance, item.group.invariance,
  
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
    unlist(analysis.options[grepl('^titl*',names(analysis.options),ignore.case=TRUE)][1]),'\n')
  
  #write Mplus "Data" section
  input <- paste0(input,'Data: file=',filename,'_data.dat; \n',
    unlist(analysis.options[grepl('^data*',names(analysis.options),ignore.case=TRUE)][1]),'\n')
  
  #write Mplus "Variable" section
  input <- paste0(input,'Variable: \n\tnames=')
  
  input <- paste0(input,paste(names(model.data),collapse=c('\n\t\t')),';\n')
  
  input <- paste0(input,'\tmissing=ALL(-9999);\n',
    '\tusevariables=')
  
  input <- paste0(input,paste(unlist(selected.items),collapse=c('\n\t\t')),';\n')
  
  if (!is.null(grouping)) {
    input <- paste(input,paste0('grouping = group (',paste(na.omit(unique(model.data$group)),na.omit(unique(model.data$group)),sep='=',collapse=' '),')'),';\n')
  }
  
  input <- paste0(input,unlist(analysis.options[grepl('^vari*',names(analysis.options),ignore.case=TRUE)][1]),'\n')
  
  #write Mplus "Analysis" section
  input <- paste0(input,'Analysis: \n\tprocessors=',cores,';\n')
  
  input <- paste0(input,unlist(analysis.options[grepl('^anal*',names(analysis.options),ignore.case=TRUE)][1]),'\n')
  
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
          tmp.fil <- which(unlist(lapply(short,
            function(x) is.element(names(factor.structure)[i],x))))
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
          
          #estimate latent regressions (MTMM)
          if (names(selected.items[i])%in%lapply(mtmm, function(x) x[1])) {
            tmp <- mtmm[[which(unlist(lapply(mtmm, function(x) x[1]))%in%names(selected.items[i]))]][-1]
            regs <- expand.grid(sapply(tmp,function(x) names(selected.items[[x]])),names(selected.items[[i]]))
            regs <- sapply(regs,as.character)
            
            if (is.null(nrow(regs))) {
              tmp <- paste0(paste(regs,collapse=' on '),';\n')
            } else {
              tmp <- paste0(paste(apply(regs,1,paste,collapse=' on '),collapse=';\n'),';\n')
            }
            
            input <- paste(input,tmp,sep='\n')
          }
        }
      }
      
      #write the (subtest) factor structure
      for (i in 1:length(selected.items)) {
        tmp.fil <- which(unlist(lapply(short,
          function(x) is.element(names(factor.structure)[i],x))))
        if (number.of.subtests[tmp.fil]>1) {
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
          input <- paste(input,
                         paste0('[',tmp.sit,'] (',tmp.inv$alp,');',collapse='\n'),'',sep='\n')
        
        }
      }

      #set latent means in longitudinal models
      for (i in 1:length(repeated.measures)) {
        if (long.invariance[[i]]%in%c('strong','strict')) {
          input <- paste(input,
            paste0('[',names(selected.items[[repeated.measures[[i]][1]]]),'@0];',collapse='\n'),sep='\n')
          input <- paste(input,
            paste0('[',names(selected.items[[repeated.measures[[i]][2:length(repeated.measures[[i]])]]]),'*];',collapse='\n'),sep='\n')
        }
      }
    }
    
    #write Mplus input (grouping)
    else {
      #write the (item) factor structure
      for (i in 1:length(selected.items)) { #over factors
        for (j in 1:length(selected.items[[i]])) { #over subtests
          
          #shorten the writing by creating tmp-data
          tmp.fil <- which(unlist(lapply(short,
            function(x) is.element(names(factor.structure)[i],x))))
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
        #estimate latent regressions (MTMM)
        if (names(selected.items[i])%in%lapply(mtmm, function(x) x[1])) {
          tmp <- mtmm[[which(unlist(lapply(mtmm, function(x) x[1]))%in%names(selected.items[i]))]][-1]
          regs <- expand.grid(sapply(tmp,function(x) names(selected.items[[x]])),names(selected.items[[i]]))
          regs <- sapply(regs,as.character)
          
          if (is.null(nrow(regs))) {
            tmp <- paste0(paste(regs,collapse=' on '),';\n')
          } else {
            tmp <- paste0(paste(apply(regs,1,paste,collapse=' on '),collapse=';\n'),';\n')
          }
          
          input <- paste(input,tmp,sep='\n')
        }
      }
          
      #write the (subtest) factor structure
      for (i in 1:length(selected.items)) {
        tmp.fil <- which(unlist(lapply(short,
          function(x) is.element(names(factor.structure)[i],x))))
        if (number.of.subtests[tmp.fil]>1) {
          tmp.sit <- names(selected.items[[i]])
          tmp.inv <- long.equal[[i]]

          #factor loadings
          input <- paste(input,'\n',
                         names(selected.items)[i],'by',
                         paste0(tmp.sit,collapse='\n\t\t'),';\n')
          
          #residual variances
          input <- paste(input,
                         paste0(tmp.sit,';',collapse='\n'),sep='\n')
          
          #intercepts
          input <- paste(input,
                         paste0('[',tmp.sit,'] (',tmp.inv$alp,');',collapse='\n'),'',sep='\n')
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
            tmp.fil <- which(unlist(lapply(short,
              function(x) is.element(names(factor.structure)[i],x))))
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
          tmp.fil <- which(unlist(lapply(short,
            function(x) is.element(names(factor.structure)[i],x))))
          if (number.of.subtests[tmp.fil]>1) {
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
            input <- paste(input,
                           paste0('[',tmp.sit,'] (',tmp.inv$alp,');',collapse='\n'),'',sep='\n')
          }
        }
        
        #set latent means
        for (i in 1:length(repeated.measures)) {
          if (long.invariance[[i]]%in%c('strong','strict') & k==1) {
            input <- paste(input,
              paste0('[',names(selected.items[[repeated.measures[[i]][1]]]),'@0];',collapse='\n'),sep='\n')
              input <- paste(input,
                paste0('[',names(selected.items[[repeated.measures[[i]][2:length(repeated.measures[[i]])]]]),'*];',collapse='\n'),sep='\n')
          }
          if (long.invariance[[i]]%in%c('strong','strict') & k!=1) {
            input <- paste(input,
              paste0('[',names(selected.items[[repeated.measures[[i]][1]]]),'*];',collapse='\n'),sep='\n')
            input <- paste(input,
              paste0('[',names(selected.items[[repeated.measures[[i]][2:length(repeated.measures[[i]])]]]),'*];',collapse='\n'),sep='\n')
          }
        } 
      }          
    }
  }
  
  input <- paste0(input,unlist(analysis.options[grepl('^mode*',names(analysis.options),ignore.case=TRUE)][1]),'\n')
  
  #write Mplus "Output" section
  if (!output.model) {
    input <- paste(input,'Output: STDYX Tech4 NOSERROR\n',
                   unlist(analysis.options[grepl('^outp*',names(analysis.options),ignore.case=TRUE)][1]),';\n')
  }
  
  else {
    input <- paste(input,'Output: STDYX Tech4 \n',
                   unlist(analysis.options[grepl('^outp*',names(analysis.options),ignore.case=TRUE)][1]),';\n')
  }

  #create Mplus input file
  cat(input,file=paste0(filename,'.inp'))
  
  #run Mplus-Input (on windows)
  if (Sys.info()[1]=='Windows') {
    system(paste0('mplus ',filename,'.inp'),
      wait=TRUE,show.output.on.console=FALSE)
  }
  #run Mplus-Input (on linux)
  if (Sys.info()[1]=='Linux') {
    #replace spaces in directories
    working <- gsub(' ','\\ ',getwd())
    
    system(paste0('mplus ',working,'/',filename,'.inp ',working,'/',filename,'.out'),
      wait=TRUE,ignore.stdout=TRUE)
  }
  #run Mplus-Input (on osx)
  if (Sys.info()[1]=='Darwin') {
    #replace spaces in directories
    working <- gsub(' ','\\ ',getwd())
    
    system(paste0('/Applications/Mplus/mplus ',working,'/',filename,'.inp ',working,'/',filename,'.out'),
      wait=TRUE,ignore.stdout=TRUE)
  }
  
  #import Mplus output
  MplusOut <- readLines(paste0(filename,'.out'))
  
  if (output.model) return(MplusOut)
  
  if (!any(grepl('MODEL FIT',MplusOut))) {
    if (any(grepl('NO CONVERGENCE.',MplusOut))) {
      warning('The model did not converge.',call.=FALSE)
    } else {
      warning('The Mplus input file generated an error.',call.=FALSE)
    }
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
    
    #extract latent correlations
    with_begin <- grep('^ +ESTIMATED CORRELATION MATRIX FOR THE LATENT VARIABLES',MplusOut)
    with_end <- grep('^ +S.E. FOR ESTIMATED CORRELATION MATRIX FOR THE LATENT VARIABLES',MplusOut)
    with <- cbind(with_begin,with_end)
    
    lvcor <- apply(with,1,function(x) MplusOut[(x[1]+3):(x[2]-3)])
    tmp <- lapply(seq_len(ncol(lvcor)),function(x) lvcor[,x])
    tmp <- lapply(tmp,paste,collapse=' ')
    tmp <- lapply(tmp,function(x) gsub('[A-Z]','',x))
    tmp <- lapply(tmp,function(x) gsub(' +',' ',x))
    tmp <- lapply(tmp,function(x) gsub('^ ','',x))
    matrices <- list()
    for (i in 1:ncol(lvcor)) {
      matrices[[i]] <- matrix(1,ncol=length(lvcor[,i]),nrow=length(lvcor[,i]))
      matrices[[i]][lower.tri(matrices[[i]],diag=TRUE)] <- as.numeric(unlist(strsplit(tmp[[i]],' '))) 
      matrices[[i]][upper.tri(matrices[[i]],diag=TRUE)] <- as.numeric(unlist(strsplit(tmp[[i]],' ')))
    }
    
    tmp <- lapply(seq_len(ncol(lvcor)),function(x) lvcor[,x])
    tmp <- lapply(tmp,paste,collapse=' ')
    tmp <- lapply(tmp,function(x) gsub('[0-9.]','',x))
    tmp <- lapply(tmp,function(x) gsub(' +',' ',x))
    tmp <- lapply(tmp,function(x) gsub('^ ','',x))

    for (i in 1:ncol(lvcor)) {
      dimnames(matrices[[i]]) <- list(unlist(strsplit(tmp[[i]],' ')),unlist(strsplit(tmp[[i]],' ')))
    }
    output$lvcor <- matrices
    
    # compute Allen's composite reliability (overall, 1st occasion)
    tmp <- MplusOut[grep('^R-SQUARE',MplusOut):grep('^QUALITY OF NUMERICAL',MplusOut)]
    if (any(grepl('Latent',tmp))) {
      con <- tmp[grep('Latent',tmp)[1]:length(tmp)]
      con <- gsub('\\s+',' ',con)
      con <- grep('\\.[0-9]{3}',con,value=TRUE)
      con <- con[!grepl('Undefined',con)]
      con <- as.numeric(sapply(strsplit(con,'\\s+'),rbind)[3,1:length(con)])
      con <- mean(con)
      
      output$con <- con
      
      tmp <- tmp[1:grep('Latent',tmp)[1]]
    }
    tmp <- gsub('\\s+',' ',tmp)
    tmp <- grep('\\.[0-9]{3}',tmp,value=TRUE)
    tmp <- tmp[!grepl('Undefined',tmp)]
    rel <- as.numeric(sapply(strsplit(tmp,'\\s+'),rbind)[3,1:length(tmp)])
    item <- sapply(strsplit(tmp,'\\s+'),rbind)[2,1:length(tmp)]
    rel <- suppressWarnings(data.frame(item=item,rel=rel))
    rel <- aggregate(rel[,2],list(rel$item),mean)
    output$crel <- sum((rel$x/(1-rel$x)))/(1+sum((rel$x/(1-rel$x))))
  
    return(output=output)
  }  
  
} #end function