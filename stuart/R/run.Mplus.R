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
  utils::write.table(model.data,paste(filename,'_data.dat',sep=''),
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
    input <- paste(input,paste0('grouping = group (',paste(stats::na.omit(unique(model.data$group)),stats::na.omit(unique(model.data$group)),sep='=',collapse=' '),')'),';\n')
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

      #set latent means
      for (i in 1:length(factor.structure)) {
        if (long.invariance[[which(unlist(lapply(repeated.measures,function(x) is.element(names(factor.structure)[i],x))))]]%in%c('strong','strict')) {
          if (names(selected.items[i])%in%lapply(repeated.measures, function(x) x[1])) {
            input <- paste(input,
              paste0('[',names(selected.items[[i]]),'@0];',collapse='\n'),sep='\n')
          } else {
            input <- paste(input,
              paste0('[',names(selected.items[[i]]),'*];',collapse='\n'),sep='\n')
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
        #set latent means
        for (i in 1:length(factor.structure)) {
          if (long.invariance[[which(unlist(lapply(repeated.measures,function(x) is.element(names(factor.structure)[i],x))))]]%in%c('strong','strict')) {
            if (names(selected.items[i])%in%lapply(repeated.measures, function(x) x[1])&
                k==1) {
              input <- paste(input,
                paste0('[',names(selected.items[[i]]),'@0];',collapse='\n'),sep='\n')
            } else {
              input <- paste(input,
                paste0('[',names(selected.items[[i]]),'*];',collapse='\n'),sep='\n')
            }
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
    #write Mplus "savedata" section
    input <- paste(input,paste0('Savedata: estimates = ',filename,'_est.dat\n'),
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
  } else {
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
  }
  
  
  #return list of NA if errors occurred
  if (exclusion) {
    return(output=list(NA))
  } else {
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
    with_begin <- grep('^ +ESTIMATED COVARIANCE MATRIX FOR THE LATENT VARIABLES',MplusOut)
    if (as.numeric(gsub('[a-zA-Z ()]','',MplusOut[1]))>7) {
      with_end <- grep('^ +S.E. FOR ESTIMATED COVARIANCE MATRIX FOR THE LATENT VARIABLES',MplusOut)
      with <- data.frame(with_begin,with_end)
      with <- with[c(TRUE,!with_begin[-1]<with_end[-length(with_end)]),]
    } else {
      with_end <- grep('^ +ESTIMATES DERIVED FROM THE MODEL',MplusOut)[-1]
      with_end <- c(with_end,grep('^ +Beginning Time:',MplusOut))
      with <- matrix(c(with_begin[1],with_end[1]),ncol=2,nrow=length(with_end),byrow=TRUE)
      if (length(with_end)>1) {
        for (i in 2:length(with_end)) {
          with[i,] <- c(with_begin[which(with_begin>with_end[i-1])][1],with_end[i])
        } 
      }
    }
    
    lvcov <- apply(with,1,function(x) MplusOut[(x[1]+3):(x[2]-2)])
    size <- ifelse(any(lvcov==''),which(lvcov=='')[1]-1,1)
    tmp <- lapply(seq_len(max(ncol(lvcov),1)),function(x) lvcov[,x])
    if (size > 5) tmp <- lapply(tmp,function(y) y[-sapply(grep('_+',y),function(x) (x-3):x)])
    #tmp <- lapply(tmp,paste,collapse=' ')
    tmp <- lapply(tmp,function(x) gsub('[A-Z]','',x))
    tmp <- lapply(tmp,function(x) gsub(' [0-9+] ',' ',x))
    tmp <- lapply(tmp,function(x) gsub(' +',' ',x))
    tmp <- lapply(tmp,function(x) gsub('^ ','',x))
    tmp <- lapply(tmp,function(x) x[x!=''])

    tmp <- lapply(tmp,strsplit,' ')
    comA <- NULL
    comB <- NULL
    for (i in 0:(size%/%5)) {
      comA <- c(comA,(((i*5)+1):size))
      comB <- c(comB,rep(i,length((((i*5)+1):size)))*5+1)
    }
    
    psi <- list()
    
    for (i in 1:length(tmp)) {
      tmp[[i]] <- lapply(tmp[[i]],as.numeric)
      psi[[i]] <- matrix(1,ncol=size,nrow=size)
      for (j in 1:length(tmp[[i]])) {
        tmp2 <- tmp[[i]][[j]]
        psi[[i]][comA[j],comB[j]:(comB[j]+(length(tmp2)-1))] <- tmp[[i]][[j]]
      }
      psi[[i]][upper.tri(psi[[i]])] <- t(psi[[i]])[upper.tri(psi[[i]])]
    }
    
    for (i in 1:ncol(lvcov)) {
      dimnames(psi[[i]]) <- list(sapply(selected.items,names),sapply(selected.items,names))
    }
    
    output$lvcor <- lapply(psi,stats::cov2cor)
    
    
    # compute rho estimate of reliability
    tmp <- scan(paste0('./',filename,'_est.dat'),quiet=TRUE)
    
    nitems <- length(unlist(selected.items))
    nfacto <- length(selected.items)
    
    alpha <- list()
    lambda <- list()
    theta <- list()
    i <- 1
    repeat {
      alpha[[i]] <- tmp[1:nitems]
      tmp <- tmp[-seq_along(alpha[[i]])]
      lambda[[i]] <- matrix(tmp[1:(nitems*nfacto)],ncol=nfacto,nrow=nitems,byrow=TRUE)
      tmp <- tmp[-seq_along(lambda[[i]])]
      theta[[i]] <- matrix(NA,ncol=nitems,nrow=nitems)
      theta[[i]][upper.tri(theta[[i]],diag=TRUE)] <-  tmp[1:(nitems*(nitems+1)/2)]
      theta[[i]][lower.tri(theta[[i]])] <- t(theta[[i]])[lower.tri(theta[[i]])]
      tmp <- tmp[-(1:((nitems*(nitems+1)/2)+nfacto+nfacto^2+(nfacto*(nfacto+1)/2)))]
      
      dimnames(lambda[[i]]) <- list(unlist(selected.items),sapply(selected.items,names))
      
      if (length(tmp)==0) break
      i <- i + 1
    }
    
    rel <- lapply(lambda,function(x) rep(NA,ncol(x)))
    crel <- rep(NA,length(lambda))
    for (i in 1:length(rel)) {
      for (j in 1:length(rel[[i]])) {
        filter <- which(lambda[[i]][,j]!=0)
        rel[[i]][j] <- sum(lambda[[i]][,j,drop=FALSE]%*%psi[[i]][j,j,drop=FALSE]%*%t(lambda[[i]][,j,drop=FALSE]))/(sum(lambda[[i]][,j,drop=FALSE]%*%psi[[i]][j,j,drop=FALSE]%*%t(lambda[[i]][,j,drop=FALSE]))+sum(theta[[i]][filter,filter,drop=FALSE]))
      }
      # workaround for absence of short.factor.structure when crossvalidating
      if (class(try(short.factor.structure,silent=TRUE))=='try-error') {
        short.factor.structure <- as.list(rep(NA,ncol(lambda[[i]])))
        names(short.factor.structure) <- substr(colnames(lambda[[i]]),1,nchar(colnames(lambda[[i]]))-1)
      }
      reffilter <- substr(colnames(lambda[[i]]),1,nchar(colnames(lambda[[i]]))-1)%in%names(short.factor.structure)
      filter <- rowSums(lambda[[i]][,reffilter,drop=FALSE]!=0)>0
      
      crel[i] <- sum(lambda[[i]][,reffilter,drop=FALSE]%*%psi[[i]][reffilter,reffilter,drop=FALSE]%*%t(lambda[[i]][,reffilter,drop=FALSE]))/(sum(lambda[[i]][,reffilter,drop=FALSE]%*%psi[[i]][reffilter,reffilter,drop=FALSE]%*%t(lambda[[i]][,reffilter,drop=FALSE]))+sum(theta[[i]][filter,filter,drop=FALSE]))
    }
    crel <- mean(crel)
    output$rel <- rel
    output$crel <- crel

    
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

    return(output=output)
  }  
  
} #end function