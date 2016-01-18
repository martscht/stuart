run.lavaan <-
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
  ignore.errors=FALSE
) { #begin function

  #prepare data for model fit
  model.data <- data[,unlist(selected.items)]
  model.data$group <- data[,grouping]
  model.data <- data.frame(model.data,auxi)

  #define empty lavaan input
  input <- NULL

  if (!suppress.model) {
    #write the (item) factor structure
    for (i in 1:length(selected.items)) { #over factors
      for (j in 1:length(selected.items[[i]])) { #over subtests
        #shorten the writing by creating tmp-data
        tmp.fil <- which(unlist(lapply(short,
          function(x) is.element(names(factor.structure)[i],x))))
        tmp.sel <- selected[[tmp.fil]][[j]]
        tmp.sit <- selected.items[[i]][[j]]

        #write the labels (no grouping)
        if (is.null(grouping)) {
          tmp.inv <- lapply(item.long.equal[[i]],function(x) return(x[tmp.sel]))
        }

        #write the labels (grouping)
        else {
          tmp.inv <- list(NA)
          for (k in 1:length(item.long.equal)) { #over groups
            tmp.inv[[k]] <- lapply(item.long.equal[[k]][[i]],function(x) return(x[tmp.sel]))
            tmp.inv[[k]] <- unlist(tmp.inv[[k]])
          }
          tmp.inv <- data.frame(lapply(tmp.inv,data.frame))
          tmp.inv <- apply(tmp.inv,1,paste,collapse=',')
          tmp.inv <- paste('c(',tmp.inv,')',sep='')
          tmp.inv <- list(lam=tmp.inv[1:number.of.items[[tmp.fil]][j]],
            alp=tmp.inv[(number.of.items[[tmp.fil]][j]+1):(number.of.items[[tmp.fil]][j]*2)],
            eps=tmp.inv[(number.of.items[[tmp.fil]][j]*2+1):(number.of.items[[tmp.fil]][j]*3)])
        }

        #factor loadings
        input <- paste(input,'\n',
          names(selected.items[[i]])[j],'=~',
          paste(tmp.inv$lam,'*',tmp.sit,sep='',collapse=' + '),sep='')

        #residual variances
        input <- paste(input,
          paste(tmp.sit,'~~',tmp.inv$eps,'*',tmp.sit,sep='',collapse='\n'),sep='\n')

        #intercepts
        input <- paste(input,
          paste(tmp.sit,'~',tmp.inv$alp,'*1',sep='',collapse='\n'),sep='\n')
        
      }
      
      #supress correlations between traits and methods (for CTC(M-1) structure)
#       if (names(selected.items[i])%in%lapply(mtmm, function(x) x[1])) {
#         tmp <- mtmm[[which(unlist(lapply(mtmm, function(x) x[1]))%in%names(selected.items[i]))]][-1]
#         tmp <- outer(names(selected.items[[i]]),sapply(tmp,function(x) names(selected.items[[x]])),
#           paste,sep=' ~~ 0*')
#         tmp <- paste(tmp,collapse='\n')
#         input <- paste(input,tmp,sep='\n')
#       }

      #estimate latent regressions (MTMM)
      if (names(selected.items[i])%in%lapply(mtmm, function(x) x[1])) {
        tmp <- mtmm[[which(unlist(lapply(mtmm, function(x) x[1]))%in%names(selected.items[i]))]][-1]
        regs <- expand.grid(sapply(tmp,function(x) names(selected.items[[x]])),names(selected.items[[i]]))
        regs <- sapply(regs,as.character)
        
        if (is.null(nrow(regs))) {
          tmp <- paste(regs,collapse='~')
        } else {
          tmp <- paste(apply(regs,1,paste,collapse='~'),collapse='\n')
        }
        
        input <- paste(input,tmp,sep='\n')
      }
    }

    #write the (subtest) factor structure
    for (i in 1:length(selected.items)) {
      if (number.of.subtests[sapply(repeated.measures,function(x) is.element(names(selected.items)[1], x))]>1) {
        tmp.fil <- which(unlist(lapply(short,
          function(x) is.element(names(factor.structure)[i],x))))
        
        tmp.sit <- names(selected.items[[i]])

          if (is.null(grouping)) {
            tmp.inv <- long.equal[[i]]
          }

          #write the labels (grouping)
          else {
            tmp.inv <- list(NA)
            for (k in 1:length(long.equal)) { #over groups
              tmp.inv[[k]] <- long.equal[[k]][[i]]
              tmp.inv[[k]] <- unlist(tmp.inv[[k]])
            }
            tmp.inv <- data.frame(lapply(tmp.inv,data.frame))
            tmp.inv <- apply(tmp.inv,1,paste,collapse=',')
            tmp.inv <- paste('c(',tmp.inv,')',sep='')
            tmp.inv <- list(lam=tmp.inv[1:number.of.subtests[[tmp.fil]]],
              alp=tmp.inv[(number.of.subtests[[tmp.fil]]+1):(number.of.subtests[[tmp.fil]]*2)],
              eps=tmp.inv[(number.of.subtests[[tmp.fil]]*2+1):(number.of.subtests[[tmp.fil]]*3)])
          }

          tmp.lin <- long.invariance[[tmp.fil]]

        #factor loadings
        input <- paste(input,'\n',
          names(selected.items)[i],'=~',
          paste(tmp.inv$lam,'*',tmp.sit,sep='',collapse=' + '),sep='')

        #residual variances
        input <- paste(input,
          paste(tmp.sit,'~~',tmp.inv$eps,'*',tmp.sit,sep='',collapse='\n'),sep='\n')

        #intercepts
        #set latent means for all first occasion measures & if weak or less long inv.
        if (names(selected.items)[i]%in%names(short.factor.structure) | 
          tmp.lin%in%c('congeneric','weak')) {
          input <- paste(input,
            paste(tmp.sit,'~','0','*1',sep='',collapse='\n'),sep='\n')
          input <- paste(input,
            paste(names(selected.items)[i],'~','0','*1',sep='',collapse='\n'),sep='\n')
        }

        else {
          input <- paste(input,
            paste(tmp.sit,'~',tmp.inv$alp,'*1',sep='',collapse='\n'),sep='\n')
          input <- paste(input,
            paste(names(selected.items)[i],'~','0','*1',sep='',collapse='\n'),sep='\n')
        }
      }
    }
  }

  if (is.data.frame(analysis.options$model)) {
    if (suppress.model) input <- analysis.options$model
    else input <- rbind(lavParTable(input),analysis.options$model)
  }
  else input <- paste(input,analysis.options$model)
  
  #list of arguments to pass to lavaan
  if (is.null(analysis.options)) {
    analysis.options <- list(NULL) 
  }
    
  analysis.options$model <- input
  analysis.options$data <- model.data

  if (!is.null(grouping)) {
    analysis.options$group <- 'group'
  }
  if (is.null(analysis.options$missing)) {
    analysis.options$missing <- 'fiml'
  }

  if (!output.model) {
    analysis.options$se <- 'none'
  }

  
  #retain only the options that are accepted by lavaan
  analysis.options <- analysis.options[!sapply(analysis.options,is.null)]
  analysis.options <- analysis.options[is.element(names(analysis.options),names(formals(lavaan)))]
  
  tmp.cfa <- get('cfa',asNamespace('lavaan'))  
  output <- try(suppressWarnings(do.call('tmp.cfa',analysis.options)),silent=TRUE)

  if (class(output)=='try.error') {
    warning('The lavaan input generated an error.',call.=FALSE)
    return(output=list(NA))
  }

  if (output.model & class(output)=='lavaan') {
    return(output=output)
  }
  
  if (!output.model & class(output)=='lavaan') {

    if (!ignore.errors) {
      #check if psi is positive definite
      pd_psi <- NULL
      pd_the <- NULL
      
      if (!is.null(grouping)) {
        for (i in 1:length(inspect(output,'cov.lv'))) {
          pd_psi[i] <- all(eigen(inspect(output,'cov.lv')[[i]])$values>0) 
          pd_the[i] <- all(diag(inspect(output,'theta')[[i]])>=0)
        }
        pd_psi <- all(pd_psi)
        pd_the <- all(pd_the)
      } else {
        pd_psi <- all(eigen(inspect(output,'cov.lv'))$values>0)
        pd_the <- all(diag(inspect(output,'theta'))>=0)
      }
      
      #return only NA if Psi or Theta are Not Positive Definite      
      if (!pd_psi | !pd_the) {
        return(output=list(NA)) 
      }
    }

    fit <- try(suppressWarnings(inspect(output,'fit')),silent=TRUE)
    if (class(fit)[1]=='try-error') {
      return(output=list(NA))
      warning('The lavaan estimation generated an error, most likely non-convergence.')
    } else {
      # compute Allen's composite reliability (overall)
      if (is.null(grouping)) {
        theta <- diag(inspect(output,'theta'))
        sigma <- diag(inspect(output,'sigma'))
        
        rel <- 1-(theta/sigma)
        rel[theta<0] <- 0
        crel <- sum((rel/(1-rel)))/(1+sum((rel/(1-rel))))
        
        tmp <- inspect(output,'rsquare')
        con <- mean(tmp[!names(tmp)%in%names(model.data)])
          
      } else {
        theta <- lapply(inspect(output,'theta'),diag)
        sigma <- lapply(inspect(output,'sigma'),diag)
        
        rel <- as.list(rep(NA,length(theta)))
        for (i in 1:length(theta)) {
          rel[[i]] <- 1-(theta[[i]]/sigma[[i]])
          rel[[i]][theta[[i]]<0] <- 0
          tmp <- inspect(output,'rsquare')
          con <- mean(sapply(tmp,function(x) mean(x[!names(x)%in%names(model.data)])))
        }
        crel <- mean(sapply(rel, function(x) sum((x/(1-x)))/(1+sum((x/(1-x))))))
      }
      
      
      # Export the latent variable correlation matrix
      lvcor <- inspect(output,'cor.lv')

      output <- as.list(fit)
      output$crel <- crel
      output$lvcor <- lvcor
      if (!is.null(mtmm)) output$con <- con

      return(output=output)
    }
  }

}
