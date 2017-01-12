run.lavaan <-
function(
  data, auxi, 
  number.of.items, number.of.subtests,
  selected, selected.items,
  long.equal, item.long.equal,
  factor.structure, repeated.measures, grouping,
  short.factor.structure, short, mtmm=NULL,
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
      tmp.fil <- which(unlist(lapply(short,
        function(x) is.element(names(factor.structure)[i],x))))
      
      tmp.sit <- names(selected.items[[i]])
      tmp.lin <- long.invariance[[tmp.fil]]
      
      if (number.of.subtests[sapply(repeated.measures,function(x) is.element(names(selected.items)[1], x))]>1) {
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
      } else {
        #intercepts
        #set latent means for all first occasion measures & if weak or less long inv.
        if (names(selected.items)[i]%in%names(short.factor.structure) | 
            tmp.lin%in%c('congeneric','weak')) {
          input <- paste(input,
            paste(tmp.sit,'~','0','*1',sep='',collapse='\n'),sep='\n')
        }
        
        else {
          input <- paste(input,
            paste(tmp.sit,'~1',sep='',collapse='\n'),sep='\n')
        }
      }
    }
  }

  if (is.data.frame(analysis.options$model)) {
    if (suppress.model) input <- analysis.options$model
    else input <- rbind(lavaan::lavParTable(input),analysis.options$model)
  }
  else input <- paste(input,analysis.options$model,sep='\n')
  
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
  
  #imply sem() presets
  presets <- list(int.ov.free=TRUE,int.lv.free=FALSE,auto.fix.first=TRUE,std.lv=FALSE,
    auto.fix.single=TRUE,auto.var=TRUE,auto.cov.lv.x=TRUE,auto.th=TRUE,auto.delta=TRUE,auto.cov.y=TRUE)
  for (i in names(presets)) {
    if (!i %in% names(analysis.options)) analysis.options[i] <- presets[i]
  }
  
  #retain only the options that are accepted by lavaan
  analysis.options <- analysis.options[!sapply(analysis.options,is.null)]
  analysis.options <- analysis.options[is.element(names(analysis.options),names(formals(lavaan::lavaan)))]
  
  # tmp.cfa <- get('cfa',asNamespace('lavaan'))  
  output <- try(suppressWarnings(do.call(lavaan::lavaan,analysis.options)),silent=TRUE)

  if (class(output)=='try-error') {
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
        for (i in 1:length(lavaan::inspect(output,'cov.lv'))) {
          pd_psi[i] <- all(eigen(lavaan::inspect(output,'cov.lv')[[i]],TRUE,TRUE)$values>0) 
          pd_the[i] <- all(diag(lavaan::inspect(output,'theta')[[i]])>=0)
        }
        pd_psi <- all(pd_psi)
        pd_the <- all(pd_the)
      } else {
        pd_psi <- all(eigen(lavaan::inspect(output,'cov.lv'),TRUE,TRUE)$values>0)
        pd_the <- all(diag(lavaan::inspect(output,'theta'))>=0)
      }
      
      #return only NA if Psi or Theta are Not Positive Definite      
      if (!pd_psi | !pd_the) {
        return(output=list(NA)) 
      }
    }

    fit <- try(suppressWarnings(lavaan::inspect(output,'fit')),silent=TRUE)
    
    if (class(fit)[1]=='try-error') {
      return(output=list(NA))
      warning('The lavaan estimation generated an error, most likely non-convergence.')
    } else {
      # compute composite reliability (overall)
      if (is.null(grouping)) {
        tmp <- lavaan::inspect(output,'est')
        theta <- tmp$theta
        psi <- tmp$psi
        lambda <- tmp$lambda
        alpha <- tmp$alpha

        rel <- rep(NA,ncol(lambda))        
        for (i in 1:ncol(lambda)) {
          filter <- which(lambda[,i]!=0)
          rel[i] <- sum(lambda[,i,drop=FALSE]%*%psi[i,i,drop=FALSE]%*%t(lambda[,i,drop=FALSE]))/(sum(lambda[,i,drop=FALSE]%*%psi[i,i,drop=FALSE]%*%t(lambda[,i,drop=FALSE]))+sum(theta[filter,filter,drop=FALSE]))
        }
        # workaround for absence of short.factor.structure when crossvalidating
        if (class(try(short.factor.structure,silent=TRUE))=='try-error') {
          short.factor.structure <- as.list(rep(NA,ncol(lambda)))
          names(short.factor.structure) <- substr(colnames(lambda),1,nchar(colnames(lambda))-1)
        }
        reffilter <- substr(colnames(lambda),1,nchar(colnames(lambda))-1)%in%names(short.factor.structure)
        filter <- rowSums(lambda[,reffilter,drop=FALSE]!=0)>0
        
        crel <- sum(lambda[,reffilter,drop=FALSE]%*%psi[reffilter,reffilter,drop=FALSE]%*%t(lambda[,reffilter,drop=FALSE]))/(sum(lambda[,reffilter,drop=FALSE]%*%psi[reffilter,reffilter,drop=FALSE]%*%t(lambda[,reffilter,drop=FALSE]))+sum(theta[filter,filter,drop=FALSE]))
        
        tmp <- lavaan::inspect(output,'rsquare')
        con <- mean(tmp[!names(tmp)%in%names(model.data)])
          
      } else {
        tmp <- lavaan::inspect(output,'est')
        theta <- lapply(tmp,function(x) x$theta)
        psi <- lapply(tmp,function(x) x$psi)
        lambda <- lapply(tmp,function(x) x$lambda)
        alpha <- lapply(tmp,function(x) x$alpha)
        
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
        
        tmp <- lavaan::inspect(output,'rsquare')
        con <- mean(sapply(tmp,function(x) mean(x[!names(x)%in%names(model.data)])))
      }
      
      
      # Export the latent variable correlation matrix
      lvcor <- lavaan::inspect(output,'cor.lv')

      output <- as.list(fit)
      output$crel <- crel
      output$rel <- rel
      output$lvcor <- lvcor
      output$lambda <- lambda
      output$theta <- theta
      output$psi <- psi
      output$alpha <- alpha
      if (!is.na(con)) output$con <- con

      return(output=output)
    }
  }

}
