stuart.mmas <-
function(
  short.factor.structure, short, long.equal, item.long.equal,    #made on toplevel
  number.of.items,
  data, factor.structure, auxi, use.order,                       #simple prerequisites
  
  number.of.subtests,  invariance,                               #subtest relations
  repeated.measures, long.invariance,                            #longitudinal relations
  mtmm, mtmm.invariance,                                         #mtmm relations
  grouping, group.invariance,                                    #grouping relations

  item.invariance, item.long.invariance, item.mtmm.invariance,
  item.group.invariance,


  software, cores,                                               #Software to be used

  fitness.func=NULL, ignore.errors=FALSE,                        #fitness function
  
  ants=16, colonies=256, evaporation=.95,                        #general ACO parameters
  deposit='ib', pbest=.005, deposit.on='nodes',                  #MMAS parameters
  alpha=1, beta=1, pheromones=NULL, heuristics=NULL,
  tolerance=.001, schedule='run',                                #tolerance for convergence

  suppress.model=FALSE, analysis.options=NULL,                   #Additional modeling
  filename,

  ...                                                            #All the other stuff
) { #function begin

  #initialize fitness results of best solutions
  phe.ib <- 0
  phe.gb <- 0
  
  #initialize upper and lower limits
  phe.max <- 0
  phe.min <- 0
  
  #counting
  run <- 1
  colony <- 1

  #compute number of decisions and avg for limits
  deci <- sum(unlist(number.of.items))
  tmp <- list(NA)
  for (i in 1:length(short.factor.structure)) {
    tmp[[i]] <- length(short.factor.structure[[i]]):(length(short.factor.structure[[i]])-sum(number.of.items[[i]]))
  }
  avg <- mean(unlist(tmp))
  
  #initialize scheduling 
  scheduled <- c('ants','colonies','evaporation','pbest','alpha','beta','tolerance')
  
  #global assignment to avoid check note
  ants_cur <- NA
  colonies_cur <- NA
  evaporation_cur <- NA
  pbest_cur <- NA
  alpha_cur <- NA
  beta_cur <- NA
  tolerance_cur <- NA
  
  filt <- sapply(mget(scheduled),is.array)
  for (i in 1:length(scheduled[!filt])) {
    assign(paste0(scheduled[!filt][i],'_cur'),mget(scheduled[!filt][i])[[1]])
  }
  if (length(scheduled[filt])>0) {
    scheduled <- scheduled[filt]
    for (i in 1:length(scheduled)) {
      tmp <- mget(scheduled[i])[[1]]
      if (!any(c(0,1)%in%tmp[,1])) {
        stop(paste('The parameter schedule for',scheduled[i],'does not contain a value for the first colony.'),call.=FALSE)
      }
      tmp <- tmp[which.min(tmp[,1]),2]
      assign(paste0(scheduled[i],'_cur'),tmp)
      assign(scheduled[i],cbind(mget(scheduled[i])[[1]],FALSE))
    }
  } else {
    scheduled <- NULL
  }
  
  #initialize pheromones
  if (is.null(pheromones)) pheromones <- init.pheromones(short.factor.structure, number.of.subtests, deposit.on,alpha_cur)

  if (is.null(heuristics)) {
    heuristics <- lapply(pheromones,function(x) x^(1/1e+100))
  }

  ### Loops ###
  log <- NULL

  #creating user feedback
  message('Running STUART with MMAS.\n')
  progress <- utils::txtProgressBar(0,max(c(colonies,1)),style=3)
  count.gb <- 0

  repeat { #over colonies

    #parameter schedule
    if (!is.null(scheduled)) {
      for (i in 1:length(scheduled)) {
        tmp <- mget(scheduled[i])[[1]]
        if (schedule=='run') {
          tmp <- tmp[max(which(tmp[,1]<=run)),2]
        } 
        if (schedule=='colony') {
          tmp <- tmp[max(which(tmp[,1]<=colony)),2]
        }
        if (schedule=='mixed') {
          if (any(tmp[,3]-(tmp[,1]<=colony)<0)) mix_new <- TRUE
          if (mix_new) {
            tmp[,3] <- tmp[,3]|(tmp[,1]<=colony) 
          } else {
            tmp[,3] <- tmp[,3]|(tmp[,1]<=(colony+max(c(tmp[as.logical(tmp[,3]),1],1)-1)))
          }
          assign(scheduled[i],tmp)
          tmp <- tmp[which.max(tmp[as.logical(tmp[,3]),1]),2]
        }
        assign(paste0(scheduled[i],'_cur'),tmp)
      }
    }

    output.model <- FALSE
    ant.args <- mget(names(formals(ant.cycle)))
    if (length(scheduled[scheduled%in%names(ant.args)])>0) {
      ant.args[scheduled[scheduled%in%names(ant.args)]] <- mget(paste(scheduled[scheduled%in%names(ant.args)],'cur',sep='_'))
    }
    
    #parallel processing for R-internal estimations
    if (software=='lavaan') {
      if (cores>1) {
        #set up parallel processing on windows
        if (grepl('Windows',Sys.info()[1],ignore.case=TRUE)) {
          cl <- parallel::makeCluster(cores)
          
          ant.results <- parallel::parLapply(cl,1:ants_cur,function(x) do.call(ant.cycle,ant.args))
          parallel::stopCluster(cl)
        }
        
        #run ants in parallel on unixies
        else {
          ant.results <- parallel::mclapply(1:ants_cur,function(x) do.call(ant.cycle,ant.args), mc.cores=cores)
        }
      }

      #serial processing for single cores
      else {
        ant.results <- lapply(as.list(1:ants_cur),function(x) do.call(ant.cycle,ant.args))
      }
    }

    #serial processing if Mplus is used (Mplus-internal parallelization is used)
    if (software=='Mplus') {
      ant.args$filename <- filename
      ant.args$cores <- cores
      ant.results <- lapply(as.list(1:ants_cur),function(x) do.call(ant.cycle,ant.args))
    }
    
    #create log
    log <- rbind(log,cbind(rep(run,ants_cur),1:ants_cur,t(sapply(ant.results, function(x) array(data=unlist(x$solution.phe))))))

    #iteration.best memory
    ant.ib <- which.max(sapply(ant.results, function(x) return(x$solution.phe$pheromone)))
    solution.ib <- ant.results[[ant.ib]]$solution
    phe.ib <- ant.results[[ant.ib]]$solution.phe$pheromone
    selected.ib <- ant.results[[ant.ib]]$selected

    #feedback
    utils::setTxtProgressBar(progress,colony)

    #global.best memory
    if (phe.ib > phe.gb | run == 1) {
      count.gb <- count.gb + 1
      solution.gb <- solution.ib
      phe.gb <- phe.ib
      selected.gb <- selected.ib

      # in cases of mixed counting, reset mix_new
      mix_new <- FALSE
      
      #new solution user feedback
      message(paste('\nGlobal best no.',count.gb,'found. Colony counter reset.'))
      
      #restart the count
      colony <- 1
      utils::setTxtProgressBar(progress,0)
    }

    else {
      colony <- colony + 1
    }

    #compute upper and lower limits
    phe.max <- phe.gb/(1-evaporation_cur)
    phe.min <- (phe.max*(1-pbest_cur^(1/deci)))/((avg-1)*pbest_cur^(1/deci))

    if (phe.min >= phe.max) {
      stop('The lower pheromone limit is larger than the upper pheromone limit. This may be resolved by increasing pbest but may also indicate that none of the initial solutions were viable.\n',call.=FALSE)
    }
    
    #updated pheromones
    pheromones <- mmas.update(pheromones,phe.min,phe.max,evaporation_cur,deposit.on,
      get(paste('phe',deposit,sep='.')),get(paste('solution',deposit,sep='.')))

    #check for convergence
    tmp.min <- sapply(pheromones, function(x) sum(phe.min-tolerance_cur < x & x < phe.min+tolerance_cur))
    tmp.max <- sapply(pheromones, function(x) sum(phe.max-tolerance_cur < x & x < phe.max+tolerance_cur))
    tmp.all <- sapply(pheromones, length)
    tmp <- cbind(tmp.min,tmp.max)
    abort.sequence <- all(rowSums(tmp)==tmp.all) & all(tmp!=0)


    #abort if converged
    if (abort.sequence) {
      end.reason <- 'Algorithm converged.'
      break
    }
    if (colony > colonies_cur) {
      end.reason <- 'Maximum number of colonies exceeded.'
      break
    }

    #keep on counting
    run <- run + 1
  }

  #feedback
  close(progress)
  message(paste('\nSearch ended.',end.reason))      

  #Generating Output
  results <- mget(grep('.gb',ls(),value=TRUE))
  results$selected.items <- translate.selection(selected.gb,factor.structure,short)
  log <- data.frame(log)
  names(log) <- c('run','ant',names(ant.results[[1]]$solution.phe))
  results$log <- log
  results$pheromones <- pheromones
  results$parameters <- list(ants=ants,colonies=colonies,evaporation=evaporation,
    deposit=deposit,pbest=pbest,deposit.on=deposit.on,
    alpha=alpha,beta=beta,tolerance=tolerance,schedule=schedule,phe.max=phe.max,phe.min=phe.min,fitness.func=fitness.func,
    heuristics=heuristics)
  return(results)

}
