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
  filt <- sapply(mget(scheduled),is.array)
  for (i in 1:length(scheduled[!filt])) {
    assign(paste0(scheduled[!filt][i],'_cur'),mget(scheduled[!filt][i])[[1]],envir=.GlobalEnv)
  }
  if (length(scheduled[filt])>0) {
    scheduled <- scheduled[filt]
  } else {
    scheduled <- NULL
  }
  
  #initialize pheromones
  if (is.null(pheromones)) pheromones <- init.pheromones(short.factor.structure, number.of.subtests, deposit.on)

  if (is.null(heuristics)) {
    heuristics <- lapply(pheromones,function(x) x^1/1e+100)
  }

  ### Loops ###
  log <- NULL

  #creating user feedback
  message('Running STUART with MMAS.\n')
  progress <- txtProgressBar(0,max(c(colonies,1)),style=3)
  count.gb <- 0

  repeat { #over colonies

    #parameter schedule
    if (!is.null(scheduled)) {
      for (i in 1:length(scheduled)) {
        tmp <- mget(scheduled[i])[[1]]
        tmp <- tmp[max(which(tmp[,1]<=mget(schedule)[[1]])),2]
        assign(paste0(scheduled[i],'_cur'),tmp)
      }
    }

    output.model <- FALSE
    ant.args <- mget(names(formals(ant.cycle)))
    if (!is.null(scheduled)) {
      ant.args[scheduled[scheduled%in%names(ant.args)]] <- mget(paste(scheduled[scheduled%in%names(ant.args)],'cur',sep='_'))
    }
    
    #parallel processing for R-internal estimations
    if (software=='lavaan') {
      if (cores>1) {
        #set up parallel processing on windows
        if (grepl('Windows',Sys.info()[1],ignore.case=TRUE)) {
          cl <- parallel::makeCluster(cores)
          
          #load estimation software to clusters
          parallel::parLapply(cl,1:cores,function(x) library(software,character.only=TRUE,quietly=TRUE,verbose=FALSE))
          
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
    setTxtProgressBar(progress,colony)
    
    #global.best memory
    if (phe.ib > phe.gb | run == 1) {
      count.gb <- count.gb + 1
      solution.gb <- solution.ib
      phe.gb <- phe.ib
      selected.gb <- selected.ib

      #compute upper and lower limits
      phe.max <- phe.gb/(1-evaporation_cur)
      phe.min <- (phe.max*(1-pbest_cur^(1/deci)))/((avg-1)*pbest_cur^(1/deci))

      if (phe.min >= phe.max) {
        stop('The lower pheromone limit is larger than the upper pheromone limit. This may be resolved by increasing pbest but may also indicate that none of the initial solutions were viable.\n',call.=FALSE)
      }

      #new solution user feedback
      message(paste('\nGlobal best no.',count.gb,'found. Colony counter reset.'))

      #restart the count
      colony <- 1
      setTxtProgressBar(progress,0)
    }

    else {
      colony <- colony + 1
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
    alpha=alpha,beta=beta,tolerance=tolerance,phe.max=phe.max,phe.min=phe.min,fitness.func=fitness.func)
  return(results)

}
