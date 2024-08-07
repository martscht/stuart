stuart.randomsamples <-
function(
  short.factor.structure, short, long.equal, comparisons.equal,
  comparisons.invariance, #made on toplevel
  capacity,
  data, factor.structure, auxi, use.order,                      #simple prerequisites
  
  item.invariance,
  repeated.measures, long.invariance,                            #longitudinal relations
  mtmm, mtmm.invariance,                                         #mtmm relations
  grouping, group.invariance,                                    #grouping relations
  software, cores,                                               #Software to be used
  comparisons,
  objective=NULL, ignore.errors=FALSE,                        #fitness function
  
  suppress.model=FALSE, analysis.options=NULL,                   #Additional modeling
  seed,
  
  filename, n=1000, percentile=100,
  
  ...                                                            #All the other stuff
) { #start function

  ### Loops ###
  log <- NULL
  
  #generate random sample of combinations
  message('Generating random samples of combinations.')
  full <- FALSE
  
  if (!is.null(seed)) {
    old.seed <- .Random.seed
    set.seed(seed)
    on.exit(.Random.seed <<- old.seed)
  }

  combinations <- do.call('generate.combinations',mget(names(formals(generate.combinations))))
  duplicate <- combinations$duplicate
  
  filter <- combinations$filter[!duplicated(duplicate), , drop = FALSE]
  # combi <- lapply(combinations$combi, function(x) x[!duplicated(duplicate), ])
  combi <- combinations$combi
  
  #creating user feedback
  message('Running STUART with random samples.\n')
  progress <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(progress,0)
  count.gb <- 0
  
  output.model <- FALSE
  svalues <- FALSE
  bf.args <- mget(names(formals(bf.cycle))[-1])
  
  if (software=='Mplus') {
    #file location
    if (is.null(filename)) filename <- paste0(tempdir(), '/stuart')
    
    #writing the data file
    utils::write.table(data,paste(filename,'_data.dat',sep=''),
      col.names=FALSE,row.names=FALSE,na='-9999',
      sep='\t',dec='.')
  }
  
  if (nrow(filter) > 0) {
    
    #parallel processing for R-internal estimations
    if (software=='lavaan') {
      if (cores>1) {
        if (.Platform$GUI=='RStudio') message('\nProgressbars are not functional when utilizing multiple cores for randomsamples in RStudio.')
        #set up parallel processing on windows
        if (grepl('Windows',Sys.info()[1],ignore.case=TRUE)) {
          if (!.Platform$GUI=='RStudio') message('\nProgressbars are not functional when utilizing multiple cores for randomsamples in Windows.')
          cl <- parallel::makeCluster(cores)
          
          bf.results <- parallel::parLapply(cl,1:nrow(filter),function(run) {
            utils::setTxtProgressBar(progress, ceiling(run/(10*cores))/(nrow(filter)/(10*cores)));
            do.call('bf.cycle',c(run,bf.args))
          })
          parallel::stopCluster(cl)
        }
        
        #run ants in parallel on unixies
        else {
          bf.results <- parallel::mclapply(1:nrow(filter),
            function(run) {     
              utils::setTxtProgressBar(progress, ceiling(run/(10*cores))/(nrow(filter)/(10*cores)));
              do.call('bf.cycle',c(run,bf.args))
            },
            mc.cores=cores
          )
        }
      }
      
      else {
        bf.results <- lapply(1:nrow(filter),
          function(run) {     
            utils::setTxtProgressBar(progress, run/nrow(filter));
            do.call('bf.cycle',c(run,bf.args))
          }
        )
      }
    }
    
    #serial processing if Mplus is used (Mplus-internal parallelization is used)
    if (software=='Mplus') {
      bf.args$filename <- filename
      bf.args$cores <- cores
      bf.results <- lapply(1:nrow(filter),
        function(run) {     
          utils::setTxtProgressBar(progress, run/nrow(filter));
          do.call('bf.cycle',c(run,bf.args))
        }
      )
    }
  }
  
  #fill in results for duplicates
  tmp <- vector('list', n)
  tmp[filter[,1]] <- bf.results
  bf.results <- tmp[duplicate]
  
  # Evaluate using empirical objective
  if (inherits(objective, 'stuartEmpiricalObjective')) {
    args <- c(objective$call, x = list(bf.results))
    objective <- do.call(empiricalobjective, args)
    bf.results <- lapply(bf.results, function(x) {
      if(all(is.na(x$solution.phe[-1]))) x$solution.phe$pheromone <- 0
      else x$solution.phe$pheromone <- do.call(objective$func, x$solution.phe[-1])
      if(is.na(x$solution.phe$pheromone)) x$solution.phe$pheromone <- 0
      return(x)})
  }
  
  
  #generate matrix output
  mat_fil <- c('lvcor', 'lambda', 'theta', 'psi', 'alpha', 'beta', 'nu', 'tau', 'delta', 'gamma')
  mat_fil <- mat_fil[mat_fil %in% names(formals(objective$func))]
  mats <- as.list(vector('numeric', length(mat_fil)))
  names(mats) <- mat_fil
  
  for (m in seq_along(mat_fil)) {
    mats[[m]] <- sapply(bf.results, function(x) x$solution.phe[mat_fil[m]])
    names(mats[[m]]) <- 1:n
  }

  log <- cbind(1:n,t(sapply(bf.results, function(x) array(data=unlist(x$solution.phe[!names(x$solution.phe)%in%mat_fil])))))
  log <- data.frame(log)
  names(log) <- c('run',names(bf.results[[1]]$solution.phe)[!names(bf.results[[1]]$solution.phe)%in%mat_fil])

  #best solution
  tmp <- data.frame(1:length(bf.results),sapply(bf.results, function(x) return(x$solution.phe$pheromone)))
  tmp <- tmp[tmp[,2]!=0,]
  run.sel <- tmp[tmp[,2]==sort(tmp[,2])[round((percentile/100)*nrow(tmp))],1]
  if (length(run.sel) > 1) {
    warning('The chosen percentile of the pheromone was achieved by multiple solutions. Only the first is reported.',call.=FALSE)
    run.sel <- run.sel[1]
  }
  if (length(run.sel) == 0) run.sel <- 1
  phe.sel <- bf.results[[run.sel]]$solution.phe$pheromone
  selected.sel <- bf.results[[run.sel]]$selected
  
  close(progress)
  message('\nSearch ended.')
  
  # construction solution in standard format
  solution.sel <- short.factor.structure
  for (i in 1:length(short.factor.structure)) {
    solution.sel[[i]] <- seq_along(short.factor.structure[[i]])
    solution.sel[[i]] <- solution.sel[[i]] %in% selected.sel[[i]]
    names(solution.sel[[i]]) <- short.factor.structure[[i]]
  }
  
  results <- mget(grep('.sel',ls(),value=TRUE))
  results$selected.items <- translate.selection(selected.sel,factor.structure,short)
  results$log <- log
  results$log_mat <- mats
  results$pheromones <- NULL
  results$parameters <- list(n=n,percentile=percentile,seed=seed,objective=objective,
    factor.structure=factor.structure)
  return(results)
  
}
