stuart.randomsamples <-
function(
  short.factor.structure, short, long.equal, item.long.equal,    #made on toplevel
  number.of.items,
  data, factor.structure, auxi, use.order,                      #simple prerequisites
  
  number.of.subtests,  invariance,                               #subtest relations
  repeated.measures, long.invariance,                            #longitudinal relations
  mtmm, mtmm.invariance,                                         #mtmm relations
  grouping, group.invariance,                                    #grouping relations
  
  item.invariance, item.long.invariance, item.mtmm.invariance,
  item.group.invariance,
  
  software, cores,                                               #Software to be used
  
  fitness.func=NULL, ignore.errors=FALSE,                        #fitness function
  
  suppress.model=FALSE, analysis.options=NULL,                   #Additional modeling
  
  filename, n=1000, percentile=100,
  
  ...                                                            #All the other stuff
) { #start function

  ### Loops ###
  log <- NULL
  
  #generate random sample of combinations
  message('Generating random samples of combinations.')
  full <- FALSE
  combinations <- do.call('generate.combinations',mget(names(formals(generate.combinations))))
  filter <- combinations$filter
  combi <- combinations$combi
  
  #creating user feedback
  message('Running STUART with random samples.\n')
  progress <- txtProgressBar(style=3)
  setTxtProgressBar(progress,0)
  count.gb <- 0
  
  output.model <- FALSE
  bf.args <- mget(names(formals(bf.cycle))[-1])
  
  #parallel processing for R-internal estimations
  if (software=='lavaan') {
    if (cores>1) {
      #set up parallel processing on windows
      if (grepl('Windows',Sys.info()[1],ignore.case=TRUE)) {
        cl <- parallel::makeCluster(cores)
        
        #load estimation software to clusters
        parallel::parLapply(cl,1:cores,function(x) library(software,character.only=TRUE,quietly=TRUE,verbose=FALSE))
        parallel::parLapply(cl,1:cores,function(x) library(stuart,quietly=TRUE,verbose=FALSE))
        
        bf.results <- parallel::parLapply(cl,1:n,function(run) {
          setTxtProgressBar(progress, ceiling(run/(10*cores))/(n/(10*cores)));
          do.call('bf.cycle',c(run,bf.args))
        })
        parallel::stopCluster(cl)
      }
      
      #run ants in parallel on unixies
      else {
        bf.results <- parallel::mclapply(1:n,
          function(run) {     
            setTxtProgressBar(progress, ceiling(run/(10*cores))/(n/(10*cores)));
            do.call('bf.cycle',c(run,bf.args))
          },
          mc.cores=cores
        )
      }
    }
    
    else {
      bf.results <- lapply(1:n,
        function(run) {     
          setTxtProgressBar(progress, run/n);
          do.call('bf.cycle',c(run,bf.args))
        }
      )
    }
  }
  
  #serial processing if Mplus is used (Mplus-internal parallelization is used)
  if (software=='Mplus') {
    bf.args$filename <- filename
    bf.args$cores <- cores
    bf.results <- lapply(1:n,
      function(run) {     
        setTxtProgressBar(progress, run/nrow(filter));
        do.call('bf.cycle',c(run,bf.args))
      }
    )
  }
  
  log <- cbind(1:nrow(filter),t(sapply(bf.results, function(x) array(data=unlist(x$solution.phe)))))
  
  #best solution
  tmp <- data.frame(1:length(bf.results),sapply(bf.results, function(x) return(x$solution.phe$pheromone)))
  tmp <- tmp[tmp[,2]!=0,]
  run.sel <- tmp[tmp[,2]==sort(tmp[,2])[(percentile/100)*nrow(tmp)],1]
  phe.sel <- bf.results[[run.sel]]$solution.phe$pheromone
  selected.sel <- bf.results[[run.sel]]$selected
  
  close(progress)
  message('\nSearch ended.')
  
  results <- mget(grep('.sel',ls(),value=TRUE))
  results$selected.items <- translate.selection(selected.sel,factor.structure,short)
  log <- data.frame(log)
  names(log) <- c('run',names(bf.results[[1]]$solution.phe))
  results$log <- log
  results$pheromones <- NULL
  results$parameters <- NULL
  return(results)
  
}
