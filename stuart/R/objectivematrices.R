objectivematrices <- 
  function(
    data, factor.structure, capacity=NULL, #number.of.subtests=1, #subtest settings
    matrices = c('lvcor'), n.random = 0,
    #invariance='parallel',
    item.invariance='congeneric',                  #cross invariance
    
    repeated.measures=NULL, long.invariance='strict', #long structure
    mtmm=NULL, mtmm.invariance='configural', #MTMM structure
    grouping=NULL, group.invariance='strict', #grouping structure
    comparisons=NULL,
    auxiliary=NULL, use.order=FALSE,
    software='lavaan', cores=NULL,                                        #run settings
    objective=NULL, ignore.errors=FALSE,                      #fitness specs
    analysis.options=NULL, suppress.model=FALSE,                          #modeling specs
    ...
  ) {
    
    if (is.null(objective)) {
      objective <- fixedobjective()
    }
    filt <-  matrices
    filt <- setdiff(filt,  c(eval(objective$call$criteria), eval(objective$call$add)))
    
    objective$call$add <- c(eval(objective$call$add), filt)
    
    addage <- vector('list', length(filt))
    names(addage) <- filt
    formals(objective$func) <- c(formals(objective$func), addage)
    
    args <- as.list(match.call())[-1]
    args <- c(args,formals()[!names(formals())%in%c(names(args),'...')])
    args <- args[names(args) %in% names(formals(randomsamples))]
    args$n <- max(c(1, n.random))
    args$objective <- objective
    args$matrices <- NULL
    
    attempts <- ifelse(n.random == 0, 0, 9)
    worked <- FALSE
    message('Attempting to extract matrices from a random subset...')
    while (!worked & attempts < 10) {
      invisible(utils::capture.output(suppressMessages(resi <- do.call(randomsamples, args))))
      attempts <- attempts + 1
      if (any(resi$log$pheromone != 0)) {
        worked <- TRUE
        message('\b done!')
      }
    }
    if (!worked) {
      stop('Was not able to extract matrices in 10 attempts. This may indicate a problem with the model, but may also be resolved by simply trying again.', call. = FALSE)
    }
    if (n.random == 0) {
      resi_mat <- resi$log_mat
    } else {
      tmp <- which(resi$log$pheromone != 0)[1]
      resi_mat <- lapply(resi$log_mat, `[`, tmp)
    }

    out <- vector('list', length(matrices))
    names(out) <- matrices
    types <- c('use', 'mean', 'sd', 'side', 'skew', 'scale')
    for (i in matrices) {
      out[[i]] <- vector('list', length(types))
      names(out[[i]]) <- types
      for (j in types) {
        out[[i]][[j]] <- extractmatrices(resi_mat[[i]][[1]], j)
      }
    }
    
    if (n.random > 0) {
      for (i in matrices) {
        dims <- dim(resi$log_mat[[i]][which(resi$log$pheromone != 0)[1]][[1]])
        if (i == 'lvcor') {
          resi_means <- do.call(rbind, sapply(resi$log_mat[[i]], c)) |> 
            fishz() |> colMeans(na.rm = TRUE) |> inv.fishz()
        } else {
          resi_means <- do.call(rbind, sapply(resi$log_mat[[i]], c)) |> colMeans(na.rm = TRUE)
        }
        resi_sds <- do.call(rbind, sapply(resi$log_mat[[i]], c)) |> apply(2, stats::sd, na.rm = TRUE)
        resi_sds <- sapply(resi_sds, \(x) max(c(.01, x)))

        out[[i]]$mean <- matrix(resi_means, nrow = dims[1], ncol = dims[2])
        out[[i]]$sd <-  matrix(resi_sds, nrow = dims[1], ncol = dims[2])
      }
    } 
    
    class(out) <- 'stuartObjectiveMatrices'
    return(out)

  }


extractmatrices <- function(x, type = 'use') {
  if (is.list(x)) {
    lapply(x, extractmatrices, type = type)
  }
  else {
    x <- unclass(x)
    if (type == 'use') {out <- x != 0}
    if (type == 'mean') {x[x != Inf] <- 0; out <- x}
    if (type == 'sd') {x[x != Inf] <- 1; out <- x}
    if (type == 'side') {x[x != Inf] <- 'center'; out <- x}
    if (type == 'skew') {out <- x == Inf}
    if (type == 'scale') {x[x != Inf] <- 1; out <- x}
    return(out)
  }
}
