translate.selection <-
function(
  selected, 
  factor.structure, short.factor.structure, short,
  repeated.measures, mtmm
) { #begin function

  selected.items <- vector('list',length(factor.structure))

  for (i in 1:length(factor.structure)) {
    locate <- which(unlist(lapply(short,
      function(x) is.element(names(factor.structure)[i],x))))

    selected.items[[i]] <- lapply(selected[[locate]],function(x) factor.structure[[i]][x])
  }

  #dole out some names
  names(selected.items) <- names(factor.structure)
  for (i in 1:length(selected.items)) {
    names(selected.items[[i]]) <- paste(names(selected.items)[i],LETTERS[1:length(selected.items[[i]])],sep='')
  }

  return(selected.items)

}
