translate.selection <-
function(
  selected,
  factor.structure, repeated.measures
) { #begin function

  selected.items <- list(NA)

  for (i in 1:length(factor.structure)) {
    selected.items[[i]] <- list(NA)
    for (j in 1:length(repeated.measures)) {
      if (is.element(names(factor.structure)[i],repeated.measures[[j]])) {
        for (k in 1:length(selected[[j]])) {
          selected.items[[i]][[k]] <- factor.structure[[i]][selected[[j]][[k]]]
        }
      }
    }
  }

  #dole out some names
  names(selected.items) <- names(factor.structure)
  for (i in 1:length(selected.items)) {
    names(selected.items[[i]]) <- paste(names(selected.items)[i],LETTERS[1:length(selected.items[[i]])],sep='')
  }

  return(selected.items)

}
