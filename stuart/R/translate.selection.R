translate.selection <-
function(
  selected, 
  factor.structure, short, mtmm
) { #begin function

  selected.items <- vector('list',length(factor.structure))

  for (i in 1:length(factor.structure)) {
    locate <- which(unlist(lapply(short,
      function(x) is.element(names(factor.structure)[i],x))))

    selected.items[[i]] <- lapply(selected[[locate]],function(x) factor.structure[[i]][x])

    # Useable for CTC(M-1) structure    
#     if (names(factor.structure)[i]%in%unlist(lapply(mtmm,function(x) x[1]))) {
#       filt <- unlist(lapply(mtmm,function(x) x[1]))==names(factor.structure)[i]
#       if (length(unlist(mtmm[filt]))>1) {
#         for (j in 2:length(unlist(mtmm[filt]))) {
#           tmp <- factor.structure[[which(names(factor.structure)==unlist(mtmm[filt])[j])]]
#           for (k in 1:length(selected[[locate]])) {
#             selected.items[[i]][[k]] <- c(selected.items[[i]][[k]],tmp[selected[[locate]][[k]]])
#           }
#         }
#       }
#     }
    
  }

  #dole out some names
  names(selected.items) <- names(factor.structure)
  for (i in 1:length(selected.items)) {
    names(selected.items[[i]]) <- paste(names(selected.items)[i],LETTERS[1:length(selected.items[[i]])],sep='')
  }

  return(selected.items)

}
