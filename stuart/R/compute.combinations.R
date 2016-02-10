compute.combinations <-
function(
  short.factor.structure, number.of.items, use.order
) { #begin function

  comb <- matrix(c(sapply(short.factor.structure,length),unlist(number.of.items)),ncol=2)
  if (use.order) {
    combinations <- prod(factorial(comb[,1])/(factorial((comb[,1]-comb[,2]))))
  } else {
    combinations <- prod(choose(comb[,1],comb[,2]))
  }
  
  return(combinations)

}
