compute.combinations <-
function(
  short.factor.structure, number.of.items
) { #begin function

  comb <- matrix(c(sapply(short.factor.structure,length),unlist(number.of.items)),ncol=2)
  combinations <- prod(factorial(comb[,1])/(factorial((comb[,1]-comb[,2]))))
  
  return(combinations)

}
