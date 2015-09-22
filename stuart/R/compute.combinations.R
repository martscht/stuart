compute.combinations <-
function(
  short.factor.structure, number.of.items
) { #begin function

  comb <- matrix(c(sapply(short.factor.structure,length),unlist(number.of.items)),ncol=2)
  combinations <- prod(choose(comb[,1],comb[,2]))
  
  return(combinations)

}
