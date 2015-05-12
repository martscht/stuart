compute.combinations <-
function(
  short.factor.structure, number.of.items
) { #begin function

  combinations <- NULL
  for (i in 1:length(short.factor.structure)) {
    combinations[i] <- factorial(length(short.factor.structure[[i]])) / factorial(length(short.factor.structure[[i]])-sum(number.of.items[[i]]))
  }
  combinations <- prod(combinations)

  return(combinations)

}
