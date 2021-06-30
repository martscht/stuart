# # Install current version
# devtools::install_bitbucket('martscht/stuart/stuart', ref = 'develop')
# 
# # load packages and data
# library(stuart)
# data(fairplayer)
devtools::load_all('~/stuart/stuart')

library(lavaan)

# set up minimal example (two constructs)
fs <- list(em1 = names(fairplayer)[5:12],
  si1 = names(fairplayer)[83:92])

# custom objective
objective.scaled <- function(rmsea.scaled, srmr, cfi.scaled) {
  out1 = 0.5-(0.5/(1 + exp(- 100 * (rmsea.scaled-.05))))
  out2 = 0.5-(0.5/(1 + exp(- 100 * (srmr-.05))))
  out3 = (1/(1 + exp(- 100 * (cfi.scaled-.95))))
  out = out1 + out2 + out3 + out4
  return(out)
}

# ordinal dataset
ords <- fairplayer[, names(fairplayer)%in%unlist(fs)]
ords <- lapply(ords, as.ordered)
ords <- do.call(data.frame, ords)

# run with regular data
sel <- mmas(fairplayer, fs, 4, 
  seed = 35355,
  analysis.options = list(estimator = 'wlsmv', ordered = TRUE),
  objective=objective.normal)

# Returns:
  # Error: The lower pheromone limit is larger than the upper pheromone limit. This may indicate that none of the initial solutions were viable due to estimation problems.

# Attempt with ordinal data
sel <- mmas(ords, fs, 4, 
  seed = 35355,
  objective=objective.normal,
  colonies = 0)

# Returns:
  # Warning messages:
  #   1: It is highly recommended to used either scaled or robust versions of model fit criteria in your objective function when modeling ordinal indicators with lavaan. 
  # 2: Invariance assumptions regarding residual variances of ordinal indicators are not possible in the current approach and are ignored. 

summary(sel)
sel$final

held <- holdout(ords)
sel <- mmas(ords, fs, 4, 
  seed = 35355,
  objective=objective.scaled,
  colonies = 0)
crossvalidate(sel, ords, ords[-c(1:10), ])
