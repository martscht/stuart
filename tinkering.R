# # Install current version
# devtools::install_bitbucket('martscht/stuart/stuart', ref = 'develop')
# 
# # load packages and data
# library(stuart)
# data(fairplayer)
devtools::load_all('~/projects/stuart/stuart')

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


#### Lexical debugging ----
data('sups', package = 'stuart')

sups_items <- c(
  "Gives me tasks requiring people who can influence my career",
  "Goes out of his/her way to promote my career interests",
  "Suggests strategies to advance my career",
  "Helps me identify and evaluate different career options",
  "Helps me identify important skills, interests, and values regarding my career",
  "Is realistic in discussing my career progression",
  "Helps me participate in high visibility activity either inside or outside the organization",
  "Discusses my concerns about competence, promotion, and relationships at work",
  "Helps me develop a strategy to meet my career goals",
  "Shares personal experiences as an alternative way of looking at my problems",
  "Encourages me to try new ways of behaving in my job",
  "Gives me tasks that help me learn new skills",
  "Assesses my performance with regard to goals and objectives",
  "Agrees goals and objectives to measure my current performance",
  "Makes clear what the goals and objectives of the organization are",
  "Identifies critical job elements",
  "Gives specific guidance as to how I can improve",
  "Keeps me informed of how well I am doing",
  "Produces a development plan to help me achieve future goals and objectives")

fs <- list(
  pro = paste0('SupS', 1:12),
  feed = paste0('SupS', 13:19))

simi <- lexicalSimilarity(sups, fs, 3, sups_items)

# Run traditional selection with arc-localization
sel <- mmas(sups, fs, 3, 
  seed = 35355, localization = 'arcs')
summary(sel)

best <- bruteforce(sups, fs, 3)
summary(best)

# integrate lexical similarities as heuristics
heu <- heuristics(sups, fs, 3, localization = 'arcs')
heu$pro
heu$pro <- simi$similarity.matrix[rownames(simi$similarity.matrix) %in% rownames(heu$pro), colnames(simi$similarity.matrix) %in% colnames(heu$pro)]
heu$feed <- simi$similarity.matrix[rownames(simi$similarity.matrix) %in% rownames(heu$feed), colnames(simi$similarity.matrix) %in% colnames(heu$feed)]

sel_simi <- mmas(sups, fs, 3, 
  seed = 35355, localization = 'arcs', heuristics = heu, beta = 1.5)
summary(sel_simi)

beta_schedule <- matrix(c(1, 50, 100, 3, 1, 0), ncol = 2)
sel_simi <- mmas(sups, fs, 3, 
  seed = 35355, localization = 'arcs', heuristics = heu, beta = beta_schedule)
summary(sel_simi)
