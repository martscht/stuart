library(devtools)

setwd('~/stuart')
load_all('./stuart')

### simple example ----
data(sups)
fs <- list(cp = names(sups)[2:13],
  fe = names(sups)[14:19])

combinations(sups, fs, 4)

# fixed default-use (probit)
sel <- mmas(sups, fs, 4,
  evaporation = .8)
summary(sel)

# fixed objective (probit)
obj <- stuart:::defaultobjective(criteria = c('rmsea', 'srmr', 'cfi', 'crel'),
  add = c('chisq', 'df', 'pvalue'), scale = c(.33, .33, .33, 1))

sel2 <- mmas(sups, fs, 4,
  objective = obj,
  evaporation = .8)

sel2b <- gene(sups, fs, 4, 
  objective = obj)

# empirical objective
obj <- stuart:::empiricalobjective(criteria = c('rmsea.robust', 'srmr', 'cfi.robust', 'crel'),
  add = c('chisq', 'df', 'pvalue'), scale = c(.33, .33, .33, 1))

sel3 <- mmas(sups, fs, 4,
  objective = obj,
  evaporation = .8,
  analysis.options = list(estimator = 'mlr'))

sel3b <- gene(sups, fs, 4,
  objective = obj)

sel3c <- bruteforce(sups, fs, 4, objective = obj)

# mixed objective
fix <- stuart:::defaultobjective(criteria = c('rmsea', 'cfi', 'srmr', 'beta[2,1]'), scale = 2/3)
obj <- stuart:::empiricalobjective(criteria = c('crel'), add = c('chisq', 'df', 'pvalue'), 
  fixed = fix)

mod_add <- 'fe ~ cp'

sel4 <- mmas(sups, fs, 4,
  objective = obj,
  evaporation = .8,
  colonies = 64,
  analysis.options = list(model = mod_add))

selb <- bruteforce(sups, fs, 4)
sel1 <- mmas(sups, fs, 4)


# finding some problems
summary(sel3)
log <- sel4$log
obj_post <- sel4$parameters$objective$func
obj_pre <- obj$func

log$pheromone_pre <- do.call(obj_pre, log[, names(formals(obj_pre))]) 
log$pheromone_post <- do.call(obj_post, log[, names(formals(obj_post))]) 

library(ggplot2)
long_log <- reshape(log, direction = 'long',
  varying = list(pheromone = c('pheromone', 'pheromone_pre', 'pheromone_post')),
  v.names = 'pheromone',
  timevar = 'type', times = c('Actual', 'Pre', 'Post'),
  idvar = 'solution')
View(long_log)

ggplot(long_log, aes(x = solution, y = pheromone, color = type)) +
  geom_point(alpha = .5) + theme_minimal()

subset(long_log, type == 'Actual') |> ggplot(aes(x = solution, y = rmsea)) +
  geom_point(alpha = .5) + theme_minimal() + geom_smooth()
subset(long_log, type == 'Actual') |> ggplot(aes(x = solution, y = crel)) +
  geom_point(alpha = .5) + theme_minimal() + geom_smooth()
subset(long_log, type == 'Actual') |> ggplot(aes(x = solution, y = srmr)) +
  geom_point(alpha = .5) + theme_minimal() + geom_smooth()
subset(long_log, type == 'Actual') |> ggplot(aes(x = solution, y = cfi)) +
  geom_point(alpha = .5) + theme_minimal() + geom_smooth()
subset(long_log, type == 'Post') |> ggplot(aes(x = solution, y = pheromone)) +
  geom_point(alpha = .5) + theme_minimal() + geom_smooth()
