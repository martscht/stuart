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
obj <- stuart:::fixedobjective(criteria = c('rmsea', 'srmr', 'cfi', 'crel'),
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
fix <- stuart:::fixedobjective(criteria = c('rmsea', 'cfi', 'srmr'), scale = 2/3)
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

#### Multiple Groups ----
data(fairplayer)
fs <- list(em1 = paste0('sEM0', 1:8, 't1'),
  em2 = paste0('sEM0', 1:8, 't2'))
re <- list(em = c('em1', 'em2'))

# simple comparisons
selg <- mmas(fairplayer, fs, 3,
  repeated.measures = re,
  grouping = 'IGL',
  colonies = 0, comparisons = c('group', 'long'),
  pbest = .02)
selg$parameters$objective # old comparison preset used

# presetting objectives
fix <- stuart:::fixedobjective(criteria = c('rmsea', 'cfi', 'srmr', 'crel', 'delta.pvalue.group', 'delta.pvalue.long'),
  comparisons = c('group', 'long'),
  scale = c(1/3, 1/3, 1/3, 1, .25, .25))
fix

selg2 <- mmas(fairplayer, fs, 3,
  repeated.measures = re,
  grouping = 'IGL',
  objective = fix,
  colonies = 0, comparisons = c('group', 'long'),
  pbest = .02)
summary(selg2)

selg2b <- bruteforce(fairplayer, fs, 3,
  repeated.measures = re,
  grouping = 'IGL',
  objective = fix,
  comparisons = c('group', 'long'))

#### Multiple methods ----
fs <- list(em1 = paste0('sEM0', 1:8, 't1'),
  em2 = paste0('tEM0', 1:8, 't1'))
mm <- list(em = c('em1', 'em2'))

fix <- stuart:::fixedobjective(criteria = c('rmsea', 'cfi', 'srmr', 'crel', 'delta.pvalue'),
  comparisons = c('group'),
  scale = c(1/3, 1/3, 1/3, 1, .25))
fix

selg2 <- mmas(fairplayer, fs, 3,
  mtmm = mm,
  grouping = 'IGL',
  objective = fix,
  colonies = 0, comparisons = c('group'),
  cores = 1,
  pbest = .02)
summary(selg2)

#### Now empirical ----
emp <- stuart:::empiricalobjective(criteria = c('rmsea', 'cfi', 'srmr', 'crel', 'delta.pvalue', 'con'),
  side = c('bottom', 'top', 'bottom', 'top', 'top', 'top'),
  comparisons = c('group'),
  scale = c(1/3, 1/3, 1/3, 1, .25, 1))

selg3 <- mmas(fairplayer, fs, 3,
  mtmm = mm,
  grouping = 'IGL',
  objective = emp, burnin = 1,
  colonies = 10, comparisons = c('group'),
  pbest = .5)
summary(selg3)

selg4 <- gene(fairplayer, fs, 3,
  mtmm = mm,
  grouping = 'IGL',
  objective = emp, burnin = 1,
  generations = 10, comparisons = c('group'),
  seed = 1)

#### Returning all matrices (once) ----
fs <- list(em = paste0('sEM0', 1:8, 't1'),
  si = c(paste0('sSI0', 1:9, 't1'), 'sSI10t1'))

add <- 'em ~ si'

mats <- objectivematrices(fairplayer, fs, 4, matrices = c('beta'),
  analysis.options = list(model = add),
  grouping = 'IGL')

obj <- fixedobjective(c('rmsea', 'cfi', 'srmr'), matrices = mats)

sel1 <- gene(fairplayer, fs, 4, objective = obj,
  grouping = 'IGL', analysis.options = list(model = add),
  seed = 1)
sel2 <- mmas(fairplayer, fs, 4, objective = obj,
  grouping = 'IGL', analysis.options = list(model = add),
  seed = 1)
sel3 <- randomsamples(fairplayer, fs, 4, objective = obj,
  grouping = 'IGL', analysis.options = list(model = add),
  seed = 1)

obj <- empiricalobjective(c('rmsea', 'delta.pvalue', 'cfi', 'srmr'), matrices = mats,
  comparisons = 'group')
sel1 <- gene(fairplayer, fs, 4, objective = obj,
  analysis.options = list(model = add),
  seed = 1, burnin = 2, generations = 2)
sel2 <- mmas(fairplayer, fs, 4, objective = obj,
  analysis.options = list(model = add),
  seed = 1, burnin = 2, colonies = 1)


mats <- objectivematrices(fairplayer, fs, 4, matrices = c('beta'),
  analysis.options = list(model = add))
obj <- empiricalobjective(c('rmsea', 'cfi', 'srmr'), matrices = mats)

selk_gene <- kfold('gene', 2,
  data = fairplayer, factor.structure = fs, capacity = 4,
  objective = obj, analysis.options = list(model = add),
  seed = 1)

selk_mmas <- kfold('mmas', 2,
  data = fairplayer, factor.structure = fs, capacity = 4,
  objective = obj, analysis.options = list(model = add),
  seed = 1)
