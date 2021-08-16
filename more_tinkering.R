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

# empirical objective
obj <- stuart:::empiricalobjective(criteria = c('rmsea', 'srmr', 'cfi', 'crel'),
  add = c('chisq', 'df', 'pvalue'), scale = c(.33, .33, .33, 1))

sel3 <- mmas(sups, fs, 4,
  objective = obj,
  evaporation = .8)


selb <- bruteforce(sups, fs, 4)
sel1 <- mmas(sups, fs, 4)

# extract empiricals from brute
rmsea <- selb$log$rmsea
rmsea_obj <- stuart:::extractobjective(rmsea, min(c(.1, 100/length(rmsea))), 'bottom')
srmr <- selb$log$srmr
srmr_obj <- stuart:::extractobjective(srmr, min(c(.1, 100/length(srmr))), 'bottom')
crel <- selb$log$crel
crel_obj <- stuart:::extractobjective(crel, min(c(.1, 100/length(crel))), 'bottom')


parsed <- parse(text = 
    paste(gsub('x', 'rmsea', rmsea_obj$string), 
      gsub('x', 'srmr', srmr_obj$string), 
      gsub('x', 'crel', crel_obj$string), sep = ' + '))
brute_func <- function(rmsea, srmr, crel) eval(parsed)
opt <- selb$log[which.max(selb$log$pheromone), ]
brute_func(opt$rmsea, opt$srmr, opt$crel)
max(brute_func(selb$log$rmsea, selb$log$srmr, selb$log$crel))
  