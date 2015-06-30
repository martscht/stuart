library(devtools)
library(roxygen2)

setwd('~/stuart')
load_all('./stuart')

load('./stuart/data/fairplayer.rda')
load('./stuart/data/sups.rda')

# Generate documentation after changes in files
devtools::document('./stuart')

### Version 0.3.0 preparations ----

data(fairplayer)
names(fairplayer)
fairplayer$g <- c(rep(0,65),rep(1,70))

fs <- list(SI=names(fairplayer)[2:11],
  EM=names(fairplayer)[32:39])

#shorten the loop after fiddling
redo <- function(x=NULL) {
  undebug(run.Mplus)
  load_all('./stuart')
  debug(run.Mplus)
  item.selection <- mmas(fairplayer, fs, 1, 4, grouping='g',
    pbest=.1, ants=1, colonies=1, cores=1, software='Mplus')
}

debug(run.Mplus)


mplus.begin <- proc.time()
item.selection <- mmas(fairplayer, fs, 1, 4, ants=3, colonies=1, pbest=.1, software='Mplus')
mplus.end <- proc.time()

lavaan.begin <- proc.time()
item.selection <- mmas(fairplayer, fs, 1, 4, pbest=.1, software='lavaan')
lavaan.end <- proc.time()

# Obtaining a 4-item short-version with strict
# longitudinal invariance
data(fairplayer)

fs <- list(SI1=names(fairplayer)[2:11],
           SI2=names(fairplayer[12:21]))

repme <- list(SI=c('SI1','SI2'))

longitudinal <- bruteforce(fairplayer, fs, 1, 3, repeated.measures=repme, software='Mplus')

