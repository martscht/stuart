library(devtools)
library(roxygen2)

setwd('~/stuart')
load_all('./stuart')

load('./stuart/data/fairplayer.rda')
load('./stuart/data/fairplayer_mtmm.rda')
load('./stuart/data/sups.rda')

# Generate documentation after changes in files
devtools::document('./stuart')

### Version 0.4.0 preparations ----

names(fairplayer_mtmm)

# intial tryout: self-ratings EM
fs <- list(em1=names(fairplayer_mtmm)[5:12],
  em2=names(fairplayer_mtmm)[13:20])

repme <- list(em=c('em1','em2'))

tmp <- mmas(fairplayer_mtmm,fs,1,3,repeated.measures=repme,software='Mplus')
summary(tmp)
crossvalidate(tmp,fairplayer_mtmm,fairplayer_mtmm)

# set up: one construct, two methods
fs <- list(emS=names(fairplayer_mtmm)[5:12],
  emT=names(fairplayer_mtmm)[29:36])

mtmm <- list(em=c('emS','emT'))

debug(mmas);debug(data.prep)

tmp <- mmas(fairplayer_mtmm,fs,2,3,repeated.measures=mtmm,software='Mplus',pbest=.1,ants=10,colonies=3)
summary(tmp)

