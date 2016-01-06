library(devtools)
library(roxygen2)

setwd('~/stuart')
load_all('./stuart')

load('./stuart/data/fairplayer.rda')
load('./stuart/data/fairplayer_mtmm.rda')
load('./stuart/data/sups.rda')

# Generate documentation after changes in files
devtools::document('./stuart')
