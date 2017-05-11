library(devtools)
library(roxygen2)

setwd('~/stuart')
load_all('./stuart')

load('./stuart/data/fairplayer.rda')
load('./stuart/data/sups.rda')

# Check package
check('./stuart')

# Generate documentation after changes in files
devtools::document('./stuart')

# build packages
build('./stuart')
