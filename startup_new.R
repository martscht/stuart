library(devtools)
library(roxygen2)

setwd('~/projects/stuart')
load_all('./stuart')

load('./stuart/data/fairplayer.rda')
load('./stuart/data/sups.rda')
load('./stuart/data/sia.rda')

# Generate documentation after changes in files
devtools::document('./stuart')

# Check package
check('./stuart', cran = TRUE, incoming = TRUE)

# build packages
build('./stuart')

# check built package
check_built('./stuart_0.10.3.tar.gz')
