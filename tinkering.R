library(devtools)
load_all('~/stuart/stuart')

data(fairplayer)

fs <- list(ra1 = names(fairplayer)[53:57],
  ra2 = names(fairplayer)[58:62],
  ra3 = names(fairplayer)[63:67])
reme <- list(ra = c('ra1', 'ra2', 'ra3'))

fairplayer[!is.na(fairplayer$sRA01t3) & (fairplayer$sRA01t3 == 11), 'sRA01t3'] <- 1

cmbnLevels <- function(x, extreme = FALSE) {
  y <- x
  if (extreme) {
    y[x==1] <- 2
    y[x==5] <- 4
  } else {
    y[x==4 | x==5] <- 3
  }
  
  y <- droplevels(y)
  return(y)
}

ords <- fairplayer[, names(fairplayer)%in%unlist(fs)]
ords <- lapply(ords, as.ordered)
ords <- as.data.frame(lapply(ords, cmbnLevels))

of <- function(rmsea, crel, delta.cfi.long, delta.pvalue.long) {
  1-rmsea + crel + 1-(delta.cfi.long) + as.numeric(delta.pvalue.long > .2)
}

sel <- bruteforce(ords, fs, 3, repeated.measures = reme, long.invariance = 'strong', cores = 1,
  comparisons = c('long'))
summary(sel)
