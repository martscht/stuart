library(devtools)
load_all('~/stuart/stuart')

data(fairplayer)
set.seed(35355)

cmbnLevels <- function(x, extreme = FALSE) {
  y <- x
  if (extreme) {
    y[x==1] <- 2
    y[x==5] <- 4
  } else {
    y[x==1 | x==2] <- 3
  }
  
  y <- droplevels(y)
  return(y)
}


### Full checks ----

# All continuous, no extras
fs <- list(ssi1 = names(fairplayer)[83:92])

sel_mpl1 <- bruteforce(fairplayer, fs, 3, software = 'Mplus')
summary(sel_mpl1)

sel_lav1 <- bruteforce(fairplayer, fs, 3)
summary(sel_lav1)

# All continuous, repeated measures
fs <- list(ssi1 = names(fairplayer)[83:92],
  ssi2 = names(fairplayer)[93:102],
  ssi3 = names(fairplayer)[103:112])
reme <- list(ssi = c('ssi1', 'ssi2', 'ssi3'))

sel_mpl2 <- bruteforce(fairplayer, fs, 3, software = 'Mplus', repeated.measures = reme)
summary(sel_mpl2)

sel_lav2 <- bruteforce(fairplayer, fs, 3, repeated.measures = reme)
summary(sel_lav2)

# All continuous, mtmm
fs <- list(ssi1 = names(fairplayer)[83:92],
  tsi1 = names(fairplayer)[113:122])
mult <- list(si = c('ssi1', 'tsi1'))

sel_mpl3 <- bruteforce(fairplayer, fs, 3, software = 'Mplus', mtmm = mult)
summary(sel_mpl3)

sel_lav3 <- bruteforce(fairplayer, fs, 3, mtmm = mult)
summary(sel_lav3)

# All continuous, groups
fairplayer$g <- sample(c(0, 1), nrow(fairplayer), TRUE)

fs <- list(ssi1 = names(fairplayer)[83:92])

sel_mpl4 <- bruteforce(fairplayer, fs, 3, software = 'Mplus', grouping = 'g')
summary(sel_mpl4)

sel_lav4 <- bruteforce(fairplayer, fs, 3, grouping = 'g')
summary(sel_lav4)

# All continuous, combined
fs <- list(ssi1 = names(fairplayer)[83:92],
  ssi2 = names(fairplayer)[93:102],
  ssi3 = names(fairplayer)[103:112],
  tsi1 = names(fairplayer)[113:122],
  tsi2 = names(fairplayer)[123:132],
  tsi3 = names(fairplayer)[133:142])

reme <- list(ssi = c('ssi1', 'ssi2', 'ssi3'),
  tsi = c('tsi1', 'tsi2', 'tsi3'))

mult <- list(si1 = c('ssi1', 'tsi1'),
  si2 = c('ssi2', 'tsi2'),
  si3 = c('ssi3', 'tsi3'))

fairplayer$g <- sample(c(0, 1), nrow(fairplayer), TRUE)

sel_mpl5 <- bruteforce(fairplayer, fs, 3, software = 'Mplus', grouping = 'g',
  repeated.measures = reme, mtmm = mult)
summary(sel_mpl5)

sel_lav5 <- bruteforce(fairplayer, fs, 3, grouping = 'g',
  repeated.measures = reme, mtmm = mult)
summary(sel_lav5)


# All ordinal, no extras  
fs <- list(ssi1 = names(fairplayer)[83:92])

ords <- fairplayer[, names(fairplayer) %in% unlist(fs)]
ords <- lapply(ords, 'as.ordered')
ords <- lapply(ords, cmbnLevels)
ords <- as.data.frame(ords)

obj <- function(chisq, df, pvalue, rmsea, crel) {
  1 / (1 + exp(6 - 10 * (crel))) +
  (1 - (1 / (1 + exp(5 - 100 * rmsea))))
}

sel_mpl6 <- bruteforce(ords, fs, 3, software = 'Mplus', objective = obj)
summary(sel_mpl6)

sel_lav6 <- bruteforce(ords, fs, 3, objective = obj)
summary(sel_lav6)


# All ordinal, combined
fs <- list(ssi1 = names(fairplayer)[83:92],
  ssi2 = names(fairplayer)[93:102],
  ssi3 = names(fairplayer)[103:112],
  tsi1 = names(fairplayer)[113:122],
  tsi2 = names(fairplayer)[123:132],
  tsi3 = names(fairplayer)[133:142])

ords <- fairplayer[, names(fairplayer) %in% unlist(fs)]
ords <- lapply(ords, 'as.ordered')
ords[grep('^sSI', names(ords))] <- lapply(ords[grep('^sSI', names(ords))], cmbnLevels)
ords[grep('^tSI', names(ords))] <- lapply(ords[grep('^tSI', names(ords))], cmbnLevels, extreme = TRUE)
ords <- as.data.frame(ords)


reme <- list(ssi = c('ssi1', 'ssi2', 'ssi3'),
  tsi = c('tsi1', 'tsi2', 'tsi3'))

mult <- list(si1 = c('ssi1', 'tsi1'),
  si2 = c('ssi2', 'tsi2'),
  si3 = c('ssi3', 'tsi3'))

obj <- function(chisq, df, pvalue, rmsea, crel) {
  1 / (1 + exp(6 - 10 * (crel))) +
    (1 - (1 / (1 + exp(5 - 100 * rmsea))))
}

ords$g <- sample(c(0, 1), nrow(fairplayer), TRUE)

sel_mpl7 <- bruteforce(ords, fs, 3, software = 'Mplus', grouping = 'g',
  repeated.measures = reme, mtmm = mult, objective = obj)
summary(sel_mpl7)

sel_lav7 <- bruteforce(fairplayer, fs, 3, grouping = 'g',
  repeated.measures = reme, mtmm = mult)
summary(sel_lav7)

# CV on mixed data, repeated
fs <- list(ssi1 = names(fairplayer)[83:92],
  ssi2 = names(fairplayer)[93:102])

reme <- list(si = c('ssi1', 'ssi2'))

ords <- fairplayer[, names(fairplayer) %in% unlist(fs)[seq_along(unlist(fs))%%2==0]]
ords <- lapply(ords, 'as.ordered')
ords <- lapply(ords, cmbnLevels)
ords <- as.data.frame(ords)
ords <- cbind(ords, fairplayer[, names(fairplayer) %in% unlist(fs)[seq_along(unlist(fs))%%2==1]])

obj <- function(chisq, df, pvalue, rmsea, crel) {
  1 / (1 + exp(6 - 10 * (crel))) +
    (1 - (1 / (1 + exp(5 - 100 * rmsea))))
}

hold <- holdout(ords)

sel_mpl8 <- bruteforce(hold, fs, 3, software = 'Mplus', objective = obj, repeated.measures = reme)
summary(sel_mpl8)
crossvalidate(sel_mpl8, hold)

sel_lav8 <- bruteforce(hold, fs, 3, objective = obj, repeated.measures = reme, cores = 1)
summary(sel_lav8)
crossvalidate(sel_lav8, hold)

# CV on all ordinal data
fs <- list(ssi1 = names(fairplayer)[83:92],
  ssi2 = names(fairplayer)[93:102])

reme <- list(si = c('ssi1', 'ssi2'))

ords <- lapply(fairplayer[, unlist(fs)], 'as.ordered')
ords <- lapply(ords, cmbnLevels)
ords <- as.data.frame(ords)

obj <- function(chisq, df, pvalue, rmsea, crel) {
  1 / (1 + exp(6 - 10 * (crel))) +
    (1 - (1 / (1 + exp(5 - 100 * rmsea))))
}

hold <- holdout(ords)

sel_mpl9 <- bruteforce(hold, fs, 3, software = 'Mplus', objective = obj, repeated.measures = reme)
summary(sel_mpl9)
crossvalidate(sel_mpl9, hold)

sel_lav9 <- bruteforce(hold, fs, 3, objective = obj, repeated.measures = reme)
summary(sel_lav9)
crossvalidate(sel_lav9, hold)

# Using invariance checks in objective
fs <- list(ssi1 = names(fairplayer)[83:92],
  ssi2 = names(fairplayer)[93:102])

reme <- list(si = c('ssi1', 'ssi2'))

sel_mpl10 <- bruteforce(fairplayer, fs, 3, software = 'Mplus', repeated.measures = reme, comparisons = 'long')
summary(sel_mpl10)

sel_lav10 <- bruteforce(fairplayer, fs, 3, repeated.measures = reme, comparisons = 'long')
summary(sel_lav10)