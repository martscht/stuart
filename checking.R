library(stuart)

data(fairplayer)
names(fairplayer)

fs <- list(SI1=names(fairplayer)[2:11],
  SI2=names(fairplayer[12:21]))

repme <- list(SI=c('SI1','SI2'))

mmas_mplus <- mmas(fairplayer, fs, 1, 3, repeated.measures=repme, pbest=.1, software='Mplus')
mmas_lavaan <- mmas(fairplayer, fs, 1, 3,  repeated.measures=repme, pbest=.1, software='lavaan')
bf_mplus <- bruteforce(fairplayer, fs, 1, 3, repeated.measures=repme, software='Mplus')
bf_lavaan <- bruteforce(fairplayer, fs, 1, 3, repeated.measures=repme, software='lavaan')

crossvalidate(mmas_mplus,fairplayer[1:45,],fairplayer,invariance='strict')
crossvalidate(bf_mplus,fairplayer[1:45,],fairplayer,invariance='strict')
