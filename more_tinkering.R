library(devtools)

setwd('~/stuart')
load_all('./stuart')
source('empobjective.R')

### simple example ----
data(sups)
fs <- list(cp = names(sups)[2:13],
  fe = names(sups)[14:19])

combinations(sups, fs, 4)

# for beta in search, it must be included in random samples
obj <- function(chisq, df, pvalue, rmsea, srmr, crel, beta) {
  tmp <- stuart:::objective.preset(chisq, df, pvalue, rmsea, srmr, crel)
  tmp + beta[1,2]
}

# add regresion to model
add <- 'cp ~ fe'

sel <- randomsamples(sups, fs, 4,
  n = 1000,
  analysis.options = list(model = add),
  objective = obj)

head(sel$log)
sel$log_mat[[1]][,,1:5]

### extract some empirical objectives ----

# rmsea (skewed normal, lower is better)
hist(sel$log$rmsea[sel$log$rmsea < quantile(sel$log$rmsea, .25)])
rmsea <- empobjective(sel$log$rmsea, proportion = .25, side = 'bottom', skew = TRUE)
rmsea$string
curve(rmsea$func(x), xlim = c(0, .1))

# composite reliability (skewed normal, higher is better)
hist(sel$log$crel[sel$log$crel > quantile(sel$log$crel, .75)])
crel <- empobjective(sel$log$crel, proportion = .25, side = 'top', skew = TRUE)
crel$string
curve(crel$func(x), xlim = c(.78, .86))

# regression weight (normal, central is better)
tmp <- sel$log_mat[[1]][1, 2, ]
hist(tmp[tmp > quantile(tmp, .375) & tmp < quantile(tmp, .625)])
beta <- empobjective(tmp, proportion = .25, side = 'center')
beta$string
curve(beta$func(x), xlim = c(.75, 1))

### use in own objective function ----
obj2 <- function(chisq, df, pvalue, rmsea, srmr, crel, beta) {
  1 * (1 - sn::psn(rmsea, 0.0726129537576523, 0.0168665779095395, -183.446148148375)) +
    1 * sn::psn(crel, 0.789200200116067, 0.0203335040549927, 183.446148148375) +
    1 * 2 * ifelse(beta[1,2] > 0.86781251338366, pnorm(x, 0.86781251338366, 0.0322817947458314, lower.tail = FALSE), pnorm(x, 0.86781251338366, 0.0322817947458314, lower.tail = TRUE))
}

sel2 <- gene(sups, fs, 4,
  analysis.options = list(model = add),
  objective = obj2)
summary(sel2)
