---
output: 
  md_document: 
    variant: markdown_github
---

# README #

This repository contains the current alpha-build of the STUART package for R. The name suggests, what the package is made for: *s*ub*t*ests *u*sing *a*lgorithmic *r*ummaging *t*echniques. It is intended for the creation of short-forms of questionnaires in a multitude of situations including multiple facets, multiple groups, multiple measurement occasions, and multiple sources of information. A full vignette is currently in the works - for some of the theoretical and technical stuff you can [take a look at the dissertation I wrote](https://refubium.fu-berlin.de/handle/fub188/2951) - but this readme will provide a short guide on using the package.

## Installation ##

The easiest way to install the current version of STUART is via the use of the `install_bitbucket()`-function included in the `devtools` package. To install the *stable* version use:


```r
devtools::install_bitbucket('martscht/stuart/stuart')
```

I will do my very best to ensure that this version is the same as the one that you can find on CRAN. If you're feeling a bit more experimental, you can install the development build by setting `ref='develop'`, like so:


```r
devtools::install_bitbucket('martscht/stuart/stuart', ref = 'develop')
```


```r
library(stuart)
```

After installation the easiest way to get an overview of STUARTs functions and capabilities is to use `?stuart` to open the package help-file. You could also read the rest of this README for an introduction and some examples.

## Prerequisites

The core idea behind STUART is to perform item-selection not based on univariate propoerties of the items, but to instead use information about the quality of a constructed solution for this task. In this sense, all constructed solutions are viewed through the lense of confirmatory factor analysis (CFA) which is used to relate the items to latent constructs which are assumed to be the cause of observable behavior. To perform CFA on of two additional software components is required: either the [R-Package lavaan](http://lavaan.ugent.be/) or the [commercial software Mplus](http://statmodel.com/). Because the choice which of these to use is yours, neither of them installed when installing STUART, so you will need to do so manually, befor being able to use this package. If you intend to use Mplus, you will also need to install the [R-Package MplusAutomation](https://cran.r-project.org/web/packages/MplusAutomation/index.html), so STUART can interface with the Mplus output.

**WARNING: While both software solutions are implemented, as of STUART Version 0.8.0 it is highly recommended to use lavaan, if possible. This is due to the current MplusAutomation-based implementation of using Mplus is much slower than the current lavaan implementation (by factors of around 20).**

## Features

The current STUART version (0.8.0) provides four approaches to item selection, each in its own function:
  
  * `mmas()`, an adaptation of Stützle's (1998) $\mathcal{MAX}-\mathcal{MIN}$-Ant-System,
  * `gene()`, a basic genetic algorithm based on the ideas of Galán, Mengshoel, \& Pinter (2013),
  * `bruteforce()`, the brute-force approach of simply trying all possible combinations, and
  * `randomsamples()`, a simple random sample of all possible combinations.
  
Before picking any of these, it is best to think about what it is that you are trying to achieve and what kind of situation you are in. If you are simply tring to determine what an average solution might look like (in terms of scale properties) it is best to use `randomsamples()`. If you are trying to find the best possible solution your next depends primarily on the number of possible solutions. You can determine this with `combinations()`. If the number is sufficiently small (or you are willing wait sufficiently long) it is recommended to use `bruteforce()`, because this guarantess that you will find the optimal solution in your data. Which number passes as "sufficiently small" is up to you and depends primarily on how long the estimation of a single CFA takes on your computer. My ballpark recommendation is about 200 000 for a regular CFA model with multiple facets. If one of these takes about a quarter second to run, this should be done in about 2 hours on a decently performing 8-core machine. If there are substantially more possible combinations of items that could be tested in the process of item selection, it is advisable to use either `mmas()` or `gene()`. The `mmas()` approach has been thoroughly scrutinized ([by me](https://refubium.fu-berlin.de/handle/fub188/2951)), while the `gene()` approach is currently going through this process. Preliminary results suggest that there is little difference in the performance of these two approaches, so my current recommendation would be to use the one you feel more comfortable with - Ant Colony Optimzation or Genetic Algorithms.

Because all of these approaches to item selection require some form of empirical data, there is bound to be uncertainty associated with the final solutions. To remedy the resulting insecurities about your solution a bit, there is also `crossvalidate()`, which allows you to check the quality of the final solution in a holdout sample by checking measurement invariance between the initial (calibration) sample and the second (validation) sample. To help you with splitting your data into these two sub-samples, STUART comes with the `holdout()` function.

If you have experience with older versions of STUART, the main new feature in 0.8.0 is the possibility of using ordinal indicators - either as the only type of items, or in combination with metric indicators. Thus, you should be able to use STUART for item selection with almost any type of questionnaire item-pool and have it generate the models, which include your assumptions about measurement invariance across time, groups, sources of information, and items automatically.

If you have not yet collected your own data, or simply want to play around with the features of STUART, there are also two example datasets: `fairplayer` and `sups`. The former includes multiple groups, multiple occasions, multiple constructs, and multiple sources of information, making it a perfect toy-example.

## Examples

In this section I will provide some small examples. These are not exhaustive for all possible strategies which can be employed with STUART, but should provide some insight into using its features. All examples use the `fairplayer` dataset provided in the package:


```r
data(fairplayer)
```

This dataset contains information about 143 students on 142 variables. The bulk of these variables are items regarding empathy (EM), social intelligence (SI), and relational aggression (RA). Each item was presented to the students themselves (s) as well as their teacher (t) at three separate occasions (t1, t2, t3). Thus the names of the variables in the dataset encode this information, for example `sRA03t2` is the self-rated relational aggression on the third item at the second measurement occasion.

The currently available examples are shown in the following table. If there is a specific example you would like to see, please either contact me directly or simply [file an issue](https://bitbucket.org/martscht/stuart/issues?status=new&status=open).

Example | Approach | Multiple Facets | Multiple Occasions | Multiple Groups | Multiple Sources | Comments |
--- | --- | --- |--- | --- | --- | --- |
[Minimal](#ex1_minimal) | `bruteforce` |  |  |  |  |  |  |
[Multiple Facets](#ex2_gene) | `gene` | X |  |  |  |  |  |
[Setting Anchors](#ex3_mmas) | `mmas` | X |  |  |  |  |  |



### A minimal example {#ex1_minimal}

In the `fairplayer` dataset, relational aggression was measured with five items. Let's say (as a minimal example) we want to find the optimal three-item short version of this scale. For this we need to provide STUART with some information about which items constitute the original item-pool from which to choose. This information is stored in a `list` and prodvided to any STUART-function as the `factor.structure` argument. In this case:


```r
fs <- list(RA = c('sRA01t1', 'sRA02t1', 'sRA03t1', 'sRA04t1', 'sRA05t1'))
```

This list contains only a single vector (because we are looking at only one facet at only occasion measured by only one source of information). Each element of the list needs to be named, because this name is used in the CFA models as the name of the latent variable. In this case I chose "RA" to indicate that we're looking at relational aggression. The content of each element of the list is a character vector containing the names of the items that constitute the item-pool for this latent variable.

A quick glance at your scribbled notes from the intro to stats course and some quick calculating tells you that there are 10 possible combinations in this case, where we are drawing 3 items from a pool of 5 items (the order is irrelevant, because simple CFA models are covariance equivalent across all orders of the indicators). This means, that this consitutes an appropriate time to use the `bruteforce()` function:


```r
sel <- bruteforce(data = fairplayer, factor.structure = fs, capacity = 3)
```

```
## There are 10 combinations that need to be tested.
```

```
## Generating all possible combinations.
```

```
## Running STUART with Brute-Force.
```

```
##   |                                                                         |                                                                 |   0%
```

```
## Progressbars are not functional when utilizing multiple cores for bruteforce in RStudio.
```

```
## 
## Search ended.
```

The `bruteforce()`-function (as do the other three item-selection functions of STUART) requires a minimum of three arguments:

  * `data`: the dataset you are using,
  * `factor.structure`: the list assigning items to their facets, and
  * `capacity`: the number of items you want to select.
  
There are (many) more arguments you *can* use, but these three are absolutely necessary. What this function should return is the three items contained in what is deemed the optimal solution in accordance to the preset objective:


```r
sel
```

```
## $RA
## [1] "sRA02t1" "sRA04t1" "sRA05t1"
```

This object is of the class `stuartOutput` and contains seven elements. As with so many objects in R, `summary()` provides us with some more information about what happened:


```r
summary(sel)
```

```
## Warning: This is a beta-build of stuart. Please report any bugs you encounter.
```

```
## SUMMARY OF ANALYSIS:
## 
## Analysis Type: bruteforce 
## Estimation Software: lavaan 
## Models estimated: 10 
## Replications of final solution: 1 
## Time Required: 0.341 seconds
## 
## Optimization History:
##   run pheromone chisq df pvalue rmsea         srmr      crel
## 1   1  1.867268     0  0     NA     0 3.325637e-09 0.7917421
## 2   2  1.892558     0  0     NA     0 5.116822e-09 0.8165856
## 9   9  1.905564     0  0     NA     0 1.509157e-08 0.8315434
## 
## Constructed Subtests:
## RA: sRA02t1 sRA04t1 sRA05t1
```

The summary provides us with information about the approach we used (`bruteforce` in this case), the software that was used to estimate the CFAs (`lavaan`, per default) the total number of models that was estimated (`10`), the number of times the best solution was replicated (when using `bruteforce` this should always be `1`), and the total time it took to perform the selection. The next section is a table containing the optimization history. This table shows the value a solution achieved on the objective function (labeled `pheromone`, because of the packages ACO roots) and the values these solutions had on all the components included in the arguments of the objective function. Because we did not provide a specific objective in this case, the preset was used. Take a look at the preset:


```r
stuart:::objective.preset
```

```
## function (chisq, df, pvalue, rmsea, srmr, crel) 
## {
##     1/(1 + exp(6 - 10 * (crel))) + 0.5 * (1 - (1/(1 + exp(5 - 
##         100 * rmsea)))) + 0.5 * (1 - (1/(1 + exp(6 - 100 * srmr))))
## }
## <bytecode: 0x560e0a1b5010>
## <environment: namespace:stuart>
```

As you can see, per default the quality of a solution is determined by a sum of logistic functions incorporating `crel` (composite reliability, which is computed as McDonald's $\omega$), the RMSEA, and the SRMR. Because our scale has one latent variable and only three items, model fit will always be perfect, so it the objective simplified to the search for the most reliable three-item scale.

Note that the optimization history contains only three solutions and not all ten. That is because the optimization will report a solution only if it is the best solution found at that point in the optimization process. This means that the first solution will (almost) always be the first in this list, while any subsequent solution will only be listed if it is better than all those previously listed.

If you want to see the full list of solutions that were generated, one of the seven elements of `stuartOutput`s is the `log`:


```r
sel$log
```

```
##    run pheromone        chisq df pvalue rmsea         srmr      crel
## 1    1  1.867268 0.000000e+00  0     NA     0 3.325637e-09 0.7917421
## 2    2  1.892558 0.000000e+00  0     NA     0 5.116822e-09 0.8165856
## 3    3  0.000000           NA NA     NA    NA           NA        NA
## 4    4  1.594282 1.387779e-13  0     NA     0 9.797503e-09 0.6400738
## 5    5  1.248296 0.000000e+00  0     NA     0 7.551628e-09 0.4916680
## 6    6  0.000000           NA NA     NA    NA           NA        NA
## 7    7  1.868069 1.110223e-13  0     NA     0 4.607573e-09 0.7924608
## 8    8  1.821704 0.000000e+00  0     NA     0 2.357105e-08 0.7559534
## 9    9  1.905564 0.000000e+00  0     NA     0 1.509157e-08 0.8315434
## 10  10  1.821323 0.000000e+00  0     NA     0 1.115970e-08 0.7556883
```

The solutions that contain `NA` on all variables were deemed inadmissable solutions. This occurs when the CFA leads to errors in estimation (e.g. non-convergence) or problems with estimated paramaters (e.g. negative variances). Such solutions are excluded by default, but the latter type of solutions can be included by using `ignore.errors = TRUE`.


### Multiple Facets {#ex2_gene}

The previous example was limited to a single facet. In this example, we will take a look at something a bit more complex, which likely constitutes the most common situation. In this example we will look at empathy, relational aggression, and social intelligence simultaneously. The example is a bit lacking, because these three constitute different constructs and not different facets of a single construct, as would most often be the case in scale construction.

As was the case in the previous example, the first thing we need to do is set up the factor structure in a list that links items to their facets:


```r
fs <- list(EM = names(fairplayer)[5:12],
  RA = names(fairplayer)[53:57],
  SI = names(fairplayer)[83:92])
fs
```

```
## $EM
## [1] "sEM01t1" "sEM02t1" "sEM03t1" "sEM04t1" "sEM05t1" "sEM06t1" "sEM07t1"
## [8] "sEM08t1"
## 
## $RA
## [1] "sRA01t1" "sRA02t1" "sRA03t1" "sRA04t1" "sRA05t1"
## 
## $SI
##  [1] "sSI01t1" "sSI02t1" "sSI03t1" "sSI04t1" "sSI05t1" "sSI06t1" "sSI07t1"
##  [8] "sSI08t1" "sSI09t1" "sSI10t1"
```

In this case we use all items measured via self-reports at the first measurement occasion. The `fs`-object is a list of 3, where each element of the list represents a facet of the questionnaire. For example, the first element is named `EM` to represent the assessment of Empathy and contains the names of the 8 items that were used in this case.

Say we wanted 3 items for empathy, 3 items for relational, and 4 items for social intelligence to have a ten-item questionnaire at the end of item selection. This can be achieved by defining a list with the number of items per facet. Of course, these numbers must be in the same order as the factor structure, so they can be aligned.


```r
ni <- list(3, 3, 4)
```

To compute the number of possible combinations, we can use the convenience function `combinations()`:


```r
combinations(fairplayer, fs, ni)
```

```
## [1] 117600
```

In a real-world setting I would recommend running `bruteforce()` with this number of possible combinations. However, because this is an example, we will try something different than in the last example, by using the genetic algorithm that is implemented in `gene()`.

Just like in the previous example, only three arguments are strictly necessary: the dataset, the factor structure, and the number of items. To generate reproducible results we can also use the additional argument `seed` to provide a random seed:


```r
sel <- gene(fairplayer, fs, ni, seed = 35355)
```


```r
Running STUART with Genetic Algorithm.

  |==============================================                                   |  55%

Search ended. Algorithm converged.
```



An important piece of information here is that the algorithm converged. In this approach this means that the quality of the best solutions per generation showed minimal variation after some time. The alternative would have been for the algorithm to abort after 128 generations (per default), if convergence would not have been reached by then. Again, let us take a look at the summary to view the results in detail:


```r
summary(sel)
```

```
## Warning: This is a beta-build of stuart. Please report any bugs you encounter.
```

```
## SUMMARY OF ANALYSIS:
## 
## Analysis Type: gene 
## Estimation Software: lavaan 
## Models estimated: 4544 
## Replications of final solution: 2307 
## Time Required: 42.297 seconds
## 
## Optimization History:
##     run ind pheromone    chisq df       pvalue      rmsea       srmr
## 1     1   1 0.9714857 67.74143 32 0.0002288957 0.09415116 0.07756107
## 3     1   3 1.0069370 63.16413 32 0.0008310642 0.08791586 0.07190684
## 5     1   5 1.4117306 43.02228 32 0.0923156853 0.05228479 0.05556138
## 12    1  12 1.7310307 28.20980 32 0.6589455342 0.00000000 0.04848657
## 51    1  51 1.7379274 30.93543 32 0.5202934365 0.00000000 0.04694889
## 73    2   9 1.7526648 23.94351 32 0.8464530294 0.00000000 0.04651070
## 111   2  47 1.7914778 28.90732 32 0.6238659074 0.00000000 0.04620266
## 149   3  21 1.8175800 26.60508 32 0.7362439152 0.00000000 0.04508053
## 187   3  59 1.8200085 28.65614 32 0.6365675485 0.00000000 0.04319827
## 300   5  44 1.8377925 23.35948 32 0.8666563405 0.00000000 0.03650133
## 324   6   4 1.8404997 28.88624 32 0.6249343199 0.00000000 0.04410038
## 352   6  32 1.8562645 19.46001 32 0.9600125185 0.00000000 0.03740006
## 395   7  11 1.8579455 17.42704 32 0.9830191354 0.00000000 0.03811721
##          crel
## 1   0.8110008
## 3   0.7986173
## 5   0.8046341
## 12  0.7770333
## 51  0.7718433
## 73  0.7807289
## 111 0.8146595
## 149 0.8347574
## 187 0.8218417
## 300 0.8037487
## 324 0.8564487
## 352 0.8275841
## 395 0.8334521
## 
## Constructed Subtests:
## EM: sEM01t1 sEM02t1 sEM07t1
## RA: sRA02t1 sRA04t1 sRA05t1
## SI: sSI01t1 sSI02t1 sSI07t1 sSI08t1
```

As you can see, the search took 42.297 seconds and estimated 4544 models. As is bound to happen in this specific genetic approach, the final solution was replicated quite often. Replications of solutions are not estimated again, which is why you should have been able to observe the search process speeding up towards the end. The final solution (again, in terms of the preset objective function, which you can view via `stuart:::objective.preset`) had a pheromone of 1.858 stemming from an RMSEA of 0 and a composite reliability of 0.833.

You can, of course, also take a more detailed look at the final soultion. Per default, lavaan is used for CFA estimation, so the lavaan object of the final model is return in the `stuartOutput` object, specifically in the slot `final`. If you want to take an in-depth look at the lavaan results of this model, you can simply use the `summary` method implemented in lavaan:


```r
lavaan::summary(sel$final)
```

```
## lavaan 0.6-4 ended normally after 52 iterations
## 
##   Optimization method                           NLMINB
##   Number of free parameters                         33
## 
##                                                   Used       Total
##   Number of observations                           126         143
##   Number of missing patterns                         9
## 
##   Estimator                                         ML
##   Model Fit Test Statistic                      17.427
##   Degrees of freedom                                32
##   P-value (Chi-square)                           0.983
## 
## Parameter Estimates:
## 
##   Information                                 Observed
##   Observed information based on                Hessian
##   Standard Errors                             Standard
## 
## Latent Variables:
##                    Estimate  Std.Err  z-value  P(>|z|)
##   EM =~                                               
##     sEM01t1 (l111)    1.000                           
##     sEM02t1 (l211)    1.786    0.300    5.957    0.000
##     sEM07t1 (l711)    1.156    0.211    5.480    0.000
##   RA =~                                               
##     sRA02t1 (l221)    1.000                           
##     sRA04t1 (l421)    1.578    0.242    6.529    0.000
##     sRA05t1 (l521)    0.666    0.104    6.411    0.000
##   SI =~                                               
##     sSI01t1 (l131)    1.000                           
##     sSI02t1 (l231)    0.936    0.209    4.470    0.000
##     sSI07t1 (l731)    1.029    0.235    4.369    0.000
##     sSI08t1 (l831)    0.990    0.240    4.124    0.000
## 
## Covariances:
##                    Estimate  Std.Err  z-value  P(>|z|)
##   EM ~~                                               
##     RA                0.082    0.040    2.053    0.040
##     SI                0.191    0.055    3.460    0.001
##   RA ~~                                               
##     SI                0.041    0.046    0.887    0.375
## 
## Intercepts:
##                    Estimate  Std.Err  z-value  P(>|z|)
##    .sEM01t1 (a111)    4.148    0.076   54.685    0.000
##    .sEM02t1 (a211)    3.937    0.086   45.759    0.000
##    .sEM07t1 (a711)    3.822    0.086   44.216    0.000
##    .sRA02t1 (a221)    1.693    0.084   20.252    0.000
##    .sRA04t1 (a421)    2.260    0.112   20.190    0.000
##    .sRA05t1 (a521)    1.422    0.069   20.472    0.000
##    .sSI01t1 (a131)    3.747    0.084   44.715    0.000
##    .sSI02t1 (a231)    4.007    0.071   56.151    0.000
##    .sSI07t1 (a731)    3.930    0.085   46.228    0.000
##    .sSI08t1 (a831)    2.875    0.090   31.820    0.000
##     EM                0.000                           
##     RA                0.000                           
##     SI                0.000                           
## 
## Variances:
##                    Estimate  Std.Err  z-value  P(>|z|)
##    .sEM01t1 (e111)    0.468    0.067    6.939    0.000
##    .sEM02t1 (e211)    0.126    0.089    1.406    0.160
##    .sEM07t1 (e711)    0.598    0.085    7.047    0.000
##    .sRA02t1 (e221)    0.367    0.079    4.661    0.000
##    .sRA04t1 (e421)    0.308    0.161    1.912    0.056
##    .sRA05t1 (e521)    0.377    0.055    6.789    0.000
##    .sSI01t1 (e131)    0.596    0.092    6.495    0.000
##    .sSI02t1 (e231)    0.378    0.064    5.880    0.000
##    .sSI07t1 (e731)    0.588    0.093    6.322    0.000
##    .sSI08t1 (e831)    0.737    0.109    6.781    0.000
##     EM                0.253    0.077    3.305    0.001
##     RA                0.503    0.121    4.169    0.000
##     SI                0.283    0.097    2.914    0.004
```

As you can see, a lot of parameters are labeled automatically. This is because these labels are used to implement invariance assumptions in more complex situations.


### Setting Anchor Items {#ex3_mmas}

In many situations, specific items are so central to the definition of a construct that they must be included in the final questionnaire. In such cases it is best to select items which fit around these anchor items. When using the `mmas()` approach to item selection this can be handled via *heuristics*. In the ACO approach underlying the `mmas()` function the probability of selecting an item when a potential solution is constructed is dependent on two factors: the pheromone (i.e. the extent to which an item has proven itself useful in prior solutions) and the heuristic information (i.e. information that is provided before beginning the construction of solutions). Per default, each item has its own pheromone and its own heuristic information - this is called *node localization*. A [later example]() shows the alternative - *arc localization* - but let us focus on the default case for now. To incorporate anchor items, we need to provide heuristic information that makes the selection of these items basically certain.

Let us look at a situation in which we are interested in selecting items for three separate facets of the same questionnaire. As described in [Example 2](#ex2_gene) this requires a factor structure with three elements linking items to their respective facets:


```r
fs <- list(EM = names(fairplayer)[5:12],
  RA = names(fairplayer)[53:57],
  SI = names(fairplayer)[83:92])
fs
```

```
## $EM
## [1] "sEM01t1" "sEM02t1" "sEM03t1" "sEM04t1" "sEM05t1" "sEM06t1" "sEM07t1"
## [8] "sEM08t1"
## 
## $RA
## [1] "sRA01t1" "sRA02t1" "sRA03t1" "sRA04t1" "sRA05t1"
## 
## $SI
##  [1] "sSI01t1" "sSI02t1" "sSI03t1" "sSI04t1" "sSI05t1" "sSI06t1" "sSI07t1"
##  [8] "sSI08t1" "sSI09t1" "sSI10t1"
```

Assuming we want to select three items for each of the facets, we can compute the number of possible combinations:


```r
combinations(fairplayer, fs, 3)
```

```
## [1] 67200
```

To use the `mmas()` function it is necessary to provide values to the arguments `data`, `factor.structure`, and `capacity` - much like in the previous examples. To use heuristics, we also need to provide those to the `heuristics` argument. These heuristics need to have a specific format for `mmas()` to understand what is happening, but thankfully there is a `heuristics()` function, which generates a preset in the correct format:


```r
heu <- heuristics(fairplayer, fs, 3)
heu
```

```
## $EM
##      sEM01t1 sEM02t1 sEM03t1 sEM04t1 sEM05t1 sEM06t1 sEM07t1 sEM08t1
## [1,]       1       1       1       1       1       1       1       1
## 
## $RA
##      sRA01t1 sRA02t1 sRA03t1 sRA04t1 sRA05t1
## [1,]       1       1       1       1       1
## 
## $SI
##      sSI01t1 sSI02t1 sSI03t1 sSI04t1 sSI05t1 sSI06t1 sSI07t1 sSI08t1
## [1,]       1       1       1       1       1       1       1       1
##      sSI09t1 sSI10t1
## [1,]       1       1
## 
## attr(,"class")
## [1] "stuartHeuristics"
## attr(,"localization")
## [1] "nodes"
```

This object is of the class `stuartHeuristics`. As you can see, every item has `1` as its heuristic information. This information is multiplied with the items pheromone to generate the selection probability, so `1` means the selection procedure is based exclusively on pheromones. Using a `0` would exclude an item from the selection procedure completely (though it would probably be easier to simply not include it in the first place). Any number larger than 1 makes the selection of the item more probable than it would be based purely on its merit encoded in the pheromone. Thus, to use anchor items we need to set the heuristic information of those items to a very large number.

Let's say we want item 3 to be an anchor for empathy, we don't want any anchors for relational aggression, and we want items 1 and 8 as anchors for social intelligence. In this case we simply overwrite the default heuristic information with a very large number, say 1 million: 


```r
heu$EM[3] <- 1e+6
heu$SI[1] <- heu$SI[8] <- 1e+6
heu
```

```
## $EM
##      sEM01t1 sEM02t1 sEM03t1 sEM04t1 sEM05t1 sEM06t1 sEM07t1 sEM08t1
## [1,]       1       1   1e+06       1       1       1       1       1
## 
## $RA
##      sRA01t1 sRA02t1 sRA03t1 sRA04t1 sRA05t1
## [1,]       1       1       1       1       1
## 
## $SI
##      sSI01t1 sSI02t1 sSI03t1 sSI04t1 sSI05t1 sSI06t1 sSI07t1 sSI08t1
## [1,]   1e+06       1       1       1       1       1       1   1e+06
##      sSI09t1 sSI10t1
## [1,]       1       1
## 
## attr(,"class")
## [1] "stuartHeuristics"
## attr(,"localization")
## [1] "nodes"
```

This makes those items 1 million times more likely to be selected than items with equal pheromone. Because these items are always selected, their pheromone cannot evaporate, thus (practically) guaranteeing that these items will always be included. To run the `mmas()` algorithm we then need to include these updated heuristics in the function:


```r
sel <- mmas(fairplayer, fs, 3, heuristics = heu)
```


```r
Running STUART with MMAS.

  |                                                                                       |   0%
Global best no. 1 found. Colony counter reset.

Global best no. 2 found. Colony counter reset.
  |=====                                                                                  |   6%
Global best no. 3 found. Colony counter reset.
  |==                                                                                     |   3%
Global best no. 4 found. Colony counter reset.
  |====                                                                                   |   4%
Global best no. 5 found. Colony counter reset.
  |===                                                                                    |   4%
Global best no. 6 found. Colony counter reset.
  |=======================================================================================| 100%

Search ended. Maximum number of colonies exceeded.
```



In this case the algorithm did not converge, but reached its abort criterion. This is not necessarily a bad thing and we will look at potential reasons in a [later example](). For now, let's take a look at the solution for this case:


```r
summary(sel)
```

```
## Warning: This is a beta-build of stuart. Please report any bugs you encounter.
```

```
## SUMMARY OF ANALYSIS:
## 
## Analysis Type: mmas 
## Estimation Software: lavaan 
## Models estimated: 4816 
## Replications of final solution: 408 
## Time Required: 88.941 seconds
## 
## Optimization History:
##     run ant pheromone    chisq df    pvalue      rmsea       srmr
## 1     1   1  1.489002 28.72482 24 0.2306596 0.03952770 0.05713651
## 3     1   3  1.617417 21.68898 24 0.5978243 0.00000000 0.05026616
## 6     1   6  1.644714 24.43025 24 0.4372274 0.01192804 0.05293613
## 9     1   9  1.707683 22.14288 24 0.5707388 0.00000000 0.04856682
## 14    1  14  1.723117 19.47913 24 0.7259783 0.00000000 0.04430688
## 17    2   1  1.810053 18.82107 24 0.7614633 0.00000000 0.03818777
## 259  17   3  1.817149 17.83216 24 0.8110735 0.00000000 0.03495550
## 373  24   5  1.817977 22.69421 24 0.5379041 0.00000000 0.04050615
## 549  35   5  1.829930 20.92041 24 0.6434159 0.00000000 0.04098934
## 715  45  11  1.840193 22.63275 24 0.5415548 0.00000000 0.03917303
##          crel
## 1   0.7609413
## 3   0.7141021
## 6   0.7521617
## 9   0.7599045
## 14  0.7467127
## 17  0.7849972
## 259 0.7800985
## 373 0.8027304
## 549 0.8178153
## 715 0.8185441
## 
## Constructed Subtests:
## EM: sEM02t1 sEM03t1 sEM07t1
## RA: sRA02t1 sRA04t1 sRA05t1
## SI: sSI01t1 sSI02t1 sSI08t1
```

As you can see, the anchored items are all included in the final selection. In my case a total of 4816 models were estimated amounting to a runtime of close to 90 seconds. The final solution also seems to work quite nicely in terms of the objective: the RMSEA is 0, the SRMR is below any reasonable threshold for model fit and the composite reliability is quite high for such a short measure.
