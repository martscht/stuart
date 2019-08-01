README
======

This repository contains the current alpha-build of the STUART package
for R. The name suggests, what the package is made for: *s*ub*t*ests
*u*sing *a*lgorithmic *r*ummaging *t*echniques. It is intended for the
creation of short-forms of questionnaires in a multitude of situations
including multiple facets, multiple groups, multiple measurement
occasions, and multiple sources of information. A full vignette is
currently in the works - for some of the theoretical and technical stuff
you can [take a look at the dissertation I
wrote](https://refubium.fu-berlin.de/handle/fub188/2951) - but this
readme will provide a short guide on using the package.

Installation
------------

The easiest way to install the current version of STUART is via the use
of the `install_bitbucket()`-function included in the `devtools`
package. To install the *stable* version use:

    devtools::install_bitbucket('martscht/stuart/stuart')

I will do my very best to ensure that this version is the same as the
one that you can find on CRAN. If you’re feeling a bit more
experimental, you can install the development build by setting
`ref='develop'`, like so:

    devtools::install_bitbucket('martscht/stuart/stuart', ref = 'develop')

    library(stuart)

    ## Warning: This is a beta-build of stuart. Please report any bugs you encounter.

After installation the easiest way to get an overview of STUARTs
functions and capabilities is to use `?stuart` to open the package
help-file. You could also read the rest of this README for an
introduction and some examples.

Prerequisites
-------------

The core idea behind STUART is to perform item-selection not based on
univariate propoerties of the items, but to instead use information
about the quality of a constructed solution for this task. In this
sense, all constructed solutions are viewed through the lense of
confirmatory factor analysis (CFA) which is used to relate the items to
latent constructs which are assumed to be the cause of observable
behavior. To perform CFA on of two additional software components is
required: either the [R-Package lavaan](http://lavaan.ugent.be/) or the
[commercial software Mplus](http://statmodel.com/). Because the choice
which of these to use is yours, neither of them installed when
installing STUART, so you will need to do so manually, befor being able
to use this package. If you intend to use Mplus, you will also need to
install the [R-Package
MplusAutomation](https://cran.r-project.org/web/packages/MplusAutomation/index.html),
so STUART can interface with the Mplus output.

#### WARNING: While both software solutions are implemented, as of STUART Version 0.8.0 it is highly recommended to use lavaan, if possible. This is due to the current MplusAutomation-based implementation of using Mplus is much slower than the current lavaan implementation (by factors of around 20).

Features
--------

The current STUART version (0.8.0) provides four approaches to item
selection, each in its own function:

-   `mmas()`, an adaptation of Stützle’s (1998) ℳ𝒜𝒳 − ℳℐ𝒩-Ant-System,
-   `gene()`, a basic genetic algorithm based on the ideas of Galán,
    Mengshoel, & Pinter (2013),
-   `bruteforce()`, the brute-force approach of simply trying all
    possible combinations, and
-   `randomsamples()`, a simple random sample of all possible
    combinations.

Before picking any of these, it is best to think about what it is that
you are trying to achieve and what kind of situation you are in. If you
are simply tring to determine what an average solution might look like
(in terms of scale properties) it is best to use `randomsamples()`. If
you are trying to find the best possible solution your next depends
primarily on the number of possible solutions. You can determine this
with `combinations()`. If the number is sufficiently small (or you are
willing wait sufficiently long) it is recommended to use `bruteforce()`,
because this guarantess that you will find the optimal solution in your
data. Which number passes as “sufficiently small” is up to you and
depends primarily on how long the estimation of a single CFA takes on
your computer. My ballpark recommendation is about 200 000 for a regular
CFA model with multiple facets. If one of these takes about a quarter
second to run, this should be done in about 2 hours on a decently
performing 8-core machine. If there are substantially more possible
combinations of items that could be tested in the process of item
selection, it is advisable to use either `mmas()` or `gene()`. The
`mmas()` approach has been thoroughly scrutinized ([by
me](https://refubium.fu-berlin.de/handle/fub188/2951)), while the
`gene()` approach is currently going through this process. Preliminary
results suggest that there is little difference in the performance of
these two approaches, so my current recommendation would be to use the
one you feel more comfortable with - Ant Colony Optimzation or Genetic
Algorithms.

Because all of these approaches to item selection require some form of
empirical data, there is bound to be uncertainty associated with the
final solutions. To remedy the resulting insecurities about your
solution a bit, there is also `crossvalidate()`, which allows you to
check the quality of the final solution in a holdout sample by checking
measurement invariance between the initial (calibration) sample and the
second (validation) sample. To help you with splitting your data into
these two sub-samples, STUART comes with the `holdout()` function.

If you have experience with older versions of STUART, the main new
feature in 0.8.0 is the possibility of using ordinal indicators - either
as the only type of items, or in combination with metric indicators.
Thus, you should be able to use STUART for item selection with almost
any type of questionnaire item-pool and have it generate the models,
which include your assumptions about measurement invariance across time,
groups, sources of information, and items automatically.

If you have not yet collected your own data, or simply want to play
around with the features of STUART, there are also two example datasets:
`fairplayer` and `sups`. The former includes multiple groups, multiple
occasions, multiple constructs, and multiple sources of information,
making it a perfect toy-example.

Examples
--------

In this section I will provide some small examples. These are not
exhaustive for all possible strategies which can be employed with
STUART, but should provide some insight into using its features. All
examples use the `fairplayer` dataset provided in the package:

    data(fairplayer)

This dataset contains information about `{r} nrow(fairplayer)` students
on `{r} ncol(fairplayer)` variables. The bulk of these variables are
items regarding empathy (EM), social intelligence (SI), and relational
aggression (RA). Each item was presented to the students themselves (s)
as well as their teacher (t) at three separate occasions (t1, t2, t3).
Thus the names of the variables in the dataset encode this information,
for example `sRA03t2` is the self-rated relational aggression on the
third item at the second measurement occasion.

### A minimal example

In the `fairplayer` dataset, relational aggression was measured with
five items. Let’s say (as a minimal example) we want to find the optimal
three-item short version of this scale. For this we need to provide
STUART with some information about which items constitute the original
item-pool from which to choose. This information is stored in a `list`
and prodvided to any STUART-function as the `factor.structure` argument.
In this case:

    fs <- list(RA = c('sRA01t1', 'sRA02t1', 'sRA03t1', 'sRA04t1', 'sRA05t1'))

This list contains only a single vector (because we are looking at only
one facet at only occasion measured by only one source of information).
Each element of the list needs to be named, because this name is used in
the CFA models as the name of the latent variable. In this case I chose
“RA” to indicate that we’re looking at relational aggression. The
content of each element of the list is a character vector containing the
names of the items that constitute the item-pool for this latent
variable.

A quick glance at your scribbled notes from the intro to stats course
and some quick calculating tells you that there are 10 possible
combinations in this case, where we are drawing 3 items from a pool of 5
items (the order is irrelevant, because simple CFA models are covariance
equivalent across all orders of the indicators). This means, that this
consitutes an appropriate time to use the `bruteforce()` function:

    sel <- bruteforce(data = fairplayer, factor.structure = fs, capacity = 3)

    ## Loading required namespace: lavaan

    ## Loading required namespace: parallel

    ## There are 10 combinations that need to be tested.

    ## Generating all possible combinations.

    ## Running STUART with Brute-Force.

    ## 
      |                                                                       
      |                                                                 |   0%

    ## 
    ## Search ended.

The `bruteforce()`-function (as do the other three item-selection
functions of STUART) requires a minimum of three arguments:

-   `data`: the dataset you are using,
-   `factor.structure`: the list assigning items to their facets, and
-   `capacity`: the number of items you want to select.

There are (many) more arguments you *can* use, but these three are
absolutely necessary. What this function should return is the three
items contained in what is deemed the optimal solution in accordance to
the preset objective:

    sel

    ## $RA
    ## [1] "sRA02t1" "sRA04t1" "sRA05t1"

This object is of the class `stuartOutput` and contains seven elements.
As with so many objects in R, `summary()` provides us with some more
information about what happened:

    summary(sel)

    ## Warning: This is a beta-build of stuart. Please report any bugs you encounter.

    ## SUMMARY OF ANALYSIS:
    ## 
    ## Analysis Type: bruteforce 
    ## Estimation Software: lavaan 
    ## Models estimated: 10 
    ## Replications of final solution: 1 
    ## Time Required: 0.617 seconds
    ## 
    ## Optimization History:
    ##   run pheromone chisq df pvalue rmsea         srmr      crel
    ## 1   1  1.867268     0  0     NA     0 3.325637e-09 0.7917421
    ## 2   2  1.892558     0  0     NA     0 5.116822e-09 0.8165856
    ## 9   9  1.905564     0  0     NA     0 1.509157e-08 0.8315434
    ## 
    ## Constructed Subtests:
    ## RA: sRA02t1 sRA04t1 sRA05t1

The summary provides us with information about the approach we used
(`bruteforce` in this case), the software that was used to estimate the
CFAs (`lavaan`, per default) the total number of models that was
estimated (`10`), the number of times the best solution was replicated
(when using `bruteforce` this should always be `1`), and the total time
it took to perform the selection. The next section is a table containing
the optimization history. This table shows the value a solution achieved
on the objective function (labeled `pheromone`, because of the packages
ACO roots) and the values these solutions had on all the components
included in the arguments of the objective function. Because we did not
provide a specific objective in this case, the preset was used. Take a
look at the preset:

    stuart:::objective.preset

    ## function (chisq, df, pvalue, rmsea, srmr, crel) 
    ## {
    ##     1/(1 + exp(6 - 10 * (crel))) + 0.5 * (1 - (1/(1 + exp(5 - 
    ##         100 * rmsea)))) + 0.5 * (1 - (1/(1 + exp(6 - 100 * srmr))))
    ## }
    ## <bytecode: 0x563e822a7178>
    ## <environment: namespace:stuart>

As you can see, per default the quality of a solution is determined by a
sum of logistic functions incorporating `crel` (composite reliability,
which is computed as McDonald’s *ω*), the RMSEA, and the SRMR. Because
our scale has one latent variable and only three items, model fit will
always be perfect, so it the objective simplified to the search for the
most reliable three-item scale.

Note that the optimization history contains only three solutions and not
all ten. That is because the optimization will report a solution only if
it is the best solution found at that point in the optimization process.
This means that the first solution will (almost) always be the first in
this list, while any subsequent solution will only be listed if it is
better than all those previously listed.

If you want to see the full list of solutions that were generated, one
of the seven elements of `stuartOutput`s is the `log`:

    sel$log

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

The solutions that contain `NA` on all variables were deemed
inadmissable solutions. This occurs when the CFA leads to errors in
estimation (e.g. non-convergence) or problems with estimated paramaters
(e.g. negative variances). Such solutions are excluded by default, but
the latter type of solutions can be included by using
`ignore.errors = TRUE`.
