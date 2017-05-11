---
output:
  html_document: default
  pdf_document: default
---
# README #

This repository contains the current alpha-build of the STUART package for R. It is intended for the creation of short-forms of questionnaires via an Ant-Colony-Optimization approach. For more details on the package please see the help files contained in the package itself.

#### WARNING: Scripts written for versions <= 0.6.2 (currently the stable branch) may be incompatible with the current delevopment versions 0.7.0-900x. This is due to changes in the naming of arguments.

## Installation ##

The easiest way to install the current version of STUART is via the use of the `install_bitbucket()`-function included in the devtools package. To install the *stable* version use:

        library(devtools)
        install_bitbucket('martscht/stuart/stuart')
        library(stuart)

This will install the version currently committed to the master branch. If you want to be a bit more edgy, you can install the development build by setting `ref='develop'`, like so:

        library(devtools)
        install_bitbucket('martscht/stuart/stuart',ref='develop')
        library(stuart)

After installation the easiest way to get an overview of STUARTs functions and capabilities is to use `?stuart` to open the package help-file.

## A Simple Example ##

The STUART package comes with a dataset from an intervention study concerning the *fairplayer* manual. The dataset contains 143 observations on 142 variables regarding empathy, social intelligence, and relational aggression. The data are longitudinal in nature (three occasions) and utilize two raters (self- and teacher-ratings).

        data(fairplayer)

The `mmas`-function can take quite a number of arguments, but only three are actually required: `data`, `factor.structure`, and `items.per.subtest`. The `data` were just loaded. To establish a factor structure the STUART functions require a named list of factors and their indicators. For the simple, three-factor structure at the first occasion this list can simply be provided as

        fs <- list(em=names(fairplayer)[5:12],
          ra=names(fairplayer)[53:57],
          si=names(fairplayer)[83:92])
        fs

the names of the list elements will be used as factor names in the models while the items provided are those that are chosen from throughout the search process. The final required setting is the number of items to be chosen per scale. In this case we could want a scale with 10 items in total: 3 for empathy, 3 for relational aggression, and 4 for social intelligence. The `items.per.subtest` argument accepts either a single value (if all scales are supposed to have the same number of items) or a list in the same order as the factor structure provided.

        nitems <- list(3,3,4)
        
With these three arguments we have a minimal working example for a run of the STUART functions. First, we can investigate the number of possible shortened scales.

        combinations(fairplayer,fs,nitems)

In this simple example there are just 117600 possible short forms. In cases such as this it might even be advisable to use the `bruteforce`-function to find the best possible short form. But, this being an example, we will use the central function of the STUART package, namely `mmas`, to find this solution quicker, albeit with less certainty about it being the absolute best solution.

Before running the next example, be aware that this may take a few minutes.

        sel <- mmas(fairplayer,fs,nitems)

The created object is of the `stuart` class for which some S3 methods are defined. For example a simple call to `print` returns the names of the selected items in the final version.

        sel
        
        $em
        $em$emA
        [1] "sEM01t1" "sEM06t1" "sEM07t1"
        
        
        $ra
        $ra$raA
        [1] "sRA02t1" "sRA04t1" "sRA05t1"
        
        
        $si
        $si$siA
        [1] "sSI02t1" "sSI04t1" "sSI07t1" "sSI08t1"

The `summary` function returns some additional information, such as the optimization history, the required estimation time and so on.

        summary(sel)
        
        Warning: This is an alpha-build, so there may be (a lot of) bugs.
        
        SUMMARY OF ANALYSIS:
        
        Analysis Type: mmas 
        Estimation Software: lavaan 
        Models estimated: 8976 
        Replications of final solution: 1 
        Time Required: 742.614 seconds
        
        Optimization History:
             run ant pheromone    chisq df    pvalue     rmsea       srmr      crel
        1      1   1  1.724044 33.00246 32 0.4179019 0.0157679 0.05020644 0.8873758
        2      1   2  1.733228 28.16404 32 0.6612230 0.0000000 0.05066193 0.9262812
        31     2  15  1.799919 21.83539 32 0.9117216 0.0000000 0.04512379 0.9028318
        52     4   4  1.825224 27.52765 32 0.6925106 0.0000000 0.04302606 0.9247173
        274   18   2  1.826272 22.64757 32 0.8891037 0.0000000 0.04286027 0.9106453
        353   23   1  1.867952 19.49596 32 0.9594677 0.0000000 0.03860388 0.8884581
        567   36   7  1.887248 17.95083 32 0.9784381 0.0000000 0.03657693 0.9137218
        2463 154  15  1.896617 18.49561 32 0.9727389 0.0000000 0.03532285 0.9046323
        4877 305  13  1.907333 14.95783 32 0.9955096 0.0000000 0.03385502 0.9054862
        
        Constructed Subtests:
        emA: sEM01t1 sEM06t1 sEM07t1
        raA: sRA02t1 sRA04t1 sRA05t1
        siA: sSI02t1 sSI04t1 sSI07t1 sSI08t1

Additionally, a call to `plot` will return a scatterplot of the optimization history. 