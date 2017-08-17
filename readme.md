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

        devtools::install_bitbucket('martscht/stuart/stuart')
        library(stuart)

This will install the version currently committed to the master branch. If you want to be a bit more edgy, you can install the development build by setting `ref='develop'`, like so:

        devtools::install_bitbucket('martscht/stuart/stuart', ref = 'develop')
        library(stuart)

After installation the easiest way to get an overview of STUARTs functions and capabilities is to use `?stuart` to open the package help-file.

## A Simple Example ##

The STUART package comes with a dataset from an intervention study concerning the *fairplayer* manual. The dataset contains 143 observations on 142 variables regarding empathy, social intelligence, and relational aggression. The data are longitudinal in nature (three occasions) and utilize two raters (self- and teacher-ratings).

        data(fairplayer)

The `mmas`-function can take quite a number of arguments, but only three are actually required: `data`, `factor.structure`, and `capacity`. The `data` were just loaded. To establish a factor structure the STUART functions require a named list of factors and their indicators. For the simple, three-factor structure at the first occasion this list can simply be provided as

        fs <- list(em=names(fairplayer)[5:12],
          ra=names(fairplayer)[53:57],
          si=names(fairplayer)[83:92])
        fs

the names of the list elements will be used as factor names in the models while the items provided are those that are chosen from throughout the search process. The final required setting is the number of items to be chosen per scale. In this case we could want a scale with 10 items in total: 3 for empathy, 3 for relational aggression, and 4 for social intelligence. The `capacity` argument accepts either a single value (if all scales are supposed to have the same number of items) or a list in the same order as the factor structure provided.

        nitems <- list(3, 3, 4)
        
With these three arguments we have a minimal working example for a run of the STUART functions. First, we can investigate the number of possible shortened scales.

        combinations(fairplayer, fs, nitems)

In this simple example there are just 117600 possible short forms. In cases such as this it might even be advisable to use the `bruteforce`-function to find the best possible short form. But, this being an example, we will use the central function of the STUART package, namely `mmas`, to find this solution quicker, albeit with less certainty about it being the absolute best solution.

Before running the next example, be aware that this may take a few minutes.

        sel <- mmas(fairplayer, fs, nitems)

The created object is of the `stuart` class for which some S3 methods are defined. For example a simple call to `print` returns the names of the selected items in the final version.

        sel
        
        $em
        [1] "sEM01t1" "sEM06t1" "sEM07t1"
        
        $ra
        [1] "sRA02t1" "sRA04t1" "sRA05t1"
        
        $si
        [1] "sSI02t1" "sSI04t1" "sSI07t1" "sSI08t1"

The `summary` function returns some additional information, such as the optimization history, the required estimation time and so on.

        summary(sel)
        
        Warning: This is an alpha-build, so there may be (a lot of) bugs.
        
        SUMMARY OF ANALYSIS:
        
        Analysis Type: mmas 
        Estimation Software: lavaan 
        Models estimated: 8064 
        Replications of final solution: 1 
        Time Required: 620.709 seconds
        
        Optimization History:
             run ant pheromone    chisq df    pvalue rmsea       srmr      crel
        1      1   1  1.789815 18.27469 32 0.9751713     0 0.04371548 0.7380582
        84     6   4  1.824709 28.69622 32 0.6345454     0 0.04191957 0.8011105
        143    9  15  1.838218 20.04979 32 0.9503899     0 0.04016347 0.7778222
        502   32   6  1.856393 19.16133 32 0.9643375     0 0.03721653 0.7430853
        519   33   7  1.866253 24.44264 32 0.8279564     0 0.03790626 0.8151667
        1631 102  15  1.867360 15.54633 32 0.9936224     0 0.03671887 0.7681884
        1653 104   5  1.868370 18.61118 32 0.9713980     0 0.03698459 0.7826044
        1989 125   5  1.872827 18.83168 32 0.9687049     0 0.03693269 0.8039070
        3416 214   8  1.880173 18.49561 32 0.9727389     0 0.03532288 0.7756336
        3958 248   6  1.892377 14.95783 32 0.9955096     0 0.03385501 0.7827800
        
        Constructed Subtests:
        em: sEM01t1 sEM06t1 sEM07t1
        ra: sRA02t1 sRA04t1 sRA05t1
        si: sSI02t1 sSI04t1 sSI07t1 sSI08t1

Additionally, a call to `plot` will return a scatterplot of the optimization history. 

## Longitudinal Selection ##

The simple example can be extended to a longitudinal setting. The `fairplayer` data stem from three measurement occasions. This means, that there are sets of three facets which are repeated measures of each other. In the extension of the simple example, this results in nine total facets (empathy, relational aggression, social intelligence, each measured thrice). All nine need to be coded in a list, to associate the observed variables with their respective facets at their respective occasion.

        fs <- list(em1 = names(fairplayer)[5:12],
                   em2 = names(fairplayer)[13:20],
                   em3 = names(fairplayer)[21:28],
                   ra1 = names(fairplayer)[53:57],
                   ra2 = names(fairplayer)[58:62],
                   ra3 = names(fairplayer)[63:67],
                   si1 = names(fairplayer)[83:92],
                   si2 = names(fairplayer)[93:102],
                   si3 = names(fairplayer)[103:112])

Using only this factor structure will result in specific items being selected at each occasion, because the facets are simply treated as different from each other. To declare facets as repeated measures of each other, another list is necessary:

        repe <- list(em = c('em1', 'em2', 'em3'),
                     ra = c('ra1', 'ra2', 'ra3'),
                     si = c('si1', 'si2', 'si3'))
                     
This list contains the names of the facets as provided by the `fs` object and allocates each of them to a "construct" measured repeatedly by these facets. Note, that these need not necessarily all be of the same length, meaning that some constructs can be measured twice, some three times, some four times, and so on.

Given repeated measures, measurement invariance is of central importance. In `stuart` longitudinal invariance is provided via the argument `long.invariance`. The possible settings here are the classic levels `configural`, `weak`, `strong`, and `strict`, with the last being the default. This argument accepts vectors of the same length as the repeated measures (i.e. construct-specific measurement invariance) or a single value. To make this example as descriptive as possible we can assume three different invariance levels:

        invar <- c('weak', 'strong', 'strict')
        
Again, assume we want to select three items for empathy, three items for relational aggression, and four for social intelligence, we now need to provide a list of 9 items (because there are 9 facets):

        nitems <- list(3, 3, 3, 3, 3, 3, 4, 4, 4)
        
Running this example will require some time, so be aware that it might not actually be necessary to run this following code. Note that, because the sample is small and the model quite large, the CFA will result in a large number of non-positiv-definite latent covariance matrices. To prevent this from aborting the search process, we can set `ignore.errors = TRUE`. In any actual application, however, this should be cause for a considerable degree of alarm.

        sel <- mmas(fairplayer, fs, nitems,
                    repeated.measures = repe, long.invariance = invar,
                    ignore.errors = TRUE)
                    
Again, using the `summary` function on the resulting object returns some information regarding the optimization process:

        summary(sel)
        
        Warning: This is an alpha-build, so there may be (a lot of) bugs.
        
        SUMMARY OF ANALYSIS:
        
        Analysis Type: mmas 
        Estimation Software: lavaan 
        Models estimated: 5008 
        Replications of final solution: 78 
        Time Required: 4104.629 seconds
        
        Optimization History:
            run ant pheromone    chisq  df       pvalue      rmsea       srmr      crel
        1     1   1 0.9960436 774.9897 401 0.000000e+00 0.08104261 0.09178599 0.7378735
        2     1   2 1.0460076 657.2298 401 1.043610e-14 0.06708083 0.08737351 0.7119002
        5     1   5 1.0475052 685.0290 401 0.000000e+00 0.07062606 0.08608036 0.7790661
        6     1   6 1.1686702 593.7273 401 1.197814e-09 0.05817753 0.07659920 0.8049035
        25    2   9 1.1711371 596.4139 401 7.627811e-10 0.05858162 0.07530049 0.8211066
        88    6   8 1.3081149 527.2350 401 2.218685e-05 0.04708403 0.07405632 0.7920223
        374  24   6 1.3155178 524.2200 401 3.242987e-05 0.04651834 0.07482392 0.8111476
        421  27   5 1.3200556 532.1031 401 1.186693e-05 0.04798329 0.07022107 0.8290066
        542  34  14 1.3309961 524.3895 401 3.175037e-05 0.04655033 0.07129974 0.8197467
        741  47   5 1.3558131 514.1646 401 1.099512e-04 0.04457988 0.07090216 0.8160987
        905  57   9 1.3641452 505.8666 401 2.855232e-04 0.04291432 0.07205498 0.7865321
        
        Constructed Subtests:
        em1: sEM05t1 sEM07t1 sEM08t1
        em2: sEM05t2 sEM07t2 sEM08t2
        em3: sEM05t3 sEM07t3 sEM08t3
        ra1: sRA01t1 sRA02t1 sRA05t1
        ra2: sRA01t2 sRA02t2 sRA05t2
        ra3: sRA01t3 sRA02t3 sRA05t3
        si1: sSI03t1 sSI04t1 sSI06t1 sSI10t1
        si2: sSI03t2 sSI04t2 sSI06t2 sSI10t2
        si3: sSI03t3 sSI04t3 sSI06t3 sSI10t3
        
When using lavaan for the model estimation (as is the preset), the output slot `final` returns the `lavaan`-class output of the final model, meaning that all lavaan-related functions can be applied to the final model:

        lavaan (0.5-23.1097) converged normally after 125 iterations
        
                                                          Used       Total
          Number of observations                           142         143
        
          Number of missing patterns                        18
        
          Estimator                                         ML
          Minimum Function Test Statistic              505.867
          Degrees of freedom                               401
          P-value (Chi-square)                           0.000
        
        Parameter Estimates:
        
          Information                                 Observed
          Standard Errors                             Standard
        
        Latent Variables:
                           Estimate  Std.Err  z-value  P(>|z|)
          em1 =~                                              
            sEM05t1 (l511)    1.000                           
            sEM07t1 (l711)    1.052    0.080   13.223    0.000
            sEM08t1 (l811)    1.065    0.088   12.169    0.000
          em2 =~                                              
            sEM05t2 (l511)    1.000                           
            sEM07t2 (l711)    1.052    0.080   13.223    0.000
            sEM08t2 (l811)    1.065    0.088   12.169    0.000
          em3 =~                                              
            sEM05t3 (l511)    1.000                           
            sEM07t3 (l711)    1.052    0.080   13.223    0.000
            sEM08t3 (l811)    1.065    0.088   12.169    0.000
          ra1 =~                                              
            sRA01t1 (l121)    1.000                           
            sRA02t1 (l221)    1.701    0.179    9.480    0.000
            sRA05t1 (l521)    1.124    0.121    9.312    0.000
          ra2 =~                                              
            sRA01t2 (l121)    1.000                           
            sRA02t2 (l221)    1.701    0.179    9.480    0.000
            sRA05t2 (l521)    1.124    0.121    9.312    0.000
          ra3 =~                                              
            sRA01t3 (l121)    1.000                           
            sRA02t3 (l221)    1.701    0.179    9.480    0.000
            sRA05t3 (l521)    1.124    0.121    9.312    0.000
          si1 =~                                              
            sSI03t1 (l331)    1.000                           
            sSI04t1 (l431)    1.057    0.143    7.374    0.000
            sSI06t1 (l631)    1.325    0.165    8.016    0.000
            sSI10t1 (l103)    1.737    0.202    8.580    0.000
          si2 =~                                              
            sSI03t2 (l331)    1.000                           
            sSI04t2 (l431)    1.057    0.143    7.374    0.000
            sSI06t2 (l631)    1.325    0.165    8.016    0.000
            sSI10t2 (l103)    1.737    0.202    8.580    0.000
          si3 =~                                              
            sSI03t3 (l331)    1.000                           
            sSI04t3 (l431)    1.057    0.143    7.374    0.000
            sSI06t3 (l631)    1.325    0.165    8.016    0.000
            sSI10t3 (l103)    1.737    0.202    8.580    0.000
        
        Covariances:
                           Estimate  Std.Err  z-value  P(>|z|)
          em1 ~~                                              
            em2               0.313    0.063    4.993    0.000
            em3               0.202    0.057    3.526    0.000
            ra1              -0.022    0.033   -0.670    0.503
            ra2              -0.049    0.036   -1.366    0.172
            ra3              -0.001    0.036   -0.029    0.977
            si1               0.150    0.037    4.036    0.000
            si2               0.125    0.040    3.111    0.002
            si3               0.089    0.040    2.231    0.026
          em2 ~~                                              
            em3               0.372    0.069    5.370    0.000
            ra1              -0.010    0.033   -0.303    0.762
            ra2              -0.023    0.035   -0.676    0.499
            ra3              -0.007    0.035   -0.192    0.848
            si1               0.144    0.037    3.863    0.000
            si2               0.215    0.048    4.532    0.000
            si3               0.195    0.047    4.172    0.000
          em3 ~~                                              
            ra1              -0.005    0.033   -0.159    0.874
            ra2              -0.006    0.038   -0.156    0.876
            ra3              -0.012    0.035   -0.357    0.721
            si1               0.162    0.039    4.166    0.000
            si2               0.195    0.046    4.277    0.000
            si3               0.263    0.052    5.038    0.000
          ra1 ~~                                              
            ra2               0.072    0.025    2.870    0.004
            ra3               0.106    0.029    3.613    0.000
            si1               0.036    0.021    1.705    0.088
            si2               0.040    0.025    1.620    0.105
            si3               0.025    0.025    0.979    0.328
          ra2 ~~                                              
            ra3               0.070    0.030    2.363    0.018
            si1              -0.002    0.022   -0.100    0.921
            si2               0.010    0.025    0.398    0.691
            si3               0.013    0.028    0.472    0.637
          ra3 ~~                                              
            si1               0.018    0.023    0.806    0.420
            si2               0.024    0.026    0.916    0.360
            si3               0.020    0.026    0.784    0.433
          si1 ~~                                              
            si2               0.162    0.040    4.014    0.000
            si3               0.153    0.040    3.859    0.000
          si2 ~~                                              
            si3               0.199    0.050    4.004    0.000
        
        Intercepts:
                           Estimate  Std.Err  z-value  P(>|z|)
           .sEM051 (a5111)    3.818    0.077   49.278    0.000
           .sEM071 (a7111)    3.831    0.085   45.211    0.000
           .sEM081 (a8111)    3.770    0.090   42.107    0.000
           .sEM052 (a5112)    3.821    0.078   49.051    0.000
           .sEM072 (a7112)    3.933    0.078   50.185    0.000
           .sEM082 (a8112)    3.838    0.085   45.005    0.000
           .sEM053 (a5113)    3.824    0.081   47.365    0.000
           .sEM073 (a7113)    3.868    0.080   48.312    0.000
           .sEM083 (a8113)    3.836    0.088   43.759    0.000
           .sRA011  (a121)    1.316    0.051   25.981    0.000
           .sRA021  (a221)    1.684    0.077   21.888    0.000
           .sRA051  (a521)    1.367    0.057   24.069    0.000
            ra1               0.000                           
           .sRA012  (a121)    1.316    0.051   25.981    0.000
           .sRA022  (a221)    1.684    0.077   21.888    0.000
           .sRA052  (a521)    1.367    0.057   24.069    0.000
            ra2              -0.024    0.055   -0.430    0.667
           .sRA013  (a121)    1.316    0.051   25.981    0.000
           .sRA023  (a221)    1.684    0.077   21.888    0.000
           .sRA053  (a521)    1.367    0.057   24.069    0.000
            ra3              -0.005    0.050   -0.097    0.923
           .sSI031  (a331)    3.780    0.056   68.068    0.000
           .sSI041  (a431)    3.399    0.056   60.509    0.000
           .sSI061  (a631)    3.329    0.063   52.593    0.000
           .sSI101  (a103)    3.133    0.078   40.115    0.000
            si1               0.000                           
           .sSI032  (a331)    3.780    0.056   68.068    0.000
           .sSI042  (a431)    3.399    0.056   60.509    0.000
           .sSI062  (a631)    3.329    0.063   52.593    0.000
           .sSI102  (a103)    3.133    0.078   40.115    0.000
            si2               0.044    0.040    1.101    0.271
           .sSI033  (a331)    3.780    0.056   68.068    0.000
           .sSI043  (a431)    3.399    0.056   60.509    0.000
           .sSI063  (a631)    3.329    0.063   52.593    0.000
           .sSI103  (a103)    3.133    0.078   40.115    0.000
            si3               0.156    0.046    3.376    0.001
            em1               0.000                           
            em2               0.000                           
            em3               0.000                           
        
        Variances:
                           Estimate  Std.Err  z-value  P(>|z|)
           .sEM051 (e5111)    0.337    0.059    5.708    0.000
           .sEM071 (e7111)    0.445    0.075    5.955    0.000
           .sEM081 (e8111)    0.543    0.084    6.447    0.000
           .sEM052 (e5112)    0.334    0.053    6.360    0.000
           .sEM072 (e7112)    0.298    0.051    5.898    0.000
           .sEM082 (e8112)    0.429    0.066    6.511    0.000
           .sEM053 (e5113)    0.398    0.062    6.408    0.000
           .sEM073 (e7113)    0.345    0.061    5.618    0.000
           .sEM083 (e8113)    0.493    0.075    6.550    0.000
           .sRA011 (e1211)    0.211    0.033    6.311    0.000
           .sRA021 (e2211)    0.220    0.071    3.082    0.002
           .sRA051 (e5211)    0.442    0.065    6.831    0.000
           .sRA012 (e1212)    0.200    0.037    5.416    0.000
           .sRA022 (e2212)    0.619    0.106    5.865    0.000
           .sRA052 (e5212)    0.183    0.039    4.721    0.000
           .sRA013 (e1213)    0.989    0.132    7.478    0.000
           .sRA023 (e2213)    0.256    0.078    3.278    0.001
           .sRA053 (e5213)    0.244    0.046    5.292    0.000
           .sSI031  (e331)    0.574    0.046   12.370    0.000
           .sSI041  (e431)    0.541    0.045   12.086    0.000
           .sSI061  (e631)    0.511    0.045   11.367    0.000
           .sSI101  (e103)    0.581    0.057   10.240    0.000
           .sSI032  (e331)    0.574    0.046   12.370    0.000
           .sSI042  (e431)    0.541    0.045   12.086    0.000
           .sSI062  (e631)    0.511    0.045   11.367    0.000
           .sSI102  (e103)    0.581    0.057   10.240    0.000
           .sSI033  (e331)    0.574    0.046   12.370    0.000
           .sSI043  (e431)    0.541    0.045   12.086    0.000
           .sSI063  (e631)    0.511    0.045   11.367    0.000
           .sSI103  (e103)    0.581    0.057   10.240    0.000
            em1               0.417    0.079    5.269    0.000
            em2               0.451    0.082    5.503    0.000
            em3               0.440    0.083    5.333    0.000
            ra1               0.196    0.040    4.860    0.000
            ra2               0.200    0.045    4.427    0.000
            ra3               0.206    0.049    4.175    0.000
            si1               0.139    0.038    3.651    0.000
            si2               0.218    0.055    3.984    0.000
            si3               0.232    0.059    3.946    0.000