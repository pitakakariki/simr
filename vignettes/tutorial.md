Introduction to Simm
====================

If we are going to invest in the collection of monitoring data, it is important that we know that our design is suitable for making the inferences we need. Does the design have sufficient power to detect what we are trying to find? Are our parameter estimates going to be precise enough for our purposes? 

When the design is complicated, e.g. in a mixed-effects model, simulation provides a general way of answering these questions. The `simm` package provides tools that make it simple to set up and run simulation experiments. 

Installation
------------

### Using `devtools` to install the new `lme4`

`simm` is based on the new (as yet unreleased) version of `lme4`.


```r
library(devtools)
# dev_mode(on=TRUE) library(lme4)
```


### Installing `simm`


```r
install.packages("simm.tar.gz", repos = NULL, type = "source")
```


### Other packages for this tutorial


```r
library(xtable)
options(width = 100)
```


Load the package
----------------

Once you have installed `simm`, it can be loaded from your library in the usual way.


```r
library(simm)
```



```r
load_all(, reset = TRUE)
```



```r
load_all("..", reset = TRUE)
```

```
## Loading simr
## Loading required namespace: lme4
## Loading required namespace: plyr
## Loading required namespace: binom
## Loading required namespace: plotrix
## Loading required package: lme4
## Loading required package: lattice
## Loading required package: Matrix
## Loading required package: plyr
## Loading required package: binom
## Loading required package: plotrix
## Loading required package: stringr
```


The kiwifruit dataset
---------------------

This data was collected as part of the ARGOS soil monitoring pilot.

The pilot study collected measurements in each of three years, 2004, 2006, and 2009[^1].


```r
head(kiwifruit[, 1:6], 10)
```

```
##    Year Cluster Property Mgmt Block    Position
## 1  2004       1        1    A     1  WithinRows
## 2  2004       1        1    A     1 BetweenRows
## 3  2004       1        1    A     2  WithinRows
## 4  2004       1        1    A     2 BetweenRows
## 5  2004       1        1    A     3  WithinRows
## 6  2004       1        1    A     3 BetweenRows
## 7  2004       2        4    A     1  WithinRows
## 8  2004       2        4    A     1 BetweenRows
## 9  2004       2        4    A     2  WithinRows
## 10 2004       2        4    A     2 BetweenRows
```

```r
head(kiwifruit[, 7:12], 10)
```

```
##     pH OlsenP AMN Carbon Nitrogen BulkDensity
## 1  6.0     52  92   6.22     0.41        0.91
## 2  6.3     40  93   7.09     0.50        0.92
## 3  6.1     48 110   6.94     0.50        0.94
## 4  6.2     34 127   6.94     0.50        0.91
## 5  5.9     71 103   5.94     0.44        0.92
## 6  6.3     55 144   6.42     0.49        0.87
## 7  6.2     60  55   7.30     0.63        0.69
## 8  6.1     38  66   9.41     0.81        0.66
## 9  6.1     62  46   7.25     0.60        0.72
## 10 6.4     53  65   8.39     0.72        0.71
```


<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Fri Dec 13 10:53:33 2013 -->
<TABLE >
<TR> <TH> Year </TH> <TH> Cluster </TH> <TH> Property </TH> <TH> Mgmt </TH> <TH> Block </TH> <TH> Position </TH> <TH> pH </TH> <TH> OlsenP </TH> <TH> AMN </TH> <TH> Carbon </TH> <TH> Nitrogen </TH> <TH> BulkDensity </TH>  </TR>
  <TR> <TD align="right"> 2004 </TD> <TD> 1 </TD> <TD> 1 </TD> <TD> A </TD> <TD> 1 </TD> <TD> WithinRows </TD> <TD align="right"> 6.00 </TD> <TD align="right"> 52.00 </TD> <TD align="right"> 92.00 </TD> <TD align="right"> 6.22 </TD> <TD align="right"> 0.41 </TD> <TD align="right"> 0.91 </TD> </TR>
  <TR> <TD align="right"> 2004 </TD> <TD> 1 </TD> <TD> 1 </TD> <TD> A </TD> <TD> 1 </TD> <TD> BetweenRows </TD> <TD align="right"> 6.30 </TD> <TD align="right"> 40.00 </TD> <TD align="right"> 93.00 </TD> <TD align="right"> 7.09 </TD> <TD align="right"> 0.50 </TD> <TD align="right"> 0.92 </TD> </TR>
  <TR> <TD align="right"> 2004 </TD> <TD> 1 </TD> <TD> 1 </TD> <TD> A </TD> <TD> 2 </TD> <TD> WithinRows </TD> <TD align="right"> 6.10 </TD> <TD align="right"> 48.00 </TD> <TD align="right"> 110.00 </TD> <TD align="right"> 6.94 </TD> <TD align="right"> 0.50 </TD> <TD align="right"> 0.94 </TD> </TR>
  <TR> <TD align="right"> 2004 </TD> <TD> 1 </TD> <TD> 1 </TD> <TD> A </TD> <TD> 2 </TD> <TD> BetweenRows </TD> <TD align="right"> 6.20 </TD> <TD align="right"> 34.00 </TD> <TD align="right"> 127.00 </TD> <TD align="right"> 6.94 </TD> <TD align="right"> 0.50 </TD> <TD align="right"> 0.91 </TD> </TR>
  <TR> <TD align="right"> 2004 </TD> <TD> 1 </TD> <TD> 1 </TD> <TD> A </TD> <TD> 3 </TD> <TD> WithinRows </TD> <TD align="right"> 5.90 </TD> <TD align="right"> 71.00 </TD> <TD align="right"> 103.00 </TD> <TD align="right"> 5.94 </TD> <TD align="right"> 0.44 </TD> <TD align="right"> 0.92 </TD> </TR>
  <TR> <TD align="right"> 2004 </TD> <TD> 1 </TD> <TD> 1 </TD> <TD> A </TD> <TD> 3 </TD> <TD> BetweenRows </TD> <TD align="right"> 6.30 </TD> <TD align="right"> 55.00 </TD> <TD align="right"> 144.00 </TD> <TD align="right"> 6.42 </TD> <TD align="right"> 0.49 </TD> <TD align="right"> 0.87 </TD> </TR>
  <TR> <TD align="right"> 2004 </TD> <TD> 2 </TD> <TD> 4 </TD> <TD> A </TD> <TD> 1 </TD> <TD> WithinRows </TD> <TD align="right"> 6.20 </TD> <TD align="right"> 60.00 </TD> <TD align="right"> 55.00 </TD> <TD align="right"> 7.30 </TD> <TD align="right"> 0.63 </TD> <TD align="right"> 0.69 </TD> </TR>
  <TR> <TD align="right"> 2004 </TD> <TD> 2 </TD> <TD> 4 </TD> <TD> A </TD> <TD> 1 </TD> <TD> BetweenRows </TD> <TD align="right"> 6.10 </TD> <TD align="right"> 38.00 </TD> <TD align="right"> 66.00 </TD> <TD align="right"> 9.41 </TD> <TD align="right"> 0.81 </TD> <TD align="right"> 0.66 </TD> </TR>
  <TR> <TD align="right"> 2004 </TD> <TD> 2 </TD> <TD> 4 </TD> <TD> A </TD> <TD> 2 </TD> <TD> WithinRows </TD> <TD align="right"> 6.10 </TD> <TD align="right"> 62.00 </TD> <TD align="right"> 46.00 </TD> <TD align="right"> 7.25 </TD> <TD align="right"> 0.60 </TD> <TD align="right"> 0.72 </TD> </TR>
  <TR> <TD align="right"> 2004 </TD> <TD> 2 </TD> <TD> 4 </TD> <TD> A </TD> <TD> 2 </TD> <TD> BetweenRows </TD> <TD align="right"> 6.40 </TD> <TD align="right"> 53.00 </TD> <TD align="right"> 65.00 </TD> <TD align="right"> 8.39 </TD> <TD align="right"> 0.72 </TD> <TD align="right"> 0.71 </TD> </TR>
   </TABLE>


[^1]: Data labelled 2009 was actually collected over two years 2009/10. We intend to use the actual year of collection in the final version.

Fit a linear mixed model
------------------------


```r
# kiwifruit <- within(kiwifruit, Year <- Year - 2006)
fit <- lmer(Carbon ~ Position * Mgmt + Year + (Year | Cluster/Mgmt/Block), kiwifruit)
print(fit)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: Carbon ~ Position * Mgmt + Year + (Year | Cluster/Mgmt/Block) 
##    Data: kiwifruit 
## REML criterion at convergence: 1544 
## Random effects:
##  Groups               Name        Std.Dev. Corr 
##  Block:(Mgmt:Cluster) (Intercept) 137.6314      
##                       Year          0.0687 -1.00
##  Mgmt:Cluster         (Intercept)  98.5046      
##                       Year          0.0490 -1.00
##  Cluster              (Intercept) 138.1417      
##                       Year          0.0689 -1.00
##  Residual                           0.6961      
## Number of obs: 641, groups: Block:(Mgmt:Cluster), 108; Mgmt:Cluster, 36; Cluster, 12
## Fixed Effects:
##              (Intercept)        PositionWithinRows                     MgmtB  
##                 -231.536                    -0.548                     0.627  
##                    MgmtC                      Year  PositionWithinRows:MgmtB  
##                    0.622                     0.118                     0.149  
## PositionWithinRows:MgmtC  
##                   -0.220
```


Extend longitudinal data
------------------------

For each of the three years there should be 216 observations --- 12 clusters x 3 management types x 3 blocks x 2 positions.


```r
nrow(subset(kiwifruit, Year == 2004))
```

```
## [1] 216
```

```r
nrow(subset(kiwifruit, Year == 2006))
```

```
## [1] 216
```

```r
nrow(subset(kiwifruit, Year == 2009))
```

```
## [1] 216
```



```r
seq(2014, 2039, length = 6)
```

```
## [1] 2014 2019 2024 2029 2034 2039
```



```r
yseq <- seq(2014, 2039, length = 6)
xfit <- extend(fit, along = "Year", values = yseq)
# xfit <- extend(fit, along='Year', values=seq(-3, 3))
dim(xfit@frame)
```

```
## [1] 1296    6
```


Setting the red-alert trend
---------------------------

We would like our monitoring effort to have enough power to detect an industry-wide trend in soil carbon of -0.13 (units). However, if we simulate from the fitted model the "true" trend would be 0.1182. `simm` lets us modify the fixed effect coefficients in our fitted models.


```r
# fixef(xfit)['Year'] <- -0.13
fixef(xfit)["Year"] <- -0.013
fixef(xfit)
```

```
##              (Intercept)       PositionWithinRows                    MgmtB                    MgmtC 
##                -231.5360                  -0.5479                   0.6266                   0.6217 
##                     Year PositionWithinRows:MgmtB PositionWithinRows:MgmtC 
##                  -0.0130                   0.1489                  -0.2197
```


TODO: set variance components.


Creating simulated data
-------------------------


```r
Nsim <- 100
sims <- simulate(xfit, Nsim)
```


Running the simulation
----------------------


```r
results <- multiFit(resp = sims, model = xfit, fn = test("Year", "lower"))
```

```
##   |                                                                                                  |                                                                                          |   0%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=                                                                                         |   1%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==                                                                                        |   2%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |===                                                                                       |   3%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |====                                                                                      |   4%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |====                                                                                      |   5%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=====                                                                                     |   6%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |======                                                                                    |   7%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=======                                                                                   |   8%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |========                                                                                  |   9%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=========                                                                                 |  10%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==========                                                                                |  11%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |===========                                                                               |  12%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |============                                                                              |  13%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=============                                                                             |  14%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==============                                                                            |  15%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==============                                                                            |  16%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |===============                                                                           |  17%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |================                                                                          |  18%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=================                                                                         |  19%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==================                                                                        |  20%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |===================                                                                       |  21%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |====================                                                                      |  22%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=====================                                                                     |  23%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |======================                                                                    |  24%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |======================                                                                    |  25%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=======================                                                                   |  26%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |========================                                                                  |  27%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=========================                                                                 |  28%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==========================                                                                |  29%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |===========================                                                               |  30%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |============================                                                              |  31%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=============================                                                             |  32%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==============================                                                            |  33%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |===============================                                                           |  34%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |================================                                                          |  35%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |================================                                                          |  36%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=================================                                                         |  37%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==================================                                                        |  38%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |===================================                                                       |  39%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |====================================                                                      |  40%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=====================================                                                     |  41%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |======================================                                                    |  42%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=======================================                                                   |  43%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |========================================                                                  |  44%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |========================================                                                  |  45%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=========================================                                                 |  46%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==========================================                                                |  47%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |===========================================                                               |  48%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |============================================                                              |  49%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=============================================                                             |  50%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==============================================                                            |  51%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |===============================================                                           |  52%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |================================================                                          |  53%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=================================================                                         |  54%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==================================================                                        |  55%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==================================================                                        |  56%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |===================================================                                       |  57%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |====================================================                                      |  58%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=====================================================                                     |  59%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |======================================================                                    |  60%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=======================================================                                   |  61%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |========================================================                                  |  62%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=========================================================                                 |  63%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==========================================================                                |  64%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==========================================================                                |  65%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |===========================================================                               |  66%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |============================================================                              |  67%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=============================================================                             |  68%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==============================================================                            |  69%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |===============================================================                           |  70%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |================================================================                          |  71%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=================================================================                         |  72%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==================================================================                        |  73%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |===================================================================                       |  74%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |====================================================================                      |  75%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |====================================================================                      |  76%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=====================================================================                     |  77%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |======================================================================                    |  78%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=======================================================================                   |  79%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |========================================================================                  |  80%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=========================================================================                 |  81%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==========================================================================                |  82%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |===========================================================================               |  83%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |============================================================================              |  84%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |============================================================================              |  85%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=============================================================================             |  86%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==============================================================================            |  87%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |===============================================================================           |  88%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |================================================================================          |  89%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=================================================================================         |  90%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==================================================================================        |  91%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |===================================================================================       |  92%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |====================================================================================      |  93%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=====================================================================================     |  94%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |======================================================================================    |  95%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |======================================================================================    |  96%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |=======================================================================================   |  97%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |========================================================================================  |  98%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |========================================================================================= |  99%
```

```
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
## Warning: additional arguments to refit.merMod ignored
```

```
##   |                                                                                                  |==========================================================================================| 100%
```



Viewing the results
-------------------


```r
pwr <- colMeans(results < 0.05)
powerPlot(colSums(results < 0.05), Nsim)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 

```r
pwr
```

```
## Year Year Year Year Year 
##    1    1    1    1    1
```



First Header | Second Header
------------ | -------------
Content      | Content
Content      | Content
