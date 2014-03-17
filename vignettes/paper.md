simr: an R Package for Power Analysis of Linear Mixed Models by Simulation
========================================================

Simplified version of the paper, with the R excerpts.





```r
library(devtools)
load_all()
```






Example Data
------------

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.svg) 


Tutorial One
------------

Post-hoc power analyses.


```r
fit <- lmer(y ~ x + (1 | g), data = example)
summary(fit)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: y ~ x + (1 | g) 
##    Data: example 
## 
## REML criterion at convergence: 97.07 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  g        (Intercept) 11.136   3.337   
##  Residual              0.972   0.986   
## Number of obs: 30, groups: g, 3
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  10.6734     1.9655    5.43
## x            -0.2398     0.0627   -3.83
## 
## Correlation of Fixed Effects:
##   (Intr)
## x -0.175
```

```r

power(fit)
```

```
## [1] "98.20% (96.58, 99.06)"
```


Tutorial Two
------------

Calculate a power curve.


```r
pc2 <- powerCurve(fit)
```

```
## Calculating power at 8 sample sizes for x
```

```r
plot(pc2)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.svg) 


Tutorial Three
--------------

Specify the sample and effect sizes.


```r
fixef(fit)
```

```
## (Intercept)           x 
##     10.6734     -0.2398
```

```r
fixef(fit)["x"]
```

```
##       x 
## -0.2398
```



```r
fixef(fit)["x"] <- -0.1
```



```r
pc3a <- powerCurve(fit)
```

```
## Calculating power at 8 sample sizes for x
```

```r
plot(pc3a)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.svg) 



```r
fit <- extend(fit, along = "x", n = 20)
pc3b <- powerCurve(fit)
```

```
## Calculating power at 18 sample sizes for x
```

```r
plot(pc3b)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.svg) 


Tutorial Four
-------------

Include Type I error calculations.


```r
pc4 <- pc3b
plot(pc4, pval = c(0.01, 0.05, 0.1))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.svg) 

