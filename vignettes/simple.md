Simple Tutorial
===============




Load `simr`
-----------


```r
library(devtools)
load_all(, reset = TRUE)
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


Building a simple model
-----------------------


```r
print(example)
```

```
##         y  x g       z
## 1   8.139  1 a -1.6814
## 2   7.948  2 a -1.6814
## 3   9.284  3 a -1.6814
## 4   7.779  4 a -1.6814
## 5   5.804  5 a -1.6814
## 6   6.132  6 a -1.6814
## 7   6.123  7 a -1.6814
## 8   7.543  8 a -1.6814
## 9   6.428  9 a -1.6814
## 10  6.219 10 a -1.6814
## 11  9.170  1 b -0.6905
## 12  8.254  2 b -0.6905
## 13 10.346  3 b -0.6905
## 14  8.807  4 b -0.6905
## 15  6.093  5 b -0.6905
## 16  8.511  6 b -0.6905
## 17  7.087  7 b -0.6905
## 18  6.242  8 b -0.6905
## 19  6.841  9 b -0.6905
## 20  5.783 10 b -0.6905
## 21 13.697  1 c  4.6761
## 22 13.551  2 c  4.6761
## 23 12.239  3 c  4.6761
## 24 14.514  4 c  4.6761
## 25 13.579  5 c  4.6761
## 26 12.038  6 c  4.6761
## 27 14.180  7 c  4.6761
## 28 13.103  8 c  4.6761
## 29 12.131  9 c  4.6761
## 30 13.071 10 c  4.6761
```

```r
attach(example)
```



```r
fit <- lmer(y ~ x + (1 | g), data = example)
gcol <- c(a = lcgreen, b = lcblue, c = lcbrown)
gbg <- c(a = lclightgreen, b = lclightblue, c = lclightbrown)
# gpch <- c(a=0, b=1, c=2)
plot(x, y, ylim = c(0, 20), lwd = 2, pch = 21, col = gcol[g], bg = gbg[g])
tmp <- mapply(abline, unique(z) + 10, -1/2, col = gcol)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.svg) 

```r
# ci_abline(fit)
```


Analysis
--------


```r
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


Subsets
-------


```r
par(mfrow = c(1, 3))

plot(x, y, ylim = c(0, 20), xlim = c(0, 10), lwd = 2)
fit1 <- lm(y ~ x, subset = 1:8)
# ci_abline(fit1)

plot(x, y, ylim = c(0, 20), xlim = c(0, 10), lwd = 2)
fit2 <- lm(y ~ x, subset = 1:6)
# ci_abline(fit2)

plot(x, y, ylim = c(0, 20), xlim = c(0, 10), lwd = 2)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.svg) 

```r
fit3 <- lm(y ~ x, subset = 1:4)
# ci_abline(fit3)
```






Power Analysis
--------------


```r
pa <- powercurve(fit)
```

```
## ===========================================================================
```

```r
plot(pa)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.svg) 

