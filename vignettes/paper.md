simr: an R Package for Power Analysis of Linear Mixed Models by Simulation
========================================================

Simplified version of the paper, with the R excerpts.





```r
library(devtools)
load_all(export_all = FALSE)
```






Example Data
------------

![plot of chunk example](figure/example.svg) 


Tutorial One
------------

Post-hoc power analyses.


```r
model1 <- lmer(y ~ x + (1 | g), data = example)
summary(model1)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: y ~ x + (1 | g)
##    Data: example
## 
## REML criterion at convergence: 97.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.7799 -0.6563 -0.0494  0.6809  2.0483 
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

powerSim(model1)
```

```
## Power to detect effect of x, (95% confidence interval):
##  96.60% ( 95.28,  97.63)
## 
## Based on 1000 simulations and effect size -0.24
```


Tutorial Two
------------

Calculate a power curve.


```r
pc1 <- powerCurve(model1)
```

```
## Calculating power at 8 sample sizes for x
```

```r
print(pc1)
```

```
## Power to detect effect of x, (95% confidence interval):
## #levels for x 
##       3:  11.10% (  9.22,  13.21)
##       4:  16.80% ( 14.53,  19.26)
##       5:  27.40% ( 24.66,  30.28)
##       6:  44.70% ( 41.59,  47.84)
##       7:  61.80% ( 58.71,  64.82)
##       8:  76.20% ( 73.44,  78.81)
##       9:  87.80% ( 85.61,  89.76)
##      10:  95.20% ( 93.69,  96.44)
## 
## Time elapsed: 0 h 15 m 13 s
```

```r
plot(pc1)
```

![plot of chunk powercurve1](figure/powercurve1.svg) 


Tutorial Three
--------------

Specify the sample and effect sizes.


```r
fixef(model1)
```

```
## (Intercept)           x 
##     10.6734     -0.2398
```

```r
fixef(model1)["x"]
```

```
##       x 
## -0.2398
```



```r
model2 <- model1
fixef(model2)["x"] <- -0.1
```



```r
pc2 <- powerCurve(model2)
```

```
## Calculating power at 8 sample sizes for x
```

```r
print(pc2)
```

```
## Power to detect effect of x, (95% confidence interval):
## #levels for x 
##       3:   6.90% (  5.41,   8.65)
##       4:   8.40% (  6.76,  10.29)
##       5:  10.50% (  8.67,  12.57)
##       6:  12.60% ( 10.61,  14.82)
##       7:  17.80% ( 15.48,  20.31)
##       8:  20.90% ( 18.42,  23.55)
##       9:  25.90% ( 23.21,  28.73)
##      10:  34.70% ( 31.75,  37.74)
## 
## Time elapsed: 0 h 15 m 24 s
```

```r
plot(pc2)
```

![plot of chunk powercurve2](figure/powercurve2.svg) 



```r
model3 <- extend(model2, along = "x", n = 20)
pc3 <- powerCurve(model3)
```

```
## Calculating power at 18 sample sizes for x
```

```r
print(pc3)
```

```
## Power to detect effect of x, (95% confidence interval):
## #levels for x 
##       3:   8.80% (  7.12,  10.73)
##       4:   9.40% (  7.66,  11.38)
##       5:  10.30% (  8.49,  12.35)
##       6:  12.90% ( 10.88,  15.14)
##       7:  15.70% ( 13.50,  18.11)
##       8:  18.60% ( 16.23,  21.15)
##       9:  26.50% ( 23.79,  29.35)
##      10:  34.00% ( 31.06,  37.03)
##      11:  44.70% ( 41.59,  47.84)
##      12:  54.50% ( 51.35,  57.62)
##      13:  65.80% ( 62.77,  68.74)
##      14:  73.80% ( 70.96,  76.50)
##      15:  83.00% ( 80.53,  85.28)
##      16:  90.80% ( 88.84,  92.52)
##      17:  94.10% ( 92.46,  95.48)
##      18:  96.70% ( 95.40,  97.72)
##      19:  98.60% ( 97.66,  99.23)
##      20:  99.30% ( 98.56,  99.72)
## 
## Time elapsed: 0 h 31 m 48 s
```

```r
plot(pc3)
```

![plot of chunk powercurve3](figure/powercurve3.svg) 


Tutorial Four
-------------

We can have different values for alpha.


```r
plot(pc3, pval = c(0.01, 0.05, 0.1))
```

![plot of chunk powercurve3multi](figure/powercurve3multi.svg) 


Tutorial Five
-------------

Extend another dimension.


```r
model4 <- extend(model2, along = "g", n = 15)
pc4 <- powerCurve(model4, along = "g")
```

```
## Calculating power at 13 sample sizes for g
```

```r
print(pc4)
```

```
## Power to detect effect of x, (95% confidence interval):
## #levels for g 
##       3:  36.80% ( 33.80,  39.87)
##       4:  46.80% ( 43.67,  49.95)
##       5:  53.70% ( 50.55,  56.83)
##       6:  61.50% ( 58.40,  64.53)
##       7:  68.10% ( 65.11,  70.98)
##       8:  74.10% ( 71.27,  76.79)
##       9:  79.10% ( 76.45,  81.58)
##      10:  84.30% ( 81.89,  86.50)
##      11:  87.60% ( 85.40,  89.58)
##      12:  88.50% ( 86.36,  90.41)
##      13:  91.10% ( 89.16,  92.79)
##      14:  92.30% ( 90.47,  93.88)
##      15:  94.30% ( 92.68,  95.65)
## 
## Time elapsed: 0 h 24 m 16 s
```

```r
plot(pc4)
```

![plot of chunk powercurve4](figure/powercurve41.svg) 

```r
plot(pc4, pval = c(0.01, 0.05, 0.1))
```

![plot of chunk powercurve4](figure/powercurve42.svg) 

