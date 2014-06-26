Increase number of simulations
========================================================










Change the value of `nSim` to get more precise power estimates:


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

powerSim(model1, nSim = 500)
```

```
## Power to detect effect of x, (95% confidence interval):
##  97.20% ( 95.33,  98.33)
## 
## Based on 500 simulations and effect size -0.24
```

```r

powerSim(model1, nSim = 1000)
```

```
## Power to detect effect of x, (95% confidence interval):
##  96.60% ( 95.28,  97.56)
## 
## Based on 1000 simulations and effect size -0.24
```

```r

powerSim(model1, nSim = 2000)
```

```
## Power to detect effect of x, (95% confidence interval):
##  96.20% ( 95.27,  96.95)
## 
## Based on 2000 simulations and effect size -0.24
```


