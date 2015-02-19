# reproducible:
set.seed(123)

# cts and categorical predictors:
x <- rep(1:10, times=3)
g <- as.factor(rep(c('a', 'b', 'c'), each=10))

# parameters
a <- 10
b <- -0.25
s <- 1
r <- 3

# random effects:
re <- rnorm(nlevels(g), 0, r)
names(re) <- levels(g)

# linear response
y <- a + b*x + re[g] + rnorm(length(x), 0, s)

# poisson response:
lambda <- exp((a + b*x + re[g])/10)
z <- rpois(length(x), lambda)





example <- data.frame(y=y, x=x, g=g, z=z)
save(example, file='data/example.rda')

