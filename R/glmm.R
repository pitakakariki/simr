

#gm1.y <- with(cbpp, cbind(incidence, size - incidence))
#gm1 <- glmer(gm1.y ~ period + (1 | herd), data = cbpp, family = binomial)



#p <- with(example, plogis(y / 4 - 3))
#set.seed(123)
#b <- rbinom(length(p), 1, p)




doFit.glmerMod <- doFit.lmerMod