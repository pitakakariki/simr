doFit.glm <- function(y, model, subset) {
  
  newData <- model$model
  responseName <- model$call$formula[[2]]
  newData[[responseName]] <- y
  
  newData <- newData[subset, ]
  
  model$call[["data"]] <- quote(newData)
  
  rval <- eval(model$call)
  
  return(rval)    
}

#doSim.glm <- doSim.default

fixef.glm <- coef

#getDefaultTest.glm <- function() {}

counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
print(d.AD <- data.frame(treatment, outcome, counts))
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
anova(glm.D93)
summary(glm.D93)