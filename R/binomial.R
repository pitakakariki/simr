#
# Binomial models with cbind responses need special treatment
#

# does the model have a cbind response?
cbindReponse <- function(object) {

    # 1 is squiggle, 2 is response, 3 is predictor
    response <- formula(object)[[2]]



}


