#
# Binomial models with cbind responses need special treatment
#

# does the model have a cbind response?
cbindResponse <- function(object) {

    family(object)$family=="binomial" && is.matrix(model.response(object@frame))
}


