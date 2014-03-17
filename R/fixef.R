#' Modify fixed effect coefs.
#'
#' This function replaces the fixed effect coefficients in a fitted model.
#'
#' @export
#'
#' @usage fixef(x)[index] <- value
#'
#' @param object a mixed-effects model (merMod) object.
#' @param value  a new vector of fixed effect coefficients.
#'
#' @details
#' 
#' This function would normally be used to change the value of individual fixed effect
#' coefficients.
#'
#' @examples
#' a <- lmer(Carbon ~ Year + (Year | Cluster), kiwifruit)
#' fixef(a)
#' fixef(a)['Year'] <- -0.13
#' fixef(a)
#'
`fixef<-` <- function(object, value) {

    fixefNames <- colnames(getME(object, 'X'))
    nameTest <- setdiff(names(value), fixefNames)
    
    if(length(nameTest) != 0) {
        
        stop(str_c(nameTest[[1]], " is not the name of a fixed effect."))
    }
    
    object @ beta <- unname(value)
    
    return(object)
}


