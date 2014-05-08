maybeTest <- function(z=sample(1:3, 1)) {
  
  if(z == 1) return(1)
  
  if(z == 2) {
    
    warning("z is 2.")
    return(2)
  }
  
  if(z == 3) stop("z is 3!")
  
  stop("Impossible!!")
  
}

maybe <- function(thing, returnName="return", warningName="warning", errorName="error") {
  
  returnValue <- NULL 
  warningValue <- NULL
  errorValue <- NULL
  
  rval <- tryCatch(
  
    withCallingHandlers(eval.parent(substitute(thing)),
    
    warning=function(w) {
      
      print("Yes")
      rval <- invokeRestart("muffleWarning")
      print("No?")
      attr(rval, warningName) <- w
      
      return(rval)
    }),
    
    error=function(e) {
      
      rval <- NA
      attr(rval, errorName) <- e
      
      return(rval)
    }
  )
  
  rval <- list()
  class(rval) <- "Maybe"
  
  rval[[returnName]] <- returnValue
  rval[[warningName]] <- warningValue
  rval[[errorName]] <- errorValue
  
  return(rval)
}

# http://stackoverflow.com/users/210673/aaron
catchToList <- function(expr) {
  val <- NULL
  myWarnings <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, w$message)
    invokeRestart("muffleWarning")
  }
  myError <- NULL
  eHandler <- function(e) {
    myError <<- e$message
    NULL
  }
  val <- tryCatch(withCallingHandlers(expr, warning = wHandler), error = eHandler)
  list(value = val, warnings = myWarnings, error=myError)
} 

