maybeTest <- function(z=sample(1:3, 1)) {
  
  if(z == 1) return(1)
  
  if(z == 2) {
    
    warning("z is 2.")
    return(2)
  }
  
  if(z == 3) stop("z is 3!")
  
  if(z == 4) {
    
    warning("z is 4.")
    warning("No, really: z is 4.")
    return(4)
  }
  
  if(z == 5) {
    
    warning("z is 5.")
    stop("z is 5!")
  }
  
  stop("Impossible!!")
  
}

maybe <- function(thing, returnName="return", warningName="warning", errorName="error") {
  
  returnValue <- NULL 
  warningValue <- NULL
  errorValue <- NULL
  
  returnValue <- tryCatch(
  
    withCallingHandlers(eval.parent(substitute(thing)),
    
    warning=function(w) {
      
      warningValue <<- append(warningValue, w)
      invokeRestart("muffleWarning")
    }),
    
    error=function(e) {
      
      errorValue <<- e
      return(NULL)
    }
  )
  
  rval <- list()
  class(rval) <- "Maybe"
  
  rval[returnName] <- list(returnValue)
  rval[warningName] <- list(warningValue)
  rval[errorName] <- list(errorValue)
  
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

