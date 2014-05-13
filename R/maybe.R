maybeTest <- function(z=sample(1:4, 1)) {
  
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
  
  if(z > 4) {
    
    warning("I can't count that high.")
    stop("z is too big!")
  }
  
  stop("Impossible!!")
  
}

maybe <- function(f, returnName="return", warningName="warning", errorName="error")
  function(...) {
  
  returnValue <- NULL 
  warningValue <- NULL
  errorValue <- NULL
  
  returnValue <- tryCatch(
  
    withCallingHandlers(eval.parent(f(...)),
    
    warning=function(w) {
      
      warningValue <<- append(warningValue, w$message)
      invokeRestart("muffleWarning")
    }),
    
    error=function(e) {
      
      errorValue <<- e$message
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

llMaybe <- function(.data, .fun, .text, ...) {
  
  z <- llply(.data, maybe(.fun), ..., .progress=progress_simr(.text))
  
  rval <- llply(z, `[[`, "return")

  warn <- llply(z, `[[`, "warning")
  err <- llply(z, `[[`, "error")
  
  attr(rval, "warnings") <- warn
  attr(rval, "errors") <- err
  
  return(rval)
}