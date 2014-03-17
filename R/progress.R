.SIMRPROGRESS <- TRUE

maybecat <- function(.) if(.SIMRPROGRESS) cat(.)

setSimrProgress <- function(x) {
    
    if(x) .SIMRPROGRESS <<- TRUE else .SIMRPROGRESS <<- FALSE
}

progress_simr <- function (title="", ...) {
    
    N <- 1
    n <- 0
    
    if(title != "") title <- str_c(title, ": ")
    if(exists(".SIMRCOUNTER")) title <- str_c(.SIMRCOUNTER, " ", title)
    
    fullwidth <- getOption("width")
    width <- fullwidth - str_length(title) - 2L
    
    set <- function(n) {
        
        nbar <- trunc(n * width / N)
        bar <- str_pad("", pad="=", width=nbar)
        blank <- str_pad("", pad=" ", width=width-nbar)

        setStr <- str_c(title, "|", bar, blank, "|")
    
    ##TODO## does this still need to be here?    
    stopifnot(str_length(setStr) == fullwidth)
        
        maybecat("\r")
        flush.console()
        
        maybecat(setStr)
        flush.console()
    }
    
    list(

        init = function(x) {

            N <<- x
            set(n)
        },
        
        step = function() {
            
            n <<- min(n + 1, N)
            set(n)
        },
        
        term = function() {

            maybecat("\r")
            maybecat(str_pad("", pad=" ", width=fullwidth))
            maybecat(str_c("\r", if(!exists('.SIMRCOUNTER') || .SIMRCOUNTER == "") "Done" else .SIMRCOUNTER))
            flush.console()
        }
    )
}


counter_simr <- function() {
    
    N <- 1
    n <- 0
    
    Nwidth <- 1
    fullwidth <- 5
    
    set <- function(n) {

        setStr <- str_c("(", str_pad(n, Nwidth), "/", N, ")")
        maybecat("\r")
        maybecat(setStr)

        .SIMRCOUNTER <<- setStr

        flush.console()
    }
        
    list(
        
        init = function(x) {
            
            N <<- x
            n <<- 1
            
            Nwidth <<- str_length(N)
            fullwidth <<- 3 + 2 * Nwidth
            
            set(n)
        },
        
        step = function() {
            
            n <<- min(n + 1, N)
            set(n)            
        },
        
        term = function() {

            .SIMRCOUNTER <<- ""
            
            maybecat("\r")
            maybecat(str_pad("", pad=" ", width=fullwidth))
            maybecat("\rDone")
            flush.console()
        }
    )    
}

