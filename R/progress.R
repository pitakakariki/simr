progress_simr <- function (text="", ...) {
    
    set <- function(x) {

        .SIMRCOUNTER $ xp <<- x
        updateProgress()
    }
    
    list(

        init = function(N) {

            if(!exists(".SIMRCOUNTER")) .SIMRCOUNTER <<- list()
            
            .SIMRCOUNTER $ Np <<- N
            .SIMRCOUNTER $ text <<- text
            
            set(1)
        },
        
        step = function() {
            
            x <- .SIMRCOUNTER $ xp
            N <- .SIMRCOUNTER $ Np

            set(min(x + 1, N))
        },
        
        term = function() {

            .SIMRCOUNTER $ xp <<- NULL
            .SIMRCOUNTER $ Np <<- NULL
            .SIMRCOUNTER $ text <<- NULL
            
            if(is.null(.SIMRCOUNTER $ xc)) done() else updateProgress()                
        }
    )
}


counter_simr <- function() {
    
    set <- function(n) {

        .SIMRCOUNTER $ xc <<- n
        updateProgress()
    }
        
    list(
        
        init = function(N) {

            .SIMRCOUNTER <<- list(Nc=N)
            
            set(1)
        },
        
        step = function() {
            
            x <- .SIMRCOUNTER $ xc
            N <- .SIMRCOUNTER $ Nc
            
            set(min(x+1, N))
        },
        
        term = function() {

            .SIMRCOUNTER $ xc <<- NULL
            .SIMRCOUNTER $ Nc <<- NULL
            
            done()
        }
    )    
}

updateProgress <- function() {
    
    with(.SIMRCOUNTER, {
        
        # build "(xc/Nc)"
        counter <- if(exists("xc", inherits=FALSE)) {
            
            str_c("(", str_pad(xc, str_length(Nc)), "/", Nc, ") ")
        } else ""
        
        # build "Text: |===   |"
        progress <- if(exists("xp", inherits=FALSE)) {
            
            title <- if(exists("text", inherits=FALSE) && text != "") str_c(text, ": ") else ""
            fullwidth <- getOption("width")
            width <- fullwidth - str_length(counter) - str_length(title) - 2L
            nbar <- trunc(xp * width / Np)
            
            str_c(title, "|", repchar("=", nbar), repchar(" ", width-nbar), "|") 
        } else ""

        # combine
        newcounter <- str_c(counter, progress)
        
        # print
        if(exists("oldcounter", inherits=FALSE) && newcounter != oldcounter) {
            
            maybecat(repchar("\b", str_length(oldcounter)))
            maybecat(newcounter)
            flush.console()
        }
        
        .SIMRCOUNTER $ oldcounter <<- newcounter
    })    
}

maybecat <- function(...) if(getSimrOption("progress")) cat(..., sep="")

repchar <- function(x, n) str_pad("", pad=x, width=n)

done <- function() {
    
    maybecat(repchar("\b", str_length(.SIMRCOUNTER $ oldcounter)))
    flush.console()
    
    rm(.SIMRCOUNTER, inherits=TRUE)
}

