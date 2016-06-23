.simrCounter <- new.env(parent=emptyenv())

progress_simr <- function (text="", ...) {

    set <- function(x) {

        .simrCounter $ xp <- x
        updateProgress()
    }

    list(

        init = function(N) {

            .simrCounter $ Np <- N
            .simrCounter $ text <- text

            set(0)
        },

        step = function() {

            x <- .simrCounter $ xp
            N <- .simrCounter $ Np

            set(min(x+1, N))
        },

        term = function() {

            rm(xp, Np, text, envir=.simrCounter)

            if(exists("xc", .simrCounter)) updateProgress() else done()
        }
    )
}


counter_simr <- function() {

    set <- function(n) {

        .simrCounter $ xc <- n
        updateProgress()
    }

    list(

        init = function(N) {

            .simrCounter $ Nc <- N
            set(1)
        },

        step = function() {

            x <- .simrCounter $ xc
            N <- .simrCounter $ Nc

            set(min(x+1, N))
        },

        term = function() {

            rm(xc, Nc, envir=.simrCounter)

            done()
        }
    )
}

updateProgress <- function() {

    sc <- .simrCounter

    # build "(xc/Nc)"
    counter <- if(exists("xc", sc)) {

        str_c("(", str_pad(sc$xc, str_length(sc$Nc)), "/", sc$Nc, ") ")

    } else ""

    # build "Text: |===   |"
    progress <- if(exists("xp", sc)) {

        title <- if(sc$text == "") "" else str_c(sc$text, ": ")

        fullwidth <- getOption("width")
        width <- fullwidth - str_length(counter) - str_length(title) - 2L
        nbar <- trunc(sc$xp * width / sc$Np)

        str_c(title, "|", str_dup("=", nbar), str_dup(" ", width-nbar), "|")

    } else ""

    # combine
    newcounter <- str_c(counter, progress)

    # print

    if(!exists("oldcounter", sc)) {

        maybecat(newcounter)
        flush.console()
    }

    if(exists("oldcounter", sc) && newcounter != sc$oldcounter) {

        maybecat(str_dup("\b", str_length(sc$oldcounter)))
        maybecat(newcounter)
        flush.console()
    }

    sc $ oldcounter <- newcounter
}

maybecat <- function(...) if(getSimrOption("progress")) cat(..., sep="")

done <- function() {

    maybecat(str_dup("\b", str_length(.simrCounter $ oldcounter)))
    flush.console()

    rm(list=ls(.simrCounter), envir=.simrCounter)
}
