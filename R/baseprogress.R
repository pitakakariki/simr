plyr_progress_text <- function (style = 3, ...) {
    
    n <- 0
    txt <- NULL
    list(init = function(x) {
        txt <<- txtProgressBar(max = x, style = style, ...)
        setTxtProgressBar(txt, 0)
    }, step = function() {
        n <<- n + 1
        setTxtProgressBar(txt, n)
    }, term = function() close(txt))
}

base_setTxtProgressBar <- function (pb, value, title = NULL, label = NULL) {
    
    if (!inherits(pb, "txtProgressBar")) 
        stop(gettextf("'pb' is not from class %s", dQuote("txtProgressBar")), 
            domain = NA)
    oldval <- pb$getVal()
    pb$up(value)
    invisible(oldval)
}

base_txtProgressBar <- function (min = 0, max = 1, initial = 0, char = "=", width = NA, 
    title, label, style = 1, file = "") {
    
    if (!identical(file, "") && !(inherits(file, "connection") && 
        isOpen(file))) 
        stop("'file' must be \"\" or an open connection object")
    if (!style %in% 1L:3L) 
        style <- 1
    .val <- initial
    .killed <- FALSE
    .nb <- 0L
    .pc <- -1L
    nw <- nchar(char, "w")
    if (is.na(width)) {
        width <- getOption("width")
        if (style == 3L) 
            width <- width - 10L
        width <- trunc(width/nw)
    }
    if (max <= min) 
        stop("must have 'max' > 'min'")
    up1 <- function(value) {
        if (!is.finite(value) || value < min || value > max) 
            return()
        .val <<- value
        nb <- round(width * (value - min)/(max - min))
        if (.nb < nb) {
            cat(paste(rep.int(char, nb - .nb), collapse = ""), 
                file = file)
            flush.console()
        }
        else if (.nb > nb) {
            cat("\r", paste(rep.int(" ", .nb * nw), collapse = ""), 
                "\r", paste(rep.int(char, nb), collapse = ""), 
                sep = "", file = file)
            flush.console()
        }
        .nb <<- nb
    }
    up2 <- function(value) {
        if (!is.finite(value) || value < min || value > max) 
            return()
        .val <<- value
        nb <- round(width * (value - min)/(max - min))
        if (.nb <= nb) {
            cat("\r", paste(rep.int(char, nb), collapse = ""), 
                sep = "", file = file)
            flush.console()
        }
        else {
            cat("\r", paste(rep.int(" ", .nb * nw), collapse = ""), 
                "\r", paste(rep.int(char, nb), collapse = ""), 
                sep = "", file = file)
            flush.console()
        }
        .nb <<- nb
    }
    up3 <- function(value) {
        if (!is.finite(value) || value < min || value > max) 
            return()
        .val <<- value
        nb <- round(width * (value - min)/(max - min))
        pc <- round(100 * (value - min)/(max - min))
        if (nb == .nb && pc == .pc) 
            return()
        cat(paste(c("\r  |", rep.int(" ", nw * width + 6)), collapse = ""), 
            file = file)
        cat(paste(c("\r  |", rep.int(char, nb), rep.int(" ", 
            nw * (width - nb)), sprintf("| %3d%%", pc)), collapse = ""), 
            file = file)
        flush.console()
        .nb <<- nb
        .pc <<- pc
    }
    getVal <- function() .val
    kill <- function() if (!.killed) {
        cat("\n", file = file)
        flush.console()
        .killed <<- TRUE
    }
    up <- switch(style, up1, up2, up3)
    up(initial)
    structure(list(getVal = getVal, up = up, kill = kill), class = "txtProgressBar")
}