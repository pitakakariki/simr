fff <- function(data, ...) {

    f <- eval(substitute(with(data, interaction(...))))
    repl <- lapply(table(f), seq_len)

    data$.simr_repl <- 0
    for(i in levels(f)) {

        data$.simr_repl[f==i] <- repl[[i]]
    }

    return(data)
}
