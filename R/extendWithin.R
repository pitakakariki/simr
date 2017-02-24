# adds a .simr_repl variable to a data.frame

addReplicateIndex <- function(data, factors) {

    factors <- str_split(factors, "[\\+,]")[[1]]
    factors <- str_trim(factors)

    #f <- eval(substitute(with(data, interaction(...))))
    x <- lapply(factors, get, data)
    f <- do.call(interaction, x)
    repl <- lapply(table(f), seq_len)

    data$.simr_repl <- 0
    for(i in levels(f)) {

        data$.simr_repl[f==i] <- repl[[i]]
    }

    return(data)
}


balance <- function(data, within, n) {

    if(missing(n)) n <- 0






}