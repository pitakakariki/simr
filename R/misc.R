getFrame <- function(a) {
    
    if(inherits(a, 'lm')) return(a$model)
    if(inherits(a, 'merMod')) return(a@frame)
    
    stop(str_c('Don\'t know how to get frame for class ', class(a)))    
}
