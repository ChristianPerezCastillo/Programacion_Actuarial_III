mejor <- function(estado, resultado){
    setwd("~/GitHub/Programacion_Actuarial_III/Caso 2") 
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    y <- levels(factor(levels(factor(outcome$State)) == estado))
    u <- is.na(y[2]) || as.logical(y[1])
    if(u == TRUE){
        stop("Estado invalido")
    }
    if((resultado != "ataque") && (resultado != "neumonia") && (resultado !="falla")){
        stop("Resultado inválido")
    }
    if(resultado == "ataque"){
        Col <- 11
    }else if(resultado == "neumonia"){
        Col <- 23
    }else {
        Col <-17}
    uCol <- outcome[c(2,7,Col)]
    tCol <- subset(uCol, uCol$State==estado  & !uCol[[3]]=="Not Available")
    tColOr <- tCol[order(as.numeric(tCol[[3]]),tCol[[1]]),]
    min <-tColOr[which.min(tColOr[[3]]),]
    H <- min$Hospital.Name
    H
}
mejor("TX", "ataque")
mejor("TX", "falla")
mejor("MD", "ataque")
mejor("MD", "neumonia")
mejor("BB", "ataque")
mejor("NY", "atakue")
