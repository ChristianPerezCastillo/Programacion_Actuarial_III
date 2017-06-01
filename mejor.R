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
    u <- subset(outcome,outcome$State == estado)
    uCol <- suppressWarnings(as.numeric(u[,Col]))
    Min <- which.min(uCol)
    uMin <- u[Min,]
    H <- uMin[order(uMin$Hospital.Name , na.last = NA),]
    HS <- H[1,2]
    HS
}
mejor("TX", "ataque")
mejor("TX", "falla")
mejor("MD", "ataque")
mejor("MD", "neumonia")
mejor("BB", "ataque")
mejor("NY", "atakue")

