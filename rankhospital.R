rankhospital<- function(estado, resultado, num= "mejor"){
    setwd("~/GitHub/Programacion_Actuarial_III/Caso 2")
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    y <- levels(factor(levels(factor(outcome$State)) == estado))
    x <- is.na(y[2]) || as.logical(y[1])
    if(x == TRUE){
        stop("Estado invalido")
    } |
        if((resultado != "ataque") && (resultado != "neumonia") && (resultado !="falla")){
            stop("Resultado inválido")
        }
    if(resultado == "ataque"){
        Col <- 11}
    if(resultado == "neumonia"){
        Col <- 23}
    if(resultado == "falla"){
        Col <-17}
    uCol <- outcome[c(2,7,Col)]
    tCol <- subset(uCol, uCol$State==estado  & !uCol[[3]]=="Not Available")
    tColOr <- tCol[order(as.numeric(tCol[[3]]),tCol[[1]]),]
    if(num == "peor"){
        max <- tColOr[which.max(tColOr[[3]]),]
        H <- max$Hospital.Name
        H
    }else if(num== "mejor"){
        min <-tColOr[which.min(tColOr[[3]]),]
        H <- min$Hospital.Name
        H
    } else {
        u <- subset(outcome,outcome$State == estado)
        uCol <- suppressWarnings(as.numeric(u[,Col]))
        O <- u[(uCol),]
        ordenar <- order(uCol,u[,2])
        H <- u[ordenar,2]
        HP <- H[num]
        HP
    }
}

rankhospital("TX", "falla", 4)
rankhospital("MD", "ataque", "peor")
rankhospital("MN", "ataque", 5000)