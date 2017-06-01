rankhospital<- function(estado, resultado, num= "mejor"){
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
        Col <- 11}
    if(resultado == "neumonia"){
        Col <- 23}
    if(resultado == "falla"){
        Col <-17}
    u <- subset(outcome,outcome$State == estado)
    uCol <- suppressWarnings(as.numeric(u[,Col]))
    if(num == "peor"){
        max <- u[which.max(uCol),]
        H <- max[order(max$Hospital.Name , na.last = NA), ]
        HP <- H[1,2]
        HP
    }else if(num== "mejor"){
        min <-u[which.min(uCol),]
        H <- min[order(min$Hospital.Name , na.last = NA), ]
        HP <- H[1,2]
        HP
    } else {
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