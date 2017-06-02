rankingcompleto <- function(resultado, num="mejor"){
    setwd("~/GitHub/Programacion_Actuarial_III/Caso 2")
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    Data <- as.character(c())
    DataF <- as.character(c())
    if((resultado != "ataque") && (resultado != "neumonia") && (resultado !="falla")){
        stop("Resultado inválido")
    }
    if(resultado == "ataque"){
        Col <- 11
    }else if(resultado == "neumonia"){
        Col <- 23
    }else {
        Col <-17
    }
    uCol <- outcome[c(2,7,Col)]
    EO <- levels(factor(outcome$State))
    for(EO1 in EO){
        
        tCol <- subset(uCol, uCol$State==EO1  & !uCol[[3]]=="Not Available")
        tColOr <- tCol[order(as.numeric(tCol[[3]]),tCol[[1]]),]
        if (num=="peor"){
            max <-tColOr[which.max(tColOr[[3]]),]
            H <- max$Hospital.Name
            Data <- c(Data,H)
        }else if(num=="mejor"){
            min <- tColOr[which.min(tColOr[[3]]),]
            H <- min$Hospital.Name
            Data <- c(Data,H)
        }else {
            H <- tColOr[num,1]
            Data <- c(Data,H)
        }
        DataF <- c(DataF, EO)
    }
    
    DF <- data.frame(Data,DataF)
    colnames(DF) <- c("Hospital", "State")
    DF
}
head(rankingcompleto("ataque", 20), 10)
tail(rankingcompleto("neumonia", "peor"), 3)
tail(rankingcompleto("falla"), 10)
