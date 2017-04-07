setwd("~/specdata")
completos <- function(directorio,id=1:332) {
    setwd("~/specdata")
    nobs <- c()
    for (i in id) {
        id2 <- formatC(i,width =3,flag="0") #da formato
        readid <- read.csv(paste(id2,".csv",sep=""),header=TRUE,sep =",")
        complete.cases(readid)  #arroja el numero de datos completos
        x <- sum(complete.cases(readid)) #suma los datos completos
        nobs <- c(nobs, x)  #se guarda para arrojar un vector con los resul.
    }
    y <- data.frame(id,nobs)
    y
}

completos("specdata",1:332)