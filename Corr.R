setwd("~/specdata")
corr <- function(directorio, horizonte=0) {
    setwd("~/specdata")
    
    id <- 1:332
    vector1 <- c()
    for (i in id) {
        hori <- 1:horizonte #condicion de cuantos casos completos quiere
        id2 <- formatC(i,width =3,flag="0") #le da formato para abrir el archivo
        readid <- read.csv(paste(id2,".csv",sep=""),header=TRUE,sep =",")
        corre <-subset(readid,complete.cases(readid) , select = c(sulfate,nitrate))
        #con la funcion subset crea un data frame de readid, con la condicion
        #de que sean casos completos y que ponga la columna de sul y nit
        if (horizonte > 0) {
            f<- corre[hori,]
            y <- cor(f$sulfate,f$nitrate) #calacula la correlacion
            vector1 <- c(vector1, y)  
            #si horizonte es mayor a 0 se hace el if para que se pongan
            #unicamente los caso completos que se requieren
        } else {
            
            #si horizonte=0 entonces pone todos los casos completos
            y <- cor(corre$sulfate,corre$nitrate)
            vector1 <- c(vector1, y)
        }
    }
    vector1
}

corr("specdata",horizonte = 5)