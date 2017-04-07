mediacontaminante <- function(directorio, contaminante, id = 1:332) {
    contaminante1<- c()
    for (i in id){
        
        
        idn <- formatC(i,width=3,flag = "0")
        readen <- read.csv(paste(idn, ".csv", sep=""), header = T)
        
        if (contaminante != "sulfate"){ #es distinto de sulfato
            
            nitrate <- (readen$nitrate)#se leera la columna de nitrate
            
            contaminante1 <- c(contaminante1, nitrate)#contaminante1 sera}
            #todos los datos almacenados
        } else if (contaminante != "nitrate") {
            sulfate <- (readen$sulfate)
            
            contaminante1 <- c(contaminante1, sulfate)
            
        }
        
        
        
        
    }
    
    datos <- contaminante1[!is.na(contaminante1)]#borra todos los valores faltantes
    media <- mean(datos)#calcula la media
    media#imprime
}