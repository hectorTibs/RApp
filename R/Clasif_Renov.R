

#' 
#' 
#' Basic hello world function to be called from the demo app
#' 
#' @export
#' 
#' 
#' 
#' 
#' 
Clasificacion <- function(){
#library("RODBC")
#odbcChannel <- odbcConnect("DataClasificacion", uid = "sa", pwd = "Tibs2016")
#ABA_NivelPoliza <- sqlFetch(odbcChannel, "Info_Aba_NivelPoliza")

ABA_NPR <- read.csv("https://raw.githubusercontent.com/hectorTibs/RApp/master/R/ABA_NPR.csv")

library(e1071)
# Selección de una submuestra de 105 (el 70% de los datos)
set.seed(101)
ABA.indices <- sample(1:nrow(ABA_NPR), nrow(ABA_NPR)*0.7)
ABA.entrenamiento <- ABA_NPR[ABA.indices,]


ABA.test <- ABA_NPR[-ABA.indices,]

model <- naiveBayes(Renovado ~ ., data = ABA.entrenamiento)

# Importancia de cada variable
#model$importance

# predict necesita el parámetro newdata
results <- predict(object = model, newdata = ABA.test, type = "raw")
results2 <- predict(object = model,ABA.test, type = "class" )

r<- round(results * 100)

mc <- table(results2, ABA.test$Renovado)
mc 

# Correctamente clasificados
x <- 100 * sum(diag(mc)) / sum(mc)

#Agregar Columna de Prediccion a la tabla ABA.test
ABA.test$PrediClas <- r
ABA.test$PClas <- results2
data.frame(PrediClas)


#########################################################################
####################### OBTENER RENOVACION POR PRODUCTO #################
#########################################################################

#df <- data.frame(ABA.test$Producto , r)
#write.csv(df,"Sumatoria.csv")

list( data =ABA.test )
}