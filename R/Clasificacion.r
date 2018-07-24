#library("RODBC")
#odbcChannel <- odbcConnect("DataClasificacion", uid = "sa", pwd = "Tibs2016")
#ABA_NivelPoliza <- sqlFetch(odbcChannel, "Info_Aba_NivelPoliza")

ABA_NivelPoliza <-
  "C://Users//fer_m//Desktop//Hector Esparza//TIBS//R//RApp//RApp//R//ABA_NivelPoliza.csv")
ABA_NP <- read.csv(ABA_NivelPoliza, header = TRUE)

library(e1071)
# Selección de una submuestra de 105 (el 70% de los datos)
set.seed(101)
ABA.indices <- sample(1:nrow(ABA_NP), nrow(ABA_NP) * 0.7)
ABA.entrenamiento <- ABA_NP[ABA.indices,]
ABA.test <- ABA_NP[-ABA.indices,]

model <- naiveBayes(EstPoliza ~ ., data = ABA.entrenamiento)

# Importancia de cada variable
#model$importance


# predict necesita el parámetro newdata
results <- predict(object = model, newdata = ABA.test, type = "raw")
results2 <- predict(object = model, ABA.test, type = "class")

mc <- table(results2, ABA.test$EstPoliza)
mc

# Correctamente clasificados
x <- 100 * sum(diag(mc)) / sum(mc)

#Agregar Columna de Prediccion a la tabla ABA.test
ABA.test$PrediClas <- results
ABA.test$PClas <- results2
