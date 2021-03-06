
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
ClasificacionCancelacion <- function(CvePoliza = "",CvePolizaPrevia = "",InVigPoliza = "",FinVigPoliza = "",Vendedor = "",Producto = "",TipoEmision = "",TiendaDanos = "",SucursalDanos = "",OfVntDanos = "",TipoProducto = "",PrimaNetaPesos = "",SumaAsegurada = "",EstPoliza = ""){
 
  ###################### LECTOR DE ARCHIVO ####################
  ABA_NivelPoliza <- ("https://raw.githubusercontent.com/hectorTibs/RApp/master/R/ABA_NivelPoliza.csv")
  ABA_NP <- read.csv(ABA_NivelPoliza,header = TRUE)
  
  ############ ENTRENAMIENTO DE RED  Y TESTEO DE #############
  ##################TEOREMA DE BAYES #########################
  library(e1071)
  # Selecci?n de una submuestra el 70% de los datos
  set.seed(101)
  ABA.indices <- sample(1:nrow(ABA_NP), nrow(ABA_NP) * 0.7)
  ABA.entrenamiento <- ABA_NP[ABA.indices,]
  ABA.test <- ABA_NP[-ABA.indices,]
  model <- naiveBayes(EstPoliza ~ ., data = ABA.entrenamiento)
  
  ####################### PREDICCION ###############################
  results <- predict(object = model, newdata = ABA.test, type = "raw")
  #results2 <- predict(object = model, ABA.test, type = "class")
  #mc <- table(results,ABA.test$EstPoliza)
  #mc
  Porcentaje <- round(results*100)
  # Correctamente clasificados
  #x <- 100 * sum(diag(mc)) / sum(mc)
  #Agregar Columna de Prediccion a la tabla ABA.test
  ABA.test$PrediClas <- Porcentaje
  #ABA.test$PClas <- results2
  #write.csv(ABA.test, "Clasif_Canc2.csv")
  
  ########### PRUEBA DE ARCHIVO RECIBIDO csv #####################
  #Prueba <- ("C:\\Users\\V Tibs\\Desktop\\Prueba.csv")
  #PruebaR <- read.csv(Prueba, header = TRUE)
  #PruebaP <- predict(object= model, newdata = PruebaR, type =  "raw")
  #prue <- PruebaP * 100
  
  ############## JSON ###########################################
  
  library(rjson)
  x<-list("CvePoliza"=CvePoliza,"CvePolizaPrevia"=CvePolizaPrevia,"InVigPoliza"=InVigPoliza,"FinVigPoliza"=FinVigPoliza,"Vendedor"=Vendedor,"Producto"=Producto,"TipoEmision"=TipoEmision,"TiendaDanos"=TiendaDanos,"SucursalDanos"=SucursalDanos,"OfVntDanos"=OfVntDanos,"TipoProducto"=TipoProducto,"PrimaNetaPesos"=PrimaNetaPesos,"SumaAsegurada"=SumaAsegurada,"EstPoliza"=EstPoliza)
  json<- toJSON(x)
  data = fromJSON(json)
  Pruebajson <- as.data.frame(data)
  #print(Pruebajson)
  #vectordatos <- list("CvePoliza"=CvePoliza,"CvePolizaPrevia"=CvePolizaPrevia,"InVigPoliza"=InVigPoliza,"FinVigPoliza"=FinVigPoliza,"Vendedor"=Vendedor,"Producto"=Producto,"TipoEmision"=TipoEmision,"TiendaDanos"=TiendaDanos,"SucursalDanos"=SucursalDanos,"OfVntDanos"=OfVntDanos,"TipoProducto"=TipoProducto,"PrimaNetaPesos"=PrimaNetaPesos,"SumaAsegurada"=SumaAsegurada,"EstPoliza"=EstPoliza)
  
  #Pruebajson <- as.data.frame(vectordatos)
  
  
  
  Prueba2<- predict(object = model, newdata = Pruebajson, type = "raw")
  PorcentajeP <- Prueba2 * 100
  resultado <- as.data.frame(PorcentajeP)
  names(resultado)<-c("Cancelada","Vencida","Vigente")
  list(resultado)
}
  

