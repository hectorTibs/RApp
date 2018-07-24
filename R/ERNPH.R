instancia <- "C://Users//fer_m//Desktop//Hector Esparza//TIBS//R//3.csv"
inputTarget.grid <- read.csv(instancia,header=FALSE)
##Encuentro el valor maximo en la instancia
maxValue=max(inputTarget.grid[,1:4])


##Toma las primeras 3 columnas como entrada
##Toma las ultimas 3 columnas como objetivo
##En ambas divido entre maxValue para normalizar
input = inputTarget.grid[,1:2]/maxValue
targ = inputTarget.grid[,3:4]/maxValue

#Calcula la red con 3 neuronas ocultas, solo tomo los 
#primeros 12 renglones (patrones) para entrenar la red
#f= round(nrow(inputTarget.grid)*0.8)
##producto.nnet = nnet(input[1:12,], targ[1:12,], size=3, rang=1, maxit=1000, trace=FALSE)
producto.nnet = nnet(input[1:60,], targ[1:60,], size=8, rang=1, maxit=1000, trace=FALSE)



#Calculo el error
diferencia=(targ[1:60,]-producto.nnet$fitted.values)^2
i=0;error_ind=c()
for(i in 1:60)
{  
  error_ind[i]=sum(diferencia[i,])/8
}
error = sum(error_ind)/60
error

#La funcion predict utiliza los pesos obtenidos para calcular el pronostico
#En este caso, enviamos todo input (los 15 patrones), lo cual nos dar????a 
#como resultado una tabla de 15 renglones (los primeros 12 son iguales a 
#la variable productos.nnet$fitted.values, y los 3 rengones restantes 
#ser????an el pronostico obtenido)
output = predict(producto.nnet, input)

#Esta grafica muestra compara la primer neurona objetivo
# y la primer neurona de salida (los primeros 12 puntos son el entrenamiento
# y los ultimos 3 fueron pronosticados en base al entrenamiento
plot(targ[,1],type="o", col= "green")
lines(output[,1],type="o",col="red")

