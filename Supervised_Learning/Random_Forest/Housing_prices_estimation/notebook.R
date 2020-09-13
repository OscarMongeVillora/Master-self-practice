#Borramos los datos
rm(list=ls())

#-------------- Pregunta 1: Lectura de datos ----------#

library(MASS)
#data(package="MASS")
boston<-NULL
dim(NULL)
names(NULL)


#-------------- Pregunta 2: datos de train ----------#
set.seed(101)
train = sample(NULL, 300) #seleccionamos 300 valores para entrenar


#-------------- Pregunta 3: Ajuste del modelo ----------#

rf.boston = randomForest(NULL~., data = NULL, subset = NULL,ntree=NULL)
rf.boston

#como se calcula el % varianza explicada?
predicted=rf.boston$predicted
y=boston$medv[NULL]
1 - NULL #=R^2

#-------------- Pregunta 4: Arboles vs. error ----------#

plot(NULL)



#-------------- Pregunta 5: oob error vs test error ----------#

#vamos a intentar para cada valor de mtry posible
oob.err = double(13) #out of bag error, cuantas variables dejo sin introducir en el bosque
test.err = double(13)
for(mtry in 1:13){
  fit = randomForest(NULL~., data = NULL, subset=NULL, mtry=NULL, ntree = 350)
  oob.err[mtry] = fit$mse[NULL] #que valor colocas aqui y por que?
  
  pred = predict(NULL, NULL) #en la primera coordenada debe ir el modelo y luego los datos que quiere predecir
  test.err[mtry] = with(boston[-train,], NULL) #que medida calculas aqui
}




#-------------- Pregunta 6: Grafico oob error vs test error ----------#

#En la primera coordenada coloque el numero de iteraciones, recuerda que la funcion cbind se utiliza
#para combinar por culumnas los vectores, por tanto coloca los vectores que deseas "plotear"
matplot(NULL, cbind(NULL, NULL), pch = 23, col = c("red", "blue"), type = "b", ylab="Mean Squared Error")
legend("topright", legend = c("OOB", "Test"), pch = 23, col = c("red", "blue"))







