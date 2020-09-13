#Borramos los datos
rm(list=ls())


#-------------- Pregunta 1: Lectura de datos ----------#

library(MASS)
#data(package="MASS")
boston<-Boston
boston
dim(boston)
names(boston)

#-------------- Pregunta 2: datos de train ----------#
set.seed(101)
train = sample(1:nrow(boston), 300) #seleccionamos 300 valores para entrenar


#-------------- Pregunta 3: Ajuste del modelo ----------#

rf.boston = randomForest(medv~., data = boston, subset = train,ntree=500, importance = TRUE)
rf.boston

#como se calcula el % varianza explicada?
predicted=rf.boston$predicted
y=boston$medv[train]
1 - sum((y-predicted)^2)/sum((y-mean(y))^2) #=R^2


mean((predicted - boston$medv[train])^2)
#-------------- Pregunta 4: Arboles vs. error ----------#
rf.boston
plot(rf.boston)

pred = predict(rf.boston, boston[train,])  #No es lo mismo
predicted=rf.boston$predicted # No es lo mismo

#-------------- Pregunta 5: oob error vs test error ----------#

#vamos a intentar para cada valor de mtry posible
oob.err = double(13) #out of bag error, cuantas variables dejo sin introducir en el bosque
test.err = double(13)
oob.err
for(mtry in 1:13){
  fit = randomForest(medv~., data = boston, subset=train, mtry=mtry, ntree = 350)
  oob.err[mtry] = fit$mse[350] #por que elijo aqui solo el ultimo valor (350)?
  
  pred = predict(fit, boston[-train,])
  test.err[mtry] = with(boston[-train,], mean( (medv-pred)^2 ))
}



#-------------- Pregunta 6: Grafico oob error vs test error ----------#

matplot(1:mtry, cbind(test.err, oob.err), pch = 23, col = c("red", "blue"), type = "b", ylab="Mean Squared Error")
legend("topright", legend = c("OOB", "Test"), pch = 23, col = c("red", "blue"))


cbind(test.err, oob.err)

min(boston$medv)
max(boston$medv)

##############################Apartado mio extra  RF FOR CLASSIFICATION
#ROC PLOT Variable binaria barato, Caro Por encima de 35 es caro -- Y = 1
install.packages("verification")
library(verification)

par(mfrow = c(3, 3))
Y <- factor(ifelse(boston$medv > 30, 1, 0))

boston <- data.frame(boston, Y)
boston$Y
fit = randomForest(Y~.-medv, data = boston, subset=train, ntree = 350)

fit$err.rate
fit$votes

train_predict = list()
for(mtry in 1:9){
  fit = randomForest(Y~.-medv, data = boston, subset=train, mtry=mtry, ntree = 350)
  
  train_predict[[mtry]] = fit$votes[,2]
  
}


yy <- as.numeric(as.character(Y[train]))
for(i in 
    1:9){
  r <- roc.area(yy, train_predict[[i]])
  roc.plot(yy, train_predict[[i]], 
           main = paste0("Var= ", i," ROC_a= ", r$A))
}


