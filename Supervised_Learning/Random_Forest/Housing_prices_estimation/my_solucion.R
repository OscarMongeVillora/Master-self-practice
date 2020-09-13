rm(list=ls())

library(MASS)
boston <- Boston
boston
names(boston)

dim(boston)
train <- sample(1:nrow(boston), 300)
data.train <- boston[train,]
data.test <- boston[-train]

rf <- randomForest(medv~., 
                   data = data.train, 
                   ntree = 500,
                   importance = TRUE)


mean(rf$mse)
mean.mse <- c()
i_label <- c()

# Busco el n tree adecuado que minimiza la suma de error cuadrático 
plot(rf)


for(i in seq(1, 500, 10)) {
 
  rf <- randomForest(medv~., 
                     data = data.train, 
                     ntree = i,
                     importance = TRUE)
  
  #mean.mse <- append(mean.mse, mean(rf$mse))  # MAL
  mean.mse <- append(mean.mse, mean((rf$predicted - data.train$medv)^2))
  i_label <- append(i_label, i)
}
mean.mse[10]
length(mean.mse)
length(seq(1, 1000, 10))
mean.mse
plot(x = seq(1, 500, 10),
     y =  mean.mse,
     xlabel = "Number of trees",
     ylabel = "Mean of square error")


# Me fijo en el minimo en el intervalo entre 200 y 300
a <- seq(1, 1000, 10)
mean.interest <- mean.mse[a > 200 & a < 300]
min(mean.interest)
which.min(mean.interest)
b <- a[a > 200 & a < 300]
b[9]

#Implemento el modelo para 281 arboles
rf <- randomForest(medv~., 
                   data = data.train, 
                   ntree = 281,
                   importance = TRUE)


rf

