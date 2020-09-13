install.packages("ISLR")
rm(list = ls())


library(ISLR)
data("Khan")
names(Khan)
unique(ytest)
xtrain = sapply(data.frame(Khan$xtrain), as.numeric)
ytrain = sapply(data.frame(Khan$ytrain), as.factor)
xtest = sapply(data.frame(Khan$xtest), as.numeric)
ytest = sapply(data.frame(Khan$ytest), as.factor)

xtrain
dim(xtrain)
dim(xtest)
# El numero de predictores es mucho mayor que el numero de observaciones
# tiende a tener overfitting, mejor usar kernel lineal

library(e1071)

# Como la variable respuesta está separa de los predictores, se unen en un único 
# dataframe. La variable respuesta tiene que ser de tipo factor.
datos_train <- data.frame( y = as.factor(ytrain), xtrain)

svm_cv <- tune("svm", y ~ ., data = datos_train, kernel = 'linear',
               ranges = list(cost = c(0.0005, 0.001, 0.01)))

svm_model <- svm(y ~ ., data = datos_train, kernel = 'linear',
               cost = 0.0005)
svm_model$SV[,1:3]
svm_cv
summary(svm_cv)
best <- svm_cv$best.model
best$coefs
colnames(datos_train[, 1:3])
datos_train[, 1:3]
colnames(datos_train)
plot(svm_model, data = datos_train[, 1:3], formula = y ~  X1 + X2)
best

svm_cv$performances
summary(svm_model)
svm_model$coefs
svm_model$SV[, 1:3]
svm_model$SV[,1:4]
svm_cv$coefs-matrix

best$decision.values
