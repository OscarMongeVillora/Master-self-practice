rm(list=ls())


#----------------- SVM lineal ---------------------# 


#----------------- Pregunta 1 ---------------------# 

set.seed(10111)
x = matrix(rnorm(40), 20, 2)
y = rep(c(-1, 1), c(10, 10))
x[y == 1,] = x[y == 1,] + 1

plot(x, col = y + 3, pch = 19)

y

#----------------- Pregunta 2 ---------------------# 

dat = data.frame(x, y = as.factor(y))
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)


#----------------- Pregunta 3 ---------------------# 


plot(svmfit, dat)



make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}



xgrid = make.grid(x)
xgrid[1:10,]




ygrid = predict(svmfit, xgrid)
ygrid
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = 0.1)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 18, cex = 2)

c("red","blue")





#----------------- Pregunta 4 ---------------------# 


beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
#rho es el intercept (del plano) negativo (ver ayuda), por tanto le cambiamos el signo
beta0 = -svmfit$rho 


#----------------- Pregunta 5 ---------------------# 


plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
abline(-beta0 / beta[2], -beta[1] / beta[2])
abline((1-beta0) / beta[2], -beta[1] / beta[2], lty = 2)
abline(-(1+beta0) / beta[2], -beta[1] / beta[2], lty = 2)



#----------------- SVM no lineal ---------------------# 



#----------------- Pregunta 1 ---------------------#  

rm(list=ls())


data(iris)
n <- nrow(iris)  # Number of observations
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_iris <- iris[tindex,]   # Create training set
test_iris <- iris[-tindex,]   # Create test set


svmfit <- svm(Species~., data=train_iris, 
            method="C-classification", kernal="radial", cost=10)


summary(svmfit)
svmfit$SV



#slice mantiene las dimensiones constantes para los valores indicados
plot(svmfit, train_iris, Petal.Width ~ Petal.Length, slice=list(Sepal.Width=3, Sepal.Length=4))



# hacemos la prediccion y entendemos la matriz de confusion
prediction <- predict(svmfit, test_iris)
conf_matrix <- table(test_iris$Species, prediction)




