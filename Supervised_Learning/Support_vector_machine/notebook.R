rm(list=ls())


#----------------- SVM lineal ---------------------# 


#----------------- Pregunta 1 ---------------------# 

set.seed(10111)
x = matrix(rnorm(NULL), nrow=NULL, ncol=NULL)
y = rep(c(-1, 1), c(10,10))
x[NULL,] = x[NULL,] + 1
plot(NULL,NULL, col = y + 3, pch = 19)



#----------------- Pregunta 2 ---------------------# 

dat = data.frame(NULL, y = as.factor(NULL))
svmfit = svm(NULL, data = NULL, kernel = "linear", cost = 10, scale = FALSE)
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




ygrid = predict(NULL, NULL)
plot(NULL, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(NULL, pch = 5, cex = 2)


#----------------- Pregunta 4 ---------------------# 


beta = drop(NULL%*%NULL)
#svm$rho es el intercept (del plano) negativo (ver ayuda), por tanto le cambiamos el signo
beta0 = -svmfit$rho


#----------------- Pregunta 5 ---------------------# 


plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(NULL, col = y + 3, pch = 19) #realice el plot de los datos "x"
points(NULL, pch = 5, cex = 2) #realice el plot de los vectores soporte
#En la primera coordenada coloque el intercept de la recta, en la segunda la pendiente de esa recta
abline(NULL, NULL)
abline(NULL, NULL)
abline(NULL, NULL)



#----------------- SVM no lineal ---------------------# 




#----------------- Pregunta 1 ---------------------#  

rm(list=ls())


data(iris)
n <- nrow(iris)  # Number of observations
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_iris <- iris[NULL,]   # Create training set
test_iris <- iris[-NULL,]   # Create test set


svmfit <- svm(NULL, data=NULL, 
              method="C-classification", kernel="radial", cost=10)


summary(svmfit)
svmfit$SV



#El parametro slice mantiene las dimensiones constantes para los valores indicados
#Esto es porque estamos graficando en dos dimensiones
plot(svmfit, train_iris, Petal.Width ~ Petal.Length,
     slice=list(Sepal.Width=mean(iris$Sepal.Width), Sepal.Length=mean(iris$Sepal.Length)))



# hacemos la prediccion y entendemos la matriz de confusion
prediction <- predict(NULL, NULL)
conf_matrix <- table(NULL, NULL)

