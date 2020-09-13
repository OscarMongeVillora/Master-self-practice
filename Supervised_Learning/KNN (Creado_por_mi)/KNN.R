rm(list=ls())
df0 <- iris
is.data.frame(iris)
train <- sample(1:nrow(df0), round(0.9 * nrow(df0)))

# Funcion de normalizacion

norml <- function(vec){(vec - min(vec)) / (max(vec) - min(vec))}
norml(c(1,2,3))

# Normalizamos nuestro data frame
#df <- data.frame(sapply(df[,1:4], norml), df$Species)
df <- data.frame(sapply(df0[,1:4], norml))
df
Y <- df0[,5] 
  
# TRAIN Y TEST

df.train <- df[train,]
df.test <- df[-train,]

library(class)

# Fit knn, la función knn te da directamente la predicción en test

pred <- knn(df.train, df.test, Y[train], k=13)  

tab <- table(pred, Y[-train])
tab
accuracy <- function(x){sum(diag(x))/(sum(x)) * 100}
accuracy(tab)

####Paréntesis de otro ejemplo
library(ggplot2)
dia <- diamonds
class(dia$cut)  #cuidado con ordered factors! convertirlos a factor
#si los usamos de variable respuesta

#K-fold validation en el ejemplo de iris de antes

pred_cv_knn <- knn.cv(df.train, Y[train])
pred_cv_knn # Me está devolviendo las predicciones a los sample de train
# Promediado el valor predecido con todas las particiones

tab <- table(pred_cv_knn, Y[train])
tab
accuracy <- function(x){sum(diag(x))/(sum(x)) * 100}
accuracy(tab)
length(Y[train])

# knn.cv no me devuelve el valor k óptimo.
acc_vec <- c()
for(k in 1:20){
  pred_cv_knn <- knn.cv(df.train, Y[train], k=k)
  tab <- table(pred_cv_knn, Y[train])
  acc_vec <- append(acc_vec, accuracy(tab))
  
}

plot(seq(1:20), acc_vec, type = "b", xlab = "K-neighbours")
# Me gusta k= 12


# Ahora miro el resultado para k = 12 para los datos de test

pred <- knn(df.train, df.test, Y[train], k=12)
tab <- table(pred, Y[-train])
accuracy(tab)
