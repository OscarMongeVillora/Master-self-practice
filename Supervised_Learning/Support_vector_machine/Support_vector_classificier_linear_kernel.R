#rep(c(1, -1), each = 10)
library(ggplot2)
rm(list=ls())
set.seed(10111)
coordenadas <- matrix(rnorm(40), 20, 2)
colnames(coordenadas) <- c("X1", "X2")
y <- as.factor(c(rep(-1, 10), rep(1, 10)))  #Importante poner como factor para
# que svm() detecte el problema de clasificación

coordenadas[y == 1, ] = coordenadas[y == 1, ] + 1
df <- data.frame(coordenadas, y)

ggplot(data = df, aes(x = X1, y = X2, colour = y)) +
  geom_point(size = 6) +
  theme_bw() +
  theme(legend.position = "none")
  
library(e1071)

df$y <-as.factor(df$y)

modelo_svm <- svm(formula = y ~ X1 + X2, data = df, kernel = "linear",
                  cost = 10, scale = FALSE)

summary(modelo_svm)
plot(modelo_svm, df)


# Con ggplot2

rango_1 <- range(df$X1)
rango_2 <- range(df$X2)

new_x1 <- seq(rango_1[1], rango_1[2], length = 75)
new_x2 <- seq(rango_2[1], rango_2[2], length = 75)

nuevos_puntos <- expand.grid(X1 = new_x1, X2 = new_x2)
nuevos_puntos
is.data.frame(nuevos_puntos)

predicciones <- predict(modelo_svm, newdata = nuevos_puntos)
color_regiones <- data.frame(nuevos_puntos, y = predicciones )

is.factor(predicciones)  
#####
modelo_svm$coefs
modelo_svm$SV
#####
beta = drop(t(modelo_svm$coefs) %*% modelo_svm$SV)
beta0 = modelo_svm$rho


ggplot() +
  # Representación de las 2 regiones empleando los puntos y coloreándolos
  # según la clase predicha por el modelo
  geom_point(data = color_regiones, aes(x = X1, y = X2, color = as.factor(y)),
             size = 0.5) +
  # Se añaden las observaciones
  geom_point(data = df, aes(x = X1, y = X2, color = as.factor(y)),
             size = 6) +
  # Se identifican aquellas observaciones que son vectores soporte del modelo
  geom_point(data = df[modelo_svm$index, ],
             aes(x = X1, y = X2, color = as.factor(y)),
             shape = 21, colour = "black",
             size = 6) +
  # Se añaden las rectas del hiperplano y los márgenes
  geom_abline(intercept = beta0/beta[2], slope = -beta[1]/beta[2]) +
  geom_abline(intercept = (beta0 - 1)/beta[2], slope = -beta[1]/beta[2],
              linetype = "dashed") +    
  geom_abline(intercept = (beta0 + 1)/beta[2], slope = -beta[1]/beta[2],
              linetype = "dashed") +
  theme_bw() +
  theme(legend.position = "none")

# tune() cross-validation(10 folds)

svm_cv <- tune("svm", y ~ X1 + X2, data = df, 
               kernel = "linear", 
               range = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 20, 50, 100,
                                     150, 200)))
summary(svm_cv)
model <- svm_cv$best.model                      
model$coefs
model$rho
model$coefs

#####

mean(c(TRUE, FALSE, TRUE))

