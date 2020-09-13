#SVM lineal

 #1
rm(list = ls())
data <- matrix(rnorm(40), 20, 2)
dim(data)
y <- rep(c(1, -1), 10)

df <- data.frame(data, y)

df[y == 1, c("X1", "X2")] <- df[y == 1, c("X1", "X2")] + 1 
df$y <- as.factor(df$y)

library(ggplot2)
ggplot(data = df, aes(x = X1, y = X2, color = as.factor(y))) +
  geom_point(size = 5)+
  theme_bw()+
  labs(title = "Distribución de muestras etiquetadas")

#2
library(e1071)

train <- sample(1 : round(0.8 * dim(df)[1]))
data.train <- df[train,]
data.test <- df[-train,]

svm.fit <- svm(y ~ X1 + X2, data = data.train, kernel = "linear",
               cost = 5, scale = FALSE)

summary(svm.fit)  # 11 vectores soporte
svm.fit$SV
svm.fit$coefs

plot(svm.fit, data.train)


range1 <- range(df$X1)
range2 <- range(df$X2)
seq1 <- seq(range1[1], range1[2], length= 75)
seq2 <- seq(range2[1], range2[2], length= 75)

xs <- expand.grid(X1 = seq1, X2 = seq2)
predictions <- predict(object = svm.fit, newdata = xs)
coloured <- data.frame(xs, y = predictions)
head(coloured)
dim(coloured)
predictions
svm.fit$coefs
svm.fit$SV
betas <- drop(t(svm.fit$coefs) %*% svm.fit$SV)
beta0 <- svm.fit$rho

ggplot() +
  geom_point(data = coloured, aes(x = X1, y = X2, color = as.factor(y)), 
             size = 0.5) +
  geom_point(data = data.train, aes(x = X1, y = X2, color = as.factor(y)),
             size = 5) + 
  geom_point(data = data.train[svm.fit$index,], 
             aes(x = X1, y = X2, color = as.factor(y)), shape = 21,
             colour = "black", size = 5)+
  geom_abline(slope = - betas[1]/betas[2], intercept = beta0 / betas[2])+
  geom_abline(slope = - betas[1]/betas[2], 
              intercept = (beta0 +1) / betas[2], linetype = "dashed") +
geom_abline(slope = - betas[1]/betas[2], 
            intercept = (beta0 -1) / betas[2], linetype = "dashed")








