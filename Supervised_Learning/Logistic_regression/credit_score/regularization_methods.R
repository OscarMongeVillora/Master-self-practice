library(ISLR)
datos <- College
sapply(datos,function(x) sum(is.na(x)))

library(glmnet)     

datos <- datos[sample(1:nrow(datos)),]
datos_train <- datos[1: round((2/3)*nrow(datos)),]
datos_test <- datos[setdiff(rownames(datos), rownames(datos_train)), ]

train <- sample(x = 1:nrow(datos), size = round(nrow(datos) * (2/3)))
datos.train <- datos[train, ]
datos.test <- datos[-train, ]

datos.train.mat <- model.matrix(Apps ~ ., data = datos_train)
datos.test.mat <- model.matrix(Apps ~ ., data = datos_test)

head(datos.train.mat)
head(datos_train)

lambda = 10 ^ seq(from = 4, to = -2, length = 100)

rnorm(2)


n <- 1000  # Number of observations
p <- 5000  # Number of predictors included in model
real_p <- 15
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- apply(x[,1:real_p], 1, sum) + rnorm(n) #Generacion del vector y
                                         
aa <- matrix(c(1:15), 3, 5)
aa
apply(aa[,1:3], 1, sum)


a <- c(8, 4, 5)
index <- c(2, 3)
a[-index]
a[index]
assign("a",c(1, 2, 3))
