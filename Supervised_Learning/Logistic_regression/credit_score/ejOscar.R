rm(list = ls())

cred <- as.data.frame(fread("./data_in/data_scoring.csv", header=TRUE, 
                            na.strings = c("")))

my_data_train = cred[1:400, ]

my_data_train

my_data_train$Y = ifelse(my_data_train$Y == "malo", 0, 1)


#Missing values
unique(my_data_train$Y)

sapply(my_data_train, function(x) sum(is.na(x)))
library(Amelia)
missmap(my_data_train, main = "Missing values vs observed")

##Models

m1 <- glm(Y ~ X1, family = binomial(link="logit"), data = my_data_train)
anova(m1, test="Chi")

m2 <- glm(Y~X2, family=binomial(link="logit"), data = my_data_train)
anova(m2, test="Chi")

m3 <- glm(Y~X3, family=binomial(link="logit"), data = my_data_train)
anova(m3, test="Chi")

m4 <- glm(Y~X4, family=binomial(link="logit"), data = my_data_train)
anova(m4, test="Chi")


#Seleccion de variables

m <- glm(Y ~ X1+X2+X3+X4+X5+X6+X7, binomial(link="logit"), 
         data = my_data_train)
summary(m)
anova(m, test= "Chi")

#Backward selection

m1 <- glm(Y ~ X1, binomial(link="logit"), data = my_data_train)
anova(m, m1, test= "Chi")

m2 <- glm(Y ~ X2, binomial(link="logit"), data = my_data_train)
anova(m, m2, test= "Chi")


# JUEVES

m <- glm(Y ~ X1+X2+X3+X4+X5+X6+X7, family = binomial(link = "logit"), 
         data = my_data_train)

m1 <- glm(Y ~ X2+X3+X4+X5+X6+X7, family = binomial(link = "logit"), 
               data = my_data_train)

m2 <- glm(Y ~ X1+X3+X4+X5+X6+X7, family = binomial(link = "logit"), 
          data = my_data_train)

m3 <- glm(Y ~ X1+X2+X4+X5+X6+X7, family = binomial(link = "logit"), 
          data = my_data_train)

m4 <- glm(Y ~ X1+X2+X3+X5+X6+X7, family = binomial(link = "logit"), 
          data = my_data_train)

m5 <- glm(Y ~ X1+X2+X3+X4+X6+X7, family = binomial(link = "logit"), 
          data = my_data_train)

m6 <- glm(Y ~ X1+X2+X3+X4+X5+X7, family = binomial(link = "logit"), 
          data = my_data_train)

m7 <- glm(Y ~ X1+X2+X3+X4+X5+X6, family = binomial(link = "logit"), 
          data = my_data_train)

anova(m, m7, test= "Chi")

m <- glm(Y ~ X1+X2+X3+X4+X5+X6, family = binomial(link = "logit"), 
         data = my_data_train)

m1 <- glm(Y ~ X2+X3+X4+X5+X6, family = binomial(link = "logit"), 
          data = my_data_train)

m2 <- glm(Y ~ X1+X3+X4+X5+X6, family = binomial(link = "logit"), 
          data = my_data_train)

m3 <- glm(Y ~ X1+X2+X4+X5+X6, family = binomial(link = "logit"), 
          data = my_data_train)

m4 <- glm(Y ~ X1+X2+X3+X5+X6, family = binomial(link = "logit"), 
          data = my_data_train)

m5 <- glm(Y ~ X1+X2+X3+X4+X6, family = binomial(link = "logit"), 
          data = my_data_train)

m6 <- glm(Y ~ X1+X2+X3+X4+X5, family = binomial(link = "logit"), 
          data = my_data_train)

anova(m, m5, test= "Chi")

m <- glm(Y ~ X1+X2+X3+X4+X6, family = binomial(link = "logit"), 
         data = my_data_train)

m1 <- glm(Y ~ X2+X3+X4+X6, family = binomial(link = "logit"), 
          data = my_data_train)

m2 <- glm(Y ~ X1+X3+X4+X6, family = binomial(link = "logit"), 
          data = my_data_train)

m3 <- glm(Y ~ X1+X2+X4+X6, family = binomial(link = "logit"), 
          data = my_data_train)

m4 <- glm(Y ~ X1+X2+X3+X6, family = binomial(link = "logit"), 
          data = my_data_train)

m6 <- glm(Y ~ X1+X2+X3+X4, family = binomial(link = "logit"), 
          data = my_data_train)

anova(m, m4, test= "Chi")

# Hemos quitado x5, x7 y x3

stepAIC(glm(Y ~ X1+X2+X3+X4+X5+X6+X7, family = binomial(link = "logit"), 
            data = my_data_train))


mdef <- glm(Y ~ X1+X2+X4+X6, family = binomial(link="logit"), 
            data = my_data_train)

exp(coefficients(mdef))


exp(confint(mdef, level=0.9))

my_pred <- predict(mdef, data = my_data_train, type = "response")
my_pred

cutoff <- function(cut, pred) {
  paid <- ifelse(pred < cut, 0, 1)
  return(paid)
}

pred_cut3 <- cutoff(0.3, my_pred)  
pred_cut5 <- cutoff(0.5, my_pred)   
pred_cut8 <- cutoff(0.8, my_pred)   
  
conf_matrix3 <- table(expected = pred_cut3, real = my_data_train$Y)  
conf_matrix3  

diag(conf_matrix3)

accuracy = sum(diag(conf_matrix3)) / sum(conf_matrix3)
accuracy

tpr = conf_matrix3[2, 2]/ (conf_matrix3[2, 2] + conf_matrix3[1, 2])
tpr

tnr = conf_matrix3[1, 1]/ (conf_matrix3[1, 1] + conf_matrix3[2, 1])
tnr

acc <- c()
tpr <- c()
tnr <- c()
eps <- c()
for(i in c(30:80)) {
  conf_matrix <- table(expec = cutoff(i/100, my_pred), real = my_data_train$Y)
  conf_matrix
  acc[i-29] <- sum(diag(conf_matrix))/sum(conf_matrix)
  tpr[i-29] <- conf_matrix[2,2]/(conf_matrix[2,2]+conf_matrix[1,2]) #tpr
  tnr[i-29] <- conf_matrix[1,1]/(conf_matrix[1,1]+conf_matrix[2,1])
  eps[i-29] = i
  }

df <- data.frame(eps, acc, tpr, tnr)

df2 <- melt(df, id_vars = i, measure.vars = c("acc", "tpr", "tnr"))

library(ggplot2)

ggplot(data = df2,
       aes(x = eps,
           y = value,
           colour = variable)) +
  geom_point()

df2[df2$eps == 65, ]

max(acc)
df[acc == max(acc),]


x <- c(1 : 30)
y <- c(20 : 40)
setdiff(x, y)
df = data.frame(x = c(1, 2, 3), y= c(4, 5, 6), z = c(7, 8, 9))
rownames(df)
nrow(df)
df <- df[sample(1:nrow(df)),]
df
nrow(df)
data_test <- df[1,]
data_train <- df[setdiff(rownames(df),rownames(data_test)),]
data_train
rownames(df)
rownames(data_test)
setdiff(rownames(df), rownames(data_test))
sample(c(1, 2, 3, 4, 5, 6), 3)
sample(1:3)
sample(1:10)


y <- c(0, 0, 1, 1, 0, 1, 0, 1, 0)
n_foldd = 3
my_folds<-KFold(y,n_foldd)
my_folds

df = data.frame(x = c(1, 2, 3 , 4), y= c(5, 6, 7, 8), z = c(9, 10, 11, 12))
df = df[sample(rownames(df)), ]
df
df_folds <- KFold(df, 2)
df_folds
df_folds[[2]]



df <- data.frame(a = c(1:10), b = c(21:30))
df
dff <- df[sample(rownames(df), nrow(df)),]
pp <- dff
pp
dfnew <- data.frame(a = c(1:10), b = c(21:30))
dfnew
dfnew <- dff + 1
dfnew
