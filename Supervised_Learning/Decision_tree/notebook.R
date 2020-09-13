#Borramos los datos
rm(list=ls())

#Lectura de los datos
data(package="ISLR")
carseats<-NULL
names(NULL)

#---- Pregunta 1: Previsualizacion de los datos ------#
hist(NULL)

#Clase de cada variable 
sapply(NULL,NULL)


#---- Pregunta 2: Clssification decision tree ------#

#creamos una variable dicotomica para sales
High = ifelse(NULL<=8, NULL, NULL)
carseats = data.frame(NULL, NULL)

#creamos un decision tree para todo el dataset menos Sales
tree.carseats = tree(NULL~.-NULL, data=NULL)

#vemos los resultados del arbol
summary(NULL)

#Nuestro modelo tiene un 9% de clasificacion erronea.
#Con un deviance de .4575:
#desviacion residual total (SSE) /(n datos-n terminales) (regresion)
#-2/df*sum (sum (n_ik*log(p_ik))) (https://stats.stackexchange.com/questions/6581/what-is-deviance-specifically-in-cart-rpart)



#---- Pregunta 3: Analisis grafico ------#

plot(NULL)
text(NULL, pretty = 0) 


tree.carseats

# Vemos que el predictor mas fuerte es la ubicación en el estante.
# Los asientos con “buenos” estantes terminan claramente tienen una mayor probabilidad.
# El lado del arbol que tiene en cuenta las estanterias medianas / malas es considerablemente mas complejo,
# y si bien el precio sigue siendo importante, algunas otras variables como la publicidad también tienen poder
# de prediccion. En general, podríamos hacerlo un poco mejor podando el arbol.
# Usaremos la validación cruzada para determinar el numero ideal de nodos
# terminales en función del número de errores de clasificación en el conjunto de prueba.


#---- Pregunta 4: train vs test ------#


set.seed(3) #para que puedan obtener todos el mismo valor
train=sample(NULL, 250) #tomamos 250 valores del dataset

#Creamos el decision tree solo con los indices de train
tree.carseats = tree(NULL~.-NULL, NULL, subset=NULL)

#visualizamos
plot(tree.carseats)
text(tree.carseats, pretty=0)

# Se ve un poco distingo debido al conjunto de datos ligeramente diferente.
tree.pred = predict(NULL, carseats[-NULL,], type="class")

#matriz de confusion solo para datos de test
conf_matrix<-with(carseats[-NULL,], table(NULL, NULL))


#---- Pregunta 5: Cost-complexity pruning ------#



# Vamos a usar la validación cruzada para podar el arbol de manera optima.
# prune.misclass usa el error de clasificación errónea como la base parahacer la poda.
cv.carseats = cv.tree(NULL, FUN = prune.misclass,K = 10)
cv.carseats

#size: numero de nodos terminales
#dev: numero de valores mal clasificados (esto es porque 
#hemos cambiado la funcion de pruning)
#mas info: en http://mlwiki.org/index.php/Cost-Complexity_Pruning

plot(NULL)


#seleccionamos finalmente el número de nodos terminales
prune.carseats = prune.misclass(NULL, best = NULL)
plot(NULL)
text(NULL, pretty=0)


#clasificamos los valores de prediccion
tree.pred = predict(NULL, carseats[-NULL,], type="class")
with(carseats[-NULL,], table(NULL, NULL))





#---- Pregunta 6: TAREA ------#




