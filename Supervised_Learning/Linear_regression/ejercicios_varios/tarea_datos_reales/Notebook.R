rm(list=ls())

#Carga los datos de inmuebles que se encuentran en la carpeta data_in
load("./data_in/inmuebles.Rdata")

#Visualiza previamente los datos
View(NULL)

#El objetivo es realizar un modelo de regresion para predecir el precio del sur de Madrid ("Fuenlabrada","Legan\u00e9s","Getafe","Alcorc\u00f3n")

#Realiza el subsetting de los datos
data_scrap<-data_scrap[which(data_scrap$level5 %in% c(NULL)),]



# Calcula la media y varianza muestral de las variables precio, habitaciones, superficie y banos
sapply(data_scrap[,c(NULL)],function(x){round(mean(x,na.rm=T),2)})

#Realice el calculo por localidad
aggregate(data_scrap[,c(NULL)], by = list(localidad=data_scrap$level5), FUN = function(x){round(mean(x,na.rm=T),2)})


#Cual es la vivienda mas cara de cada localidad?
aggregate(data_scrap[,c(NULL)], by = list(localidad=data_scrap$level5), FUN = function(x){round(max(x,na.rm=T),2)})



#Grafica un mapa con los data_scrap que tienes


pal <- colorFactor(
  palette = 'Dark2',
  domain = data_scrap$level5
)



my_map <- leaflet() %>%
  addTiles() %>%  # use the default base map which is OpenStreetMap tiles
  addCircleMarkers(lng=data_scrap$longitude,lat=data_scrap$latitude, radius = 5,  popup=factor(data_scrap$id_realEstates) ,color = pal(data_scrap$level5))
my_map


#que observas? soluciona el problema (si lo hay)

data_scrap<-data_scrap[which(NULL),]

my_map <- leaflet() %>%
  addTiles() %>%  # use the default base map which is OpenStreetMap tiles
  addCircleMarkers(lng=data_scrap$longitude,lat=data_scrap$latitude, radius = 5,  popup=factor(data_scrap$id_realEstates) ,color = pal(data_scrap$level5))
my_map



#que puedes decir de las variables de precio y superficie en metros cuadrados?
cor(NULL)


#soluciona el problema (si lo hay)
data_scrap<-data_scrap[which(NULL),]

cor(NULL,NULL)




#Aplica una regresion simple para precios y superficies
lm_model<-lm(NULL ~ NULL,data=data_scrap)

#Grafica la linea de regresion y el scatterplot
plot(data_scrap$NULL,data_scrap$NULL)

abline(lm_model,col="red",lwd=3)

summary(NULL)



#Hay algo que te llame la atencion en el plot anterior?

data_scrap<-data_scrap[which(NULL),]

#Realiza una normalizacion de los datos de precio y ajusta nuevamente la regresion
price_norm<-(NULL-mean(NULL))/sd(NULL)

lm_model<-lm(price_norm ~ NULL)
plot(NULL,price_norm)
abline(lm_model,col="red",lwd=3)
summary(lm_model)


#Ahora eres libre de realizar cualquier analisis. Hint: Empieza por outliers, NA's.

