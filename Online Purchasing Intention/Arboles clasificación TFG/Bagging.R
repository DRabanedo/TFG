#Script de Intención de compra//Código importante

##Primero asignamos la variable datos a nuestra base de datos, para poder asi estudiarla
library(rpart)
library(rpart.plot)
library(tidyverse)
datos <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/00468/online_shoppers_intention.csv', header = TRUE, sep =',')

##Vamos a crear una nueva columna, asignando a los datos de la variable Revenue
##el valor Si o No (si se produce la compra o no), si dicha variable vale TRUE ó 
##FALSE, respectivamente. Además lo creamos de forma que los datos sean de tipo factor,
##ya quevamos a elaborar un árbol de clasificación a partir del paquete rpart.
compra <- factor(datos$Revenue , levels = c(FALSE,TRUE), labels = c("NO", "SI"))
datos$y <- compra

##Reasignamos los valores que son cualitativos a datos tipo factor
datos$OperatingSystems = factor(datos$OperatingSystems)
datos$Browser = factor(datos$Browser)
datos$Region = factor(datos$Region)
datos$TrafficType = factor(datos$TrafficType)
datos$VisitorType = factor(datos$VisitorType)
datos$Weekend = factor(datos$Weekend) 
datos$Month = factor(datos$Month, labels = c('Aug', 'Dec', 'Feb', 'Jul', 'June', 'Mar', 'May', 'Nov', 'Oct', 'Sep'))
datos$Revenue = factor(datos$Revenue)

n_bootstrap = 10
min_nod <- 10
min_cp <- 0.01

cv <- 10

observaciones_tr = datos[1:10000,]
observaciones_test = datos[10001:12330,]
n_observaciones_tr = length(observaciones_tr[,1])
n_observaciones_test = length(observaciones_test[,1])
tabla_predicciones = matrix(rep(NA,n_bootstrap*n_observaciones_test), nrow = n_bootstrap)

for (i in 1:n_bootstrap){
  vec_obs_aleatorio = sample(1:n_observaciones_tr, n_observaciones_tr, replace = TRUE)
  datos_bootstrap = datos[vec_obs_aleatorio,]
  tcar_total_bootstrap <- rpart(y ~ Administrative + Administrative_Duration + Informational
                                 + Informational_Duration +  ProductRelated + ProductRelated_Duration
                                 + BounceRates + ExitRates + PageValues + SpecialDay + Month 
                                 + OperatingSystems + Browser + Region +  TrafficType + VisitorType
                                 + Weekend, data=datos_bootstrap, method = 'class',
                                 control = rpart.control(xval = cv, minbucket = min_nod, cp = min_cp))
  rpart.plot(tcar_total_bootstrap)
  tabla_predicciones[i,] = predict(tcar_total_bootstrap, observaciones_test, t="class")
  
}

##############################################################################################
#Esto es para ver que todo va bien
vector_bagging_prediccion = rep(NA,n_bootstrap)
for (j in 1:n_bootstrap){
  vector_bagging_prediccion[j] =  sum(tabla_predicciones[j,] == as.integer(observaciones_test$Revenue))
  
}
vector_bagging_prediccion
##############################################################################################

vector_predicciones=rep(NA,n_observaciones_test)
for (j in 1:n_observaciones_test){
  vector_bagging_prediccion[j] =  round(sum(tabla_predicciones[,j])/n_bootstrap)
}
vector_bagging_prediccion  
Aciertos_bagging = sum(vector_bagging_prediccion==as.integer(observaciones_test$Revenue))
Aciertos_bagging
Porcentaje_aciertos = (Aciertos_bagging/n_observaciones_test)  
Porcentaje_aciertos  

###############################################################################################
library(adabag)
tcar_bagging = bagging.cv(y ~ Administrative + Administrative_Duration + Informational
        + Informational_Duration +  ProductRelated + ProductRelated_Duration
        + BounceRates + ExitRates + PageValues + SpecialDay + Month 
        + OperatingSystems + Browser + Region +  TrafficType + VisitorType
        + Weekend, datos, v=10, mfinal = 30)
tcar_bagging
Porcentaje_aciertos_bagging_cv = (1-tcar_bagging$error)*100
Porcentaje_aciertos_bagging_cv
prob
samples
importance
terms
call
class
votes
trees
###############################################################################################

n_bootstrap_OOB = 10
min_nod <- 10
min_cp <- 0.01
cv <- 10


observaciones_OOB = datos
n_observaciones_OOB = length(observaciones_OOB[,1])
tabla_predicciones = matrix(rep(NA,n_bootstrap*n_observaciones_OOB), nrow = n_bootstrap)

for (i in 1:n_bootstrap){
  vec_obs_aleatorio = sample(1:n_observaciones_OOB, n_observaciones_OOB, replace = TRUE)
  datos_bootstrap = datos[vec_obs_aleatorio,]
  datos_bootstrap_OOB = datos[-vec_obs_aleatorio,]
  tcar_total_bootstrap <- rpart(y ~ Administrative + Administrative_Duration + Informational
                                + Informational_Duration +  ProductRelated + ProductRelated_Duration
                                + BounceRates + ExitRates + PageValues + SpecialDay + Month 
                                + OperatingSystems + Browser + Region +  TrafficType + VisitorType
                                + Weekend, data=datos_bootstrap, method = 'class',
                                control = rpart.control(xval = cv, minbucket = min_nod, cp = min_cp))
  rpart.plot(tcar_total_bootstrap)
  predict(tcar_total_bootstrap, datos_bootstrap_OOB, t="class")
  #DIFICIL
}