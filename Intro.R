#Cargamos las librerías y los datos
library(rpart)
library(rpart.plot)
library(ranger)
library(ggplot2)
datos <- read.table('kc_house_data.csv', header = TRUE, sep =',')

#Elegimos una semilla
semilla = 732
set.seed(semilla)


#Seleccionamos las variables relevantes que vamos a utilizar en la regresión
datos_sel = datos[c(3,4,5,6,7,8,9,10,11,12,13,14,15,17,20,21)]

#Convertimos en factor las variables cualitativas
datos_sel$waterfront = factor(datos_sel$waterfront)
datos_sel$condition = factor(datos_sel$condition)
datos_sel$view = factor(datos_sel$view)
datos_sel$grade = factor(datos_sel$grade)
datos_sel$zipcode = factor(datos_sel$zipcode)

#Hacemos el log de la variable respuesta para arreglar la asimetría
datos_sel$log_price = log(datos$price) 

#Variables utilizadas para realizar la regresión
id + date + price + bathrooms + sqft_living + sqft_lot + floors +
  waterfront + view + condition + grade + sqft_above + sqft_basement +
  yr_built + zipcode +  sqft_living15 + sqft_lot15

#Buscamos los datos atípicos a mano
aux = datos$sqft_living>10000
datos_sel = datos_sel[!aux,]

################################################################################

#Regresión en los datos
set.seed(semilla)
datos_reg <- rpart(log_price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors +
                    waterfront + view + condition + grade + sqft_above + sqft_basement +
                    yr_built + zipcode +  sqft_living15 + sqft_lot15, data=datos_sel, control =
                     rpart.control(xval = 10, minbucket = 1, cp = 0), method = "anova")
cp_t = datos_reg$cptable
cp_t[1:30]

plotcp(datos_reg)

################################################################################

## MÉTODO DE VALIDACIÓN CRUZADA
#Parámetro óptimo de validación cruzada
cp_cv = sqrt(datos_reg$cptable[which.min(datos_reg$cptable[,4]),1]*datos_reg$cptable[which.min(datos_reg$cptable[,4])-1,1])
cp_cv

datos_reg_cv <- rpart(log_price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors +
                        waterfront + view + condition + grade + sqft_above +
                        sqft_basement + yr_built + zipcode +  sqft_living15 +
                        sqft_lot15, data=datos_sel, cp = cp_cv, method = "anova")


#Cálculo del error de validación cruzada

fold = 10            ## N(*\'{u}*)mero de capas para la validaci(*\'{o}*) cruzada
n = nrow(datos_sel)  ## N(*\'{u}*)mero de observaciones
n_test = n/fold      ## N(*\'{u}*)mero de observaciones en la muestra de prueba 
n_tr = n - n_test    ## N(*\'{u}*)mero de observaciones en la muestra de entrenamiento
vn = 1:n             ## Vector con el n(*\'{u}*)mero total de observaciones

cv_error_vc = 0
for (i in 1:fold ) {
  set.seed(semilla)
  i_test = 1:n_test + (i - 1)*n_test
  i_tr = vn[-i_test]
  datos_tr = datos_sel[i_tr,]
  datos_test = datos_sel[i_test,]
  tcar_pruned_eficacia  <- rpart(log_price ~ bedrooms + bathrooms + sqft_living +
                                   sqft_lot + floors + sqft_above + sqft_basement + yr_built
                                 + sqft_living15 + waterfront + condition + view + grade + zipcode + sqft_lot15, data=datos_tr ,control =
                                   rpart.control(xval = 10, minbucket = 1, cp = cp_cv ), method = "anova")
  tcar_pruned_pred = predict(tcar_pruned_eficacia, datos_test, type = "vector")
  tcar_pruned_ver =  datos_test$log_price
  for (k in 1:length(tcar_pruned_ver)) {
    cv_error_vc = cv_error_vc + (tcar_pruned_pred[k] - tcar_pruned_ver[k])^2
  }
}

#datos_reg_cv$csplit
paste0("El error de validación cruzada para este árbol es de ", round(cv_error_vc/n, 5))

#paste0("El error estándar residual para este árbol es del ", round(sqrt(cv_error_vc/(n-15-2)), 5),
      # ", la media de cuadrados residual para este árbol es " ,round(cv_error_vc,5), " y el error cuadrático medio es " , round(cv_error_vc/n,5))


################################################################################

## MÉTODO DE LA REGLA 1-SE

one_se = datos_reg$cptable[which.min(cp_t[,4]),4] + datos_reg$cptable[which.min(cp_t[,4]),5]
a = datos_reg$cptable[,4] > one_se
cp_optimo_pos = min(which(a == FALSE))
cp_optimo_error = sqrt(datos_reg$cptable[which.min(datos_reg$cptable[,"xerror"]),"CP"]*datos_reg$cptable[which.min(datos_reg$cptable[,"xerror"])-1,"CP"])
cp_se = sqrt(datos_reg$cptable[cp_optimo_pos,"CP"]*datos_reg$cptable[cp_optimo_pos-1,"CP"])
cp_se
set.seed(semilla)
datos_reg_se <- rpart(log_price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors +
                        waterfront + view + condition + grade + sqft_above +
                        sqft_basement + yr_built + zipcode +  sqft_living15 +
                        sqft_lot15, data=datos_sel, cp = cp_se, method = "anova")



fold = 10            ## N(*\'{u}*)mero de capas para la validaci(*\'{o}*) cruzada
n = nrow(datos_sel)  ## N(*\'{u}*)mero de observaciones
n_test = n/fold      ## N(*\'{u}*)mero de observaciones en la muestra de prueba 
n_tr = n - n_test    ## N(*\'{u}*)mero de observaciones en la muestra de entrenamiento
vn = 1:n             ## Vector con el n(*\'{u}*)mero total de observaciones

cv_error_se = 0
for (i in 1:fold ) {
  i_test = 1:n_test + (i - 1)*n_test
  i_tr = vn[-i_test]
  datos_tr = datos_sel[i_tr,]
  datos_test = datos_sel[i_test,]
  tcar_pruned_eficacia  <- rpart(log_price ~ bedrooms + bathrooms + sqft_living +
                                   sqft_lot + floors + sqft_above + sqft_basement + yr_built
                                 + sqft_living15 + waterfront + condition + view + grade + zipcode + sqft_lot15, data=datos_tr ,control =
                                   rpart.control(xval = 10, minbucket = 1, cp = cp_se ))
  tcar_pruned_pred = predict(tcar_pruned_eficacia, datos_test, type = "vector")
  tcar_pruned_ver =  datos_test$log_price
  for (k in 1:length(tcar_pruned_ver)) {
    cv_error_se = cv_error_se + (tcar_pruned_pred[k] - tcar_pruned_ver[k])^2
  }
}

#datos_reg$cptable

paste0("El error de validación cruzada para este árbol es de ", round(cv_error_se/n, 5))
n
#paste0("El error estándar residual para este árbol es del ", round(sqrt(cv_error_se/(n-15-2)), 5),
       #", la media de cuadrados residual para este árbol es " ,round(cv_error_se,5), " y el error cuadrático medio es " , round(cv_error_se/n,5))


################################################################################

## Comparativa con una selección de árboles elegida entre todos los posibles

elecciones_grafica = (1:nrow(datos_reg$cptable))[20:700]
elecciones_grafica

nrow(datos_reg$cptable)
cp_todos_v = c(datos_reg$cptable[elecciones_grafica])
cp_todos_v

nodos_todos_v = elecciones_grafica
nodos_todos_v

par_comp = rep(NA, length(elecciones_grafica))
eficiencia = rep(NA, length(elecciones_grafica))
nodos = rep(NA, length(elecciones_grafica))
tabla_comparacion = data.frame(Parametro_complejidad = par_comp, Eficiencia = eficiencia, Nodos = nodos)

contador = 0
for (j in 1:length(elecciones_grafica)) {
  set.seed(732)
  fold = 10
  n = nrow(datos_sel)
  n_test = n/fold
  n_tr = n - n_test 
  vn = 1:n
  min_nod <- 1
  min_cp <- 0
  cv <- 10
  v_tabla = matrix(rep(NA,fold*4), nrow = fold)
  cp_todos = cp_todos_v[j]
  
  cv_error = 0
  for (i in 1:fold ) {
    i_test = 1:n_test + (i - 1)*n_test
    i_tr = vn[-i_test]
    datos_tr = datos_sel[i_tr,]
    datos_test = datos_sel[i_test,]
    tcar_pruned_eficacia  <- rpart(log_price ~ bedrooms + bathrooms + sqft_living +
                                     sqft_lot + floors + sqft_above + sqft_basement + yr_built
                                   + sqft_living15 + waterfront + condition + view + grade + zipcode + sqft_lot15, data=datos_tr ,control =
                                     rpart.control(xval = 10, minbucket = 1, cp = cp_todos))
    tcar_pruned_pred = predict(tcar_pruned_eficacia, datos_test, type = "vector")
    tcar_pruned_ver =  datos_test$log_price
    for (k in 1:length(tcar_pruned_ver)) {
      cv_error = cv_error + (tcar_pruned_pred[k] - tcar_pruned_ver[k])^2
    }
  }
  
  tabla_comparacion$Parametro_complejidad[j] = cp_todos
  tabla_comparacion$Eficiencia[j] = cv_error/n
  tabla_comparacion$Nodos[j] = nodos_todos_v[j]
  print(cv_error/n)
  contador = contador +1
  print(contador)
  
}
tabla_comparacion = tabla_comparacion[20:700,]
tabla_comparacion

#El que minimiza el error de validación cruzada
cp_cvmanual = tabla_comparacion[which.min(tabla_comparacion$Eficiencia),]$Parametro_complejidad
cv_error_cvmanual = tabla_comparacion[which.min(tabla_comparacion$Eficiencia),]$Eficiencia
paste0("El error de validación cruzada para el árbol de parámetro ", cp_cvmanual, " es de ", round(cv_error_cvmanual, 5))


#Gráfica comparativa de todos los valores
ggplot(tabla_comparacion) +
  geom_line(aes(x = Nodos, y = Eficiencia, col = "Evolución general")) +
  geom_line(aes(x = Nodos, y = rep(cv_error_vc/n, length(Nodos)), col= "Método de validación cruzada (rpart)")) +
  geom_line(aes(x = Nodos, y = rep(cv_error_se/n, length(Nodos)), col= "Método 1-SE")) +
  geom_line(aes(x = Nodos, y = rep(cv_error_cvmanual, length(Nodos)), col= "Método de validación cruzada (manual)")) +
  
  scale_color_discrete("Leyenda") +
  labs(title='Error de validación cruzada en función del número de nodos',
       x='Número de nodos',
       y='Error de validación cruzada') 


################################################################################


#Random forest de regresión
semilla = 732

n_trees_rf = 1000         #Numero de árboles a utilizar
min_nod_size_rf = 1       #Queremos un árbol saturado, para reducir el sesgo
r_forest = ranger(formula = log_price ~ .,
  data = datos_sel,      
  num.trees = n_trees_rf,
  importance = "impurity_corrected",
  min.node.size = min_nod_size_rf,
  splitrule = "variance",
  regularization.factor = 1,
  oob.error = TRUE,
  seed = semilla,
  dependent.variable.name = NULL)
summary(r_forest)
paste0("El error cuadrático medio de este bosque aleatorio de regresión  sobre el conjunto de datos es de ", round(r_forest$prediction.error,6))
r_forest$r.squared
paste0("El valor de R cuadrado de este bosque aleatorio de regresión sobre el conjunto de datos es de ", round(r_forest$r.squared,6))


##############################################################
#Problema de regresión lineal múltiple: COMPARATIVA

datos_sel_lm = datos_sel[,c(1,2,3,4,5,6,11,12,13,15,16,17)]
mult_reg = lm(log_price ~ bedrooms + bathrooms + sqft_living +sqft_lot + floors + sqft_above  + sqft_basement
              + yr_built + sqft_living15 + sqft_lot15, data=datos_sel_lm)

summary(mult_reg)
sum((mult_reg$residuals)^2)/21600
0.3432^2

## Primero elegimos el número de capas de cross-validation
fold = 10
## Observaciones
n = 21610
## Tamanño de la muestra test
n_test = n/fold
## Tamaño de la muestra trainning
n_tr = n - n_test 
vn = 1:n

cv_error = 0
for (i in 1:fold ) {
  i_test = 1:n_test + (i - 1)*n_test
  i_tr = vn[-i_test]
  datos_tr = datos_sel[i_tr,]
  datos_test = datos_sel[i_test,]
  tcar_pruned_eficacia  <- lm(log_price ~ bedrooms + bathrooms + sqft_living +sqft_lot + floors + sqft_above  + sqft_basement
                              + yr_built + sqft_living15 + sqft_lot15, data=datos_tr)
  tcar_pruned_pred = predict.lm(tcar_pruned_eficacia, datos_test)
  tcar_pruned_ver =  datos_test$log_price
  for (k in 1:length(tcar_pruned_ver)) {
    cv_error = cv_error + (tcar_pruned_pred[k] - tcar_pruned_ver[k])^2
  }
}
cv_error/21600


mult_reg_nolog = lm(price ~ bedrooms + bathrooms + sqft_living +sqft_lot + floors + sqft_above
                    + yr_built + sqft_living15 + sqft_lot15, data=datos_sel)

sum((mult_reg_nolog$residuals)^2)









n_trees_rf_bucle = c(500,1000,1500) #Numero de árboles a utilizar
mtry_rf_bucle = c(round(0.5*sqrt(14)), round(sqrt(14)), round(2*sqrt(14)))
min_nod_size_rf = 1 #Queremos un fullgrown tree, para reducir el sesgo
tabla_comparacion2 = data.frame(Splits_por_nodo = rep(NA, length(n_trees_rf_bucle)*length(mtry_rf_bucle)), Numero_arboles = rep(NA,length(n_trees_rf_bucle)*length(mtry_rf_bucle)),
                                Mean_squared = rep(NA,length(n_trees_rf_bucle)*length(mtry_rf_bucle)))
rf_tabla2 = matrix(rep(NA,length(n_trees_rf_bucle)*length(mtry_rf_bucle)), nrow = length(n_trees_rf_bucle))
k = 0
for (i in 1:length(n_trees_rf_bucle)) {
  for (j in 1:length(mtry_rf_bucle)) {
    n_trees_int = n_trees_rf_bucle[i]
    mtry_int = mtry_rf_bucle[j]
    r_forest_buc = ranger(
      formula = price ~ bedrooms + bathrooms + sqft_living +sqft_lot + floors + sqft_above + sqft_basement +yr_built + sqft_living15 + waterfront + condition + view + grade + zipcode + sqft_lot15,
      data           = datos_sel,
      num.trees      = n_trees_int,
      importance     = "impurity_corrected",
      mtry           = mtry_int ,
      seed           = 732,
      splitrule      = "variance")
    rf_tabla2[i,j] = r_forest_buc$prediction.error
    k = k + 1
    tabla_comparacion2$Mean_squared[k] = r_forest_buc$prediction.error
    tabla_comparacion2$Splits_por_nodo[k] = mtry_rf_bucle[j]
    tabla_comparacion2$Numero_arboles[k]  = n_trees_rf_bucle[i]
    
  }
}    

tabla_comparacion2$Mean_squared = as.factor(tabla_comparacion$Mean_squared)
tabla_comparacion2

rf_tabla2
Valor_optimo2 = rf_tabla2[which.max(rf_tabla2)]
paste0("El acierto en las muestras haciendo random forest con los parámetros variados es del ", Valor_optimo2)
