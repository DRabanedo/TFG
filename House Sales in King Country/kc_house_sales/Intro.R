datos <- read.table('kc_house_data.csv', header = TRUE, sep =',')
library(rpart)
library(rpart.plot)
library(ranger)
set.seed(1999)


#Hacemos el log de la variable respuesta para arreglar la asimetría
datos_sel = datos[c(3,4,5,6,7,8,9,10,11,12,13,14,15,17,20,21)]

datos_sel$log_price = log(datos$price) #Hacer el estudio de las dos

waterfront + condition + view + grade + zipcode

datos_sel$waterfront = factor(datos_sel$waterfront)
datos_sel$condition = factor(datos_sel$condition)
datos_sel$view = factor(datos_sel$view)
datos_sel$grade = factor(datos_sel$grade)
datos_sel$zipcode = factor(datos_sel$zipcode)
#factor water view cond grade zipcode 

id + date + price + bathrooms + sqft_living + sqft_lot + floors +
waterfront + view + condition + grade + sqft_above + sqft_basement +
yr_built + zipcode +  sqft_living15 + sqft_lot15

#Buscamos los datos atípicos a mano
aux = datos$sqft_living>10000
datos_sel = datos_sel[!aux,] #negación lógica


#Regresión en los datos
set.seed(732)
datos_reg <- rpart(log_price ~ bathrooms + sqft_living + sqft_lot + floors +
                    waterfront + view + condition + grade + sqft_above + sqft_basement +
                    yr_built + zipcode +  sqft_living15 + sqft_lot15, data=datos_sel, cp = 0.000000000000000005)
datos_reg$cptable[which.min(datos_reg$cptable[,4]),1]
datos_reg$cptable
summary(datos_reg)
#rsq.rpart(datos_reg)
#plotcp(datos_reg)
#rpart.plot(datos_reg)
one_se = datos_reg$cptable[which.min(cp_t[,4]),4] + datos_reg$cptable[which.min(cp_t[,4]),5]
one_se

#Todo va bien
for (i in 1:30) {
  set.seed((1999))
  datos_reg <- rpart(log_price ~ ., data=datos2, cp = 0.000005)
  one_se = datos_reg$cptable[which.min(cp_t[,4]),4] + datos_reg$cptable[which.min(cp_t[,4]),5]
  print(one_se)
}

a = datos_reg$cptable[,4] > one_se
cp_optimo_pos = min(which(a == FALSE))
cp_optimo_pos


cp_optimo_error = sqrt(datos_reg$cptable[which.min(datos_reg$cptable[,"xerror"]),"CP"]*datos_reg$cptable[which.min(datos_reg$cptable[,"xerror"])-1,"CP"])
cp_optimo = sqrt(datos_reg$cptable[cp_optimo_pos,"CP"]*datos_reg$cptable[cp_optimo_pos-1,"CP"])
cp_optimo
datos_reg$cptable[cp_optimo_pos,"CP"]

datos_cualit = datos[c(3,4,5,6,7,8,12,13,14,15,16,22,21)]


cp_optimo_error = sqrt(datos_reg$cptable[which.min(datos_reg$cptable[,"xerror"]),"CP"]*datos_reg$cptable[which.min(datos_reg$cptable[,"xerror"])-1,"CP"])
cp_optimo = sqrt(datos_reg$cptable[cp_optimo_pos,"CP"]*datos_reg$cptable[cp_optimo_pos-1,"CP"])
cp_optimo
datos_reg$cptable[cp_optimo_pos,"CP"]


#Residuos del modelo
plot(predict(datos_reg), jitter(resid(datos_reg)))
temp <- datos_reg$frame[datos_reg$frame$var == '<leaf>',]
axis(3, at = temp$yval, as.character(row.names(temp)))
mtext('leaf number', side = 3, line = 3)
abline(h = 0, lty = 2)


#Random forest de regresión
n_trees_rf = 1000 #Numero de árboles a utilizar
min_nod_size_rf = 1 #Queremos un fullgrown tree, para reducir el sesgo
r_forest = ranger(formula = log_price ~ .,
  data = datos, #Conjunto de datos
  num.trees = n_trees_rf,
  mtry = NULL, #Number of variables to possibly split at in each node, Default
  #is the (rounded down) square root of the number variables. 
  importance = "impurity_corrected",
  write.forest = TRUE, #Si guarda el árbol para después predecir con ese árbol
  probability = FALSE,
  min.node.size = min_nod_size_rf, #Queremos un fullgrown tree, para reducir el sesgo
  max.depth = NULL, #NULL nos da crecimiento ilimitado
  replace = TRUE, #La muestra la hace con remplazamiento
  case.weights = NULL, #Para oversamplear ciertos valores
  class.weights = NULL, #Pesos para el outcome
  splitrule = "variance",
  split.select.weights = NULL,
  always.split.variables = NULL, #Para meter esas variables siempre en el
  #split a mayores de las que están en mtry
  regularization.factor = 1,
  keep.inbag = FALSE,
  inbag = NULL, #Manually set observations per tree. List of size num.trees, 
  #containing inbag counts for each observation. Can be used for stratified sampling.
  oob.error = TRUE,
  num.threads = NULL,
  save.memory = FALSE, #Memory saving split mode
  verbose = TRUE, #	Show computation status and estimated runtime
  seed = 1999,
  dependent.variable.name = NULL) #Name of dependent variable, needed if no formula given
summary(r_forest)




## Primero elegimos el número de capas de cross-validation
fold = 10
## Observaciones
n = 21610
## Tamaño de la muestra test
n_test = n/fold
## Tamaño de la muestra trainning
n_tr = n - n_test 
vn = 1:n
## Hacemos una tabla para almacenar los datos
v_tabla_optimo = matrix(rep(NA,fold*4), nrow = fold)
#cp óptimo
cp_optimo = sqrt(datos_reg$cptable[cp_optimo_pos,"CP"]*datos_reg$cptable[cp_optimo_pos-1,"CP"])

#================================================================================
one_se = datos_reg$cptable[which.min(datos_reg$cptable [,4]),4] + datos_reg$cptable[which.min(datos_reg$cptable [,4]),5]
a = datos_reg$cptable[,4] > one_se
cp_optimo_pos = min(which(a == FALSE))
cp_se = sqrt(datos_reg$cptable[cp_optimo_pos,"CP"]*datos_reg$cptable[cp_optimo_pos-1,"CP"])
cp_se





## Primero elegimos el número de capas de cross-validation
  fold = 10
## Observaciones
n = 21610
## Tamanño de la muestra test
n_test = n/fold
## Tamaño de la muestra trainning
n_tr = n - n_test 
vn = 1:n
## Hacemos una tabla para almacenar los datos
v_tabla_optimo = matrix(rep(NA,fold*4), nrow = fold)
  cv_error = 0
for (i in 1:fold ) {
  i_test = 1:n_test + (i - 1)*n_test
  i_tr = vn[-i_test]
  datos_tr = datos_sel[i_tr,]
  datos_test = datos_sel[i_test,]
  tcar_pruned_eficacia  <- rpart(log_price ~ bathrooms + sqft_living +
                           sqft_lot + floors + sqft_above + sqft_basement + yr_built
                         + sqft_living15 + waterfront + condition + view + grade + zipcode + sqft_lot15, data=datos_sel ,control =
                           rpart.control(xval = 10, minbucket = 1, cp = cp_se ))
  tcar_pruned_pred = predict(tcar_pruned_eficacia, datos_test, type = "vector")
  tcar_pruned_ver =  datos_test$log_price
  for (k in 1:length( tcar_pruned_eficacia)) {
    cv_error = cv_error + (tcar_pruned_pred[k] - tcar_pruned_ver[k])^2
  }
}
print(tcar_pruned_pred)
i_test
print(tcar_pruned_ver)
datos_test$log_price[1:9]
cv_error

# ==============================================================================
#Problema de regresión lineal múltiple: COMPARATIVA
mult_reg = lm(log_price ~ bathrooms + sqft_living +sqft_lot + floors + sqft_above
              + yr_built + sqft_living15 + sqft_lot15, data=datos_sel)
summary(mult_reg)
sum((mult_reg$residuals)^2)

mult_reg_nolog = lm(price ~ bathrooms + sqft_living +sqft_lot + floors + sqft_above
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
      formula = price ~ bathrooms + sqft_living +sqft_lot + floors + sqft_above + sqft_basement +yr_built + sqft_living15 + waterfront + condition + view + grade + zipcode + sqft_lot15,
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
