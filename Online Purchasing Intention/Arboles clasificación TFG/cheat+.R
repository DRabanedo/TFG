library(ranger)
n_trees_rf = 1000 #Numero de árboles a utilizar
min_nod_size_rf = 1 #Queremos un fullgrown tree, para reducir el sesgo
r_forest = ranger(
  formula = Revenue ~ Administrative + Administrative_Duration + Informational
  + Informational_Duration +  ProductRelated + ProductRelated_Duration
  + BounceRates + ExitRates + PageValues + SpecialDay + Month 
  + OperatingSystems + Browser + Region +  TrafficType + VisitorType
  + Weekend,
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
  splitrule = "gini",
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
  seed = 8594,
  dependent.variable.name = NULL, #Name of dependent variable, needed if no formula given
  classification = TRUE)

n_trees_rf_bucle = c(100,250,500,750,1000,2000,3000) #Numero de árboles a utilizar
mtry_rf_bucle = c(round(0.5*sqrt(ncol(datos)-2)), round(sqrt(ncol(datos)-2)), round(2*sqrt(ncol(datos)-2)), 6)
min_nod_size_rf = 1 #Queremos un fullgrown tree, para reducir el sesgo
tabla_comparacion = data.frame(Splits_por_nodo = rep(NA, length(n_trees_rf_bucle)*length(mtry_rf_bucle)), Numero_arboles = rep(NA,length(n_trees_rf_bucle)*length(mtry_rf_bucle)),
                               Porcentaje_de_aciertos = rep(NA,length(n_trees_rf_bucle)*length(mtry_rf_bucle)))
rf_tabla = matrix(rep(NA,length(n_trees_rf_bucle)*length(mtry_rf_bucle)), nrow = length(n_trees_rf_bucle))
k = 0
for (i in 1:length(n_trees_rf_bucle)) {
  for (j in 1:length(mtry_rf_bucle)) {
    n_trees_int = n_trees_rf_bucle[i]
    mtry_int = mtry_rf_bucle[j]
    r_forest_buc = ranger(
      formula = Revenue ~ Administrative + Administrative_Duration + Informational
      + Informational_Duration +  ProductRelated + ProductRelated_Duration
      + BounceRates + ExitRates + PageValues + SpecialDay + Month 
      + OperatingSystems + Browser + Region +  TrafficType + VisitorType
      + Weekend,
      data           = datos, 
      num.trees      = n_trees_int,
      importance     = "impurity_corrected",
      mtry           = mtry_int ,
      seed           = 8594,
      classification = TRUE)
    rf_tabla[i,j] = round((1-r_forest_buc$prediction.error)*100, digits=3)
    k = k + 1
    tabla_comparacion$Porcentaje_de_aciertos[k] = round((1-r_forest_buc$prediction.error)*100, digits= 3)
    tabla_comparacion$Splits_por_nodo[k] = mtry_rf_bucle[j]
    tabla_comparacion$Numero_arboles[k]  = n_trees_rf_bucle[i]
    
  }
}    

tabla_comparacion$Porcentaje_de_aciertos = as.factor(tabla_comparacion$Porcentaje_de_aciertos)
tabla_comparacion[which.max(tabla_comparacion$Porcentaje_de_aciertos),]$Porcentaje_de_aciertos

n_trees_op = tabla_comparacion[which.max(tabla_comparacion$Porcentaje_de_aciertos),]$Numero_arboles
mtry_op = tabla_comparacion[which.max(tabla_comparacion$Porcentaje_de_aciertos),]$Splits_por_nodo
r_forest_op = ranger(
  formula = Revenue ~ Administrative + Administrative_Duration + Informational
  + Informational_Duration +  ProductRelated + ProductRelated_Duration
  + BounceRates + ExitRates + PageValues + SpecialDay + Month 
  + OperatingSystems + Browser + Region +  TrafficType + VisitorType
  + Weekend,
  data           = datos, 
  num.trees      = n_trees_op,
  importance     = "impurity_corrected",
  mtry           = mtry_op,
  seed           = 8594,
  classification = TRUE)

importance_pvalues(r_forest_op,method = "janitza")

Usando las de baja importancia

r_forest_noimp = ranger(
  formula = Revenue ~ SpecialDay + OperatingSystems + Browser + Region +  TrafficType
  + Weekend,
  data           = datos, 
  num.trees      = n_trees_op,
  importance     = "impurity_corrected",
  mtry           = mtry_op,
  seed           = 8594,
  classification = TRUE)

Rendimiento
Porcentaje_aciertos_rf_cv = (1-r_forest_noimp$prediction.error)*100
paste0("El acierto en las muestras haciendo random forest es del ", Porcentaje_aciertos_rf_cv, "%")

Usando las de alta imortancia

r_forest_imp = ranger(
  formula = Revenue ~ Administrative + Administrative_Duration + Informational
  + Informational_Duration +  ProductRelated + ProductRelated_Duration
  + BounceRates + ExitRates + PageValues +  Month + VisitorType,
  data           = datos, 
  num.trees      = n_trees_op,
  importance     = "impurity_corrected",
  mtry           = mtry_op,
  seed           = 8594,
  classification = TRUE)

Rendimiento
Porcentaje_aciertos_rf_cv = (1-r_forest_imp$prediction.error)*100
paste0("El acierto en las muestras haciendo random forest es del ", Porcentaje_aciertos_rf_cv, "%")

Para ver si esto es real, veamos la comparacion entre los dos bosques con
diferentes splits y nmero de arboles











n_trees_rf_bucle_imp = c(500,1000,2000) #Numero de árboles a utilizar
mtry_rf_bucle_imp = c(round(0.5*sqrt(ncol(datos)-2)), round(sqrt(ncol(datos)-2)))
min_nod_size_rf_imp = 1 #Queremos un fullgrown tree, para reducir el sesgo

tabla_comparacion_imp = data.frame(Splits_por_nodo = rep(NA, length(n_trees_rf_bucle_imp)*length(mtry_rf_bucle_imp)), Numero_arboles = rep(NA,length(n_trees_rf_bucle_imp)*length(mtry_rf_bucle_imp)),
                               Porcentaje_de_aciertos = rep(NA,length(n_trees_rf_bucle_imp)*length(mtry_rf_bucle_imp)))
rf_tabla_imp = matrix(rep(NA,length(n_trees_rf_bucle_imp)*length(mtry_rf_bucle_imp)), nrow = length(n_trees_rf_bucle_imp))

tabla_comparacion_noimp = data.frame(Splits_por_nodo = rep(NA, length(n_trees_rf_bucle_imp)*length(mtry_rf_bucle_imp)), Numero_arboles = rep(NA,length(n_trees_rf_bucle_imp)*length(mtry_rf_bucle_imp)),
                                   Porcentaje_de_aciertos = rep(NA,length(n_trees_rf_bucle_imp)*length(mtry_rf_bucle_imp)))
rf_tabla_noimp = matrix(rep(NA,length(n_trees_rf_bucle_imp)*length(mtry_rf_bucle_imp)), nrow = length(n_trees_rf_bucle_imp))
k = 0

for (i in 1:length(n_trees_rf_bucle_imp)) {
  for (j in 1:length(mtry_rf_bucle_imp)) {
    n_trees_int_imp = n_trees_rf_bucle_imp[i]
    mtry_int_imp = mtry_rf_bucle_imp[j]
    r_forest_buc_imp = ranger(
      formula = Revenue ~ Administrative + Administrative_Duration + Informational
      + Informational_Duration +  ProductRelated + ProductRelated_Duration
      + BounceRates + ExitRates + PageValues +  Month + VisitorType,
      data           = datos, 
      num.trees      = n_trees_int_imp,
      importance     = "impurity_corrected",
      mtry           = mtry_int_imp ,
      seed           = 8594,
      classification = TRUE)
    r_forest_buc_noimp = ranger(
      formula = Revenue ~ SpecialDay + OperatingSystems + Browser + Region +  TrafficType
      + Weekend,
      data           = datos, 
      num.trees      = n_trees_int_imp,
      importance     = "impurity_corrected",
      mtry           = mtry_int_imp ,
      seed           = 8594,
      classification = TRUE)
    rf_tabla_imp[i,j] = round((1-r_forest_buc_imp$prediction.error)*100, digits=3)
    k = k + 1
    tabla_comparacion_imp$Porcentaje_de_aciertos[k] = round((1-r_forest_buc_imp$prediction.error)*100, digits= 3)
    tabla_comparacion_imp$Splits_por_nodo[k] = mtry_rf_bucle_imp[j]
    tabla_comparacion_imp$Numero_arboles[k]  = n_trees_rf_bucle_imp[i]
    rf_tabla_noimp[i,j] = round((1-r_forest_buc_noimp$prediction.error)*100, digits=3)
    tabla_comparacion_noimp$Porcentaje_de_aciertos[k] = round((1-r_forest_buc_noimp$prediction.error)*100, digits= 3)
    tabla_comparacion_noimp$Splits_por_nodo[k] = mtry_rf_bucle_imp[j]
    tabla_comparacion_noimp$Numero_arboles[k]  = n_trees_rf_bucle_imp[i]
    
  }
}    

tabla_comparacion_noimp
tabla_comparacion_imp
rf_tabla_imp

DIBUJITOS
library(ggplot2)
v2_rf = rf_tabla[,1][c(3,5,6)]
v4_rf = rf_tabla[,2][c(3,5,6)]

v2_rf_imp = rf_tabla_imp[,1]
v4_rf_imp = rf_tabla_imp[,2]

v2_rf_noimp = rf_tabla_noimp[,1]
v4_rf_noimp = rf_tabla_noimp[,2]

n_trees_rf_bucle_imp
datos_grafica_imp =  data.frame( "Sub_imp" = v2_rf_imp,
                             "Acc_imp" = v4_rf_imp,
                             "Sub_noimp" = v2_rf_noimp,
                             "Acc_noimp" = v4_rf_noimp,
                             "Numero_de_arboles" = n_trees_rf_bucle_imp,
                             "Sub" = v2_rf,
                             "Acc" = v4_rf)
datos_grafica_imp
ggplot(datos_grafica_imp) +
  geom_line(aes(x = Numero_de_arboles, y = Sub_imp, col = "2 var por split (Importantes)")) + 
  geom_line(aes(x = Numero_de_arboles, y = Acc_imp, col = "4 var por split (Importantes)")) +
  geom_line(aes(x = Numero_de_arboles, y = Sub_noimp, col = "2 var por split (No Importantes)")) +
  geom_line(aes(x = Numero_de_arboles, y = Acc_noimp, col = "4 var por split(No Importantes)")) + 
  geom_line(aes(x = Numero_de_arboles, y = Sub, col = "2 var por split (TOTALES)")) + 
  geom_line(aes(x = Numero_de_arboles, y = Acc, col = "4 var por split (TOTALES)")) +
  scale_color_discrete("Número de variables por Split") +
  labs(title='Comparación entre los tres diferentes bosques',
       x='Número de árboles',
       y='Probabilidad de acierto') 







n_trees_rf_bucle_imp2 = c(500,1000,2000) #Numero de árboles a utilizar
mtry_rf_bucle_imp2 = c(round(0.5*sqrt(ncol(datos)-2)), round(sqrt(ncol(datos)-2)),8,6)
min_nod_size_rf_imp2 = 1 #Queremos un fullgrown tree, para reducir el sesgo

tabla_comparacion_imp2 = data.frame(Splits_por_nodo = rep(NA, length(n_trees_rf_bucle_imp2)*length(mtry_rf_bucle_imp2)), Numero_arboles = rep(NA,length(n_trees_rf_bucle_imp2)*length(mtry_rf_bucle_imp2)),
                                   Porcentaje_de_aciertos = rep(NA,length(n_trees_rf_bucle_imp2)*length(mtry_rf_bucle_imp2)))
rf_tabla_imp2 = matrix(rep(NA,length(n_trees_rf_bucle_imp2)*length(mtry_rf_bucle_imp2)), nrow = length(n_trees_rf_bucle_imp2))

k = 0

for (i in 1:length(n_trees_rf_bucle_imp2)) {
  for (j in 1:length(mtry_rf_bucle_imp2)) {
    n_trees_int_imp2 = n_trees_rf_bucle_imp2[i]
    mtry_int_imp2 = mtry_rf_bucle_imp2[j]
    r_forest_buc_imp2 = ranger(
      formula = Revenue ~ Administrative + Administrative_Duration + Informational
      + Informational_Duration +  ProductRelated + ProductRelated_Duration
      + BounceRates + ExitRates + PageValues +  Month + VisitorType,
      data           = datos, 
      num.trees      = n_trees_int_imp2,
      importance     = "impurity_corrected",
      mtry           = mtry_int_imp2 ,
      seed           = 8594,
      classification = TRUE)
    
    rf_tabla_imp2[i,j] = round((1-r_forest_buc_imp2$prediction.error)*100, digits=3)
    k = k + 1
    tabla_comparacion_imp2$Porcentaje_de_aciertos[k] = round((1-r_forest_buc_imp2$prediction.error)*100, digits= 3)
    tabla_comparacion_imp2$Splits_por_nodo[k] = mtry_rf_bucle_imp2[j]
    tabla_comparacion_imp2$Numero_arboles[k]  = n_trees_rf_bucle_imp2[i]
    
  }
}    

v2_rf = rf_tabla[,1][c(3,5,6)]
v4_rf = rf_tabla[,2][c(3,5,6)]
v8_rf = rf_tabla[,3][c(3,5,6)]
v6_rf = rf_tabla[,4][c(3,5,6)]

v2_rf
v2_rf_imp2 = rf_tabla_imp2[,1]
v4_rf_imp2 = rf_tabla_imp2[,2]
v8_rf_imp2 = rf_tabla_imp2[,3]
v6_rf_imp2 = rf_tabla_imp2[,4]

datos_grafica_imp2 =  data.frame( "Sub" = v2_rf,
                             "Acc" = v4_rf,
                             "Sup" = v8_rf,
                             "Una" = v6_rf,
                             "Sub_imp2" = v2_rf_imp2,
                             "Acc_imp2" = v4_rf_imp2,
                             "Sup_imp2" = v8_rf_imp2,
                             "Una_imp2" = v6_rf_imp2,
                             "Numero_de_arboles" = n_trees_rf_bucle_imp2)
ggplot(datos_grafica_imp2) +
  geom_line(aes(x = Numero_de_arboles, y = Sub, col = "2 var por split")) + 
  geom_line(aes(x = Numero_de_arboles, y = Acc, col = "4 var por split")) +
  geom_line(aes(x = Numero_de_arboles, y = Una, col = "6 var por split")) +
  geom_line(aes(x = Numero_de_arboles, y = Sup, col = "8 var por split")) + 
  geom_line(aes(x = Numero_de_arboles, y = Sub_imp2, col = "2 var por split (Modelo reducido)")) + 
  geom_line(aes(x = Numero_de_arboles, y = Acc_imp2, col = "4 var por split (Modelo reducido)")) +
  geom_line(aes(x = Numero_de_arboles, y = Una_imp2, col = "6 var por split (Modelo reducido)")) +
  geom_line(aes(x = Numero_de_arboles, y = Sup_imp2, col = "8 var por split (Modelo reducido)")) + 
  scale_color_discrete("Número de variables por Split") +
  labs(title='Comparación entre modelo total y reducido',
       x='Número de árboles',
       y='Probabilidad de acierto')







