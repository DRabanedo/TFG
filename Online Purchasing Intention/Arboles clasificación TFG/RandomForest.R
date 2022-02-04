# Tratamiento de datos
# ==============================================================================
library(ISLR)
library(dplyr)
library(tidyr)
library(skimr)

# Gráficos
# ==============================================================================
library(ggplot2)
library(ggpubr)

# Preprocesado y modelado
# ==============================================================================
library(tidymodels)
library(ranger)
library(doParallel)

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
################################################################################
r_forest = ranger(
    formula = Revenue ~ Administrative + Administrative_Duration + Informational
                  + Informational_Duration +  ProductRelated + ProductRelated_Duration
                  + BounceRates + ExitRates + PageValues + SpecialDay + Month 
                  + OperatingSystems + Browser + Region +  TrafficType + VisitorType
                  + Weekend,
    data = datos, 
    num.trees = 500,
    mtry = NULL, #Number of variables to possibly split at in each node, Default
    #is the (rounded down) square root of the number variables. 
    importance = "impurity_corrected",
    write.forest = TRUE, #Si guarda el árbol para después predecir con ese árbol
    probability = FALSE,
    min.node.size = 1, #Queremos un fullgrown tree, para reducir el sesgo
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
    seed = 37,
    dependent.variable.name = NULL, #Name of dependent variable, needed if no formula given
    classification = TRUE)
r_forest$predictions
r_forest$num.trees
r_forest$num.independent.variables
r_forest$mtry
r_forest$min.node.size
r_forest$variable.importance
r_forest$prediction.error
r_forest$forest
ncol(datos)
##########################################################################################
Bucle

n_trees_rf_bucle = c(100,250,500,750,1000,2000,3000,4000) #Numero de árboles a utilizar
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
      seed           = 565,
      classification = TRUE)
    rf_tabla[i,j] = round((1-r_forest_buc$prediction.error)*100, digits=3)
    k = k + 1
    tabla_comparacion$Porcentaje_de_aciertos[k] = round((1-r_forest_buc$prediction.error)*100, digits= 3)
    tabla_comparacion$Splits_por_nodo[k] = mtry_rf_bucle[j]
    tabla_comparacion$Numero_arboles[k]  = n_trees_rf_bucle[i]
    print(i)
  }  
}    

 tabla_comparacion$Porcentaje_de_aciertos = as.factor(tabla_comparacion$Porcentaje_de_aciertos)
    

   
tabla_comparacion
rf_tabla
tabla_comparacion$Porcentaje_de_aciertos = as.factor(tabla_comparacion$Porcentaje_de_aciertos)

library(ggplot2)
v2_rf = rf_tabla[,1]
v4_rf = rf_tabla[,2]
v8_rf = rf_tabla[,3]
v6_rf = rf_tabla[,4]
datos_grafica =  data.frame( "Sub" = v2_rf,
                             "Acc" = v4_rf,
                             "Sup" = v8_rf,
                             "Una" = v6_rf,
                             "Numero_de_arboles" = n_trees_rf_bucle)
ggplot(datos_grafica) +
  geom_line(aes(x = Numero_de_arboles, y = Sub, col = "2 var por split")) + 
  geom_line(aes(x = Numero_de_arboles, y = Acc, col = "4 var por split")) +
  geom_line(aes(x = Numero_de_arboles, y = Una, col = "6 var por split")) +
  geom_line(aes(x = Numero_de_arboles, y = Sup, col = "8 var por split")) + 
  scale_color_discrete("Número de variables por Split") +
  labs(title='Probabilidad de acierto en función del número de árboles, según el split',
       x='Número de árboles',
       y='Probabilidad de acierto') 

  

######################################################################################
#Otros features
importance(r_forest)
importance_pvalues(r_forest) #impurity corrected
predict.ranger
print(r_forest)
treeInfo