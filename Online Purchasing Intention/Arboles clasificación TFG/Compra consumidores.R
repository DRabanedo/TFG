#Script de Intención de compra//Código importante

##Primero asignamos la variable datos a nuestra base de datos, para poder asi estudiarla
library(rpart)
library(rpart.plot)
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
#########################################################################################

#Hacemos el primer arbol, default
tcar_default <- rpart( y ~ Administrative + Administrative_Duration + Informational
               + Informational_Duration +  ProductRelated + ProductRelated_Duration
               + BounceRates + ExitRates + PageValues + SpecialDay + Month 
               + OperatingSystems + Browser + Region +  TrafficType + VisitorType
               + Weekend  , data=datos, method = 'class')
rpart.plot(tcar_default, main = "Default", box.palette = c("red","green"))

tcar_default_table = table(predict(tcar_default, t="class"), datos$y)
tcar_default_table
aciertos_tcar_default = tcar_default_table[1,1]+tcar_default_table[2,2]
fallos_tcar_default = 12330 - aciertos_tcar_default
aciertos_tcar_default
fallos_tcar_default
printcp(tcar_default)
#Tenemos pues que este árbol clasifica de forma correcta el siguiente porcentaje de observaciones:
paste0("El acierto, con información privilegiada es del ", (aciertos_tcar_default/12330)*100, "%")
##########################################################################################

#Hacemos el arbol total, sin podarlo
min_nod <- 1
min_cp <- 0
cv <- 10

tcar_total <- rpart( y ~ Administrative + Administrative_Duration + Informational
                     + Informational_Duration +  ProductRelated + ProductRelated_Duration
                     + BounceRates + ExitRates + PageValues + SpecialDay + Month 
                     + OperatingSystems + Browser + Region +  TrafficType + VisitorType
                     + Weekend, data=datos, method = 'class',
                     control = rpart.control(xval = cv, minbucket = min_nod, cp = min_cp))
#rpart.plot(tcar_total, main = "Tree without restrictions", box.palette = c("red","green"))

tcar_total_table = table(predict(tcar_total, t="class"), datos$y)
tcar_total_table
aciertos_tcar_total = tcar_total_table[1,1]+tcar_total_table[2,2]
fallos_tcar_total = 12330 - aciertos_tcar_total 
aciertos_tcar_total
fallos_tcar_total
printcp(tcar_total)
paste0("El acierto sobre el conjunto original es del ", (aciertos_tcar_total/12330)*100, "%")

#Vemos la tabla de los nodos para elegir el óptimo, según el criterio que elijamos
printcp(tcar_total)

#Nos muestra el mejor numero de nodos para hacer un podado en función de su cp, 
#utilizando la regla 1-SE
plotcp(tcar_total)

#Vamos a podar el arbol utlizando el menor valor de xerror de la tabla (no tiene pq ser
#el óptimo según )
tcar_pruned_minxerror = prune(tcar_total, cp = tcar_total$cptable[which.min(tcar_total$cptable[,"xerror"]),"CP"])
rpart.plot(tcar_pruned_minxerror, main = "Best pruned Tree", box.palette = c("red","green"))

tcar_pruned_minxerror_table = table(predict(tcar_pruned_minxerror, t="class"), datos$y)
tcar_pruned_minxerror_table
aciertos_tcar_pruned_minxerror = tcar_pruned_minxerror_table[1,1]+tcar_pruned_minxerror_table[2,2]
fallos_tcar_pruned_minxerror = 12330 - aciertos_tcar_pruned_minxerror
aciertos_tcar_pruned_minxerror
fallos_tcar_pruned_minxerror

paste0("El acierto sobre el conjunto original es del ", (aciertos_tcar_pruned_minxerror/12330)*100, "%")

##########################################################################################

#Vamos a calcular los errorres por el método de validación cruzada,
#primero del arbol que minimiza el xerror y despúes con los del resto de la tabla
#para poder así compararlo

## Primero elegimos el número de capas de cross-validation
fold = 10
## Observaciones
n = 12330
## Tamanño de la muestra test
n_test = n/fold
## Tamaño de la muestra trainning
n_tr = n - n_test 
vn = 1:n
## Hacemos una tabla para almacenar los datos
v_tabla_optimo = matrix(rep(NA,fold*4), nrow = fold)
## Volvemos a extraer el cp óptimo ya que lo utilizaremos para construir el árbol
cp_optimo = sqrt(tcar_total$cptable[which.min(tcar_total$cptable[,"xerror"]),"CP"]*tcar_total$cptable[which.min(tcar_total$cptable[,"xerror"])-1,"CP"])
cp_optimo

#Ya tenemos el árbol y los parámetros preparados para aplicarle el método de
#cross-validation, veamos con el parámetro que minimiza el xerror

for (i in 1:fold ) {
  i_test = 1:n_test + (i - 1)*n_test
  i_tr = vn[-i_test]
  datos_tr = datos[i_tr,]
  datos_test = datos[i_test,]
  tcar_pruned = prune(tcar_total, cp_optimo)
  tcar_pruned_table = table(predict(tcar_pruned, datos_test, t="class",), datos_test$y)
  v_tabla_optimo[i,] = matrix(tcar_pruned_table, nrow = 1)
  
  
}
v_tabla_optimo
v_suma_optimo = apply(v_tabla_optimo,2,sum)
aciertos_cv_optimo = sum(v_suma_optimo[c(1,4)])
aciertos_cv_optimo
fallos_cv_optimo = sum(v_suma_optimo[c(2,3)])
fallos_cv_optimo
p_aciertos_cv_optimo = (aciertos_cv_optimo/12330)*100


paste0("El acierto en las muestras por cross-validation es del ", p_aciertos_cv_optimo, "%, con el parámetro de complejidad de ", round(cp_optimo, 6))

######################################################################################

#Vamos a hacer la comparativa con todos los valores de la cptable, donde se supone que están 
#los parámetros óptimos
par_comp = rep(NA,37)
eficiencia = rep(NA,37)
nodos = rep(NA,37)

tabla_comparacion = data.frame(Parametro_complejidad = par_comp, Eficiencia = eficiencia, Nodos = nodos)
cp_todos_v = c(Inf,tcar_total$cptable[,1])
nodos_todos_v = tcar_total$cptable[,2]

for (j in 1:37 ){
  fold = 10
  n = 12330
  n_test = n/fold
  n_tr = n - n_test 
  vn = 1:n
  min_nod <- 1
  min_cp <- 0
  cv <- 10
  v_tabla = matrix(rep(NA,fold*4), nrow = fold)
  cp_todos = sqrt(cp_todos_v[j]*cp_todos_v[j+1])
  
  for (i in 1:fold ) {
    i_test = 1:n_test + (i - 1)*n_test
    i_tr = vn[-i_test]
    datos_tr = datos[i_tr,]
    datos_test = datos[i_test,]
    tcar_todos <- rpart( y ~ Administrative + Administrative_Duration + Informational
                         + Informational_Duration +  ProductRelated + ProductRelated_Duration
                         + BounceRates + ExitRates + PageValues + SpecialDay + Month 
                         + OperatingSystems + Browser + Region +  TrafficType + VisitorType
                         + Weekend, data=datos_tr, method = 'class',
                         control = rpart.control(xval = cv, minbucket = min_nod, cp = min_cp))
    tcar_pruned = prune(tcar_todos, cp_todos)
    tcar_pruned_table = table(predict(tcar_pruned, datos_test, t="class",), datos_test$y)
    v_tabla[i,] = matrix(tcar_pruned_table, nrow = 1)
  }
  v_suma = apply(v_tabla,2,sum)
  aciertos_cv = sum(v_suma[c(1,4)])
  fallos_cv = sum(v_suma[c(2,3)])
  p_aciertos_cv = (aciertos_cv/12330)*100
  
  tabla_comparacion$Parametro_complejidad[j] = cp_todos
  tabla_comparacion$Eficiencia[j] = p_aciertos_cv
  tabla_comparacion$Nodos[j] = nodos_todos_v[j]
  
  
  
}
tabla_comparacion