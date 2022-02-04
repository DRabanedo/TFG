library(rpart)
library(rpart.plot)
datos <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/00468/online_shoppers_intention.csv', header = TRUE, sep =',')
compra <- factor(datos$Revenue , levels = c(FALSE,TRUE), labels = c("NO", "SI"))
datos$y <- compra

datos$OperatingSystems = factor(datos$OperatingSystems)
datos$Browser = factor(datos$Browser)
datos$Region = factor(datos$Region)
datos$TrafficType = factor(datos$TrafficType)
datos$VisitorType = factor(datos$VisitorType)
datos$Weekend = factor(datos$Weekend) 
datos$Month = factor(datos$Month, labels = c('Aug', 'Dec', 'Feb', 'Jul', 'June', 'Mar', 'May', 'Nov', 'Oct', 'Sep'))
datos$Revenue = factor(datos$Revenue)



fold = 10
n = 12330
12330 == 2*3^2*5*137
n_test = n/fold
n_tr = n - n_test
vn = 1:n
min_nod <- 1
min_cp <- 0
cv <- 10
v_tabla = matrix(rep(NA,fold*4), nrow = fold)
tcar_total <- rpart( y ~ Administrative + Administrative_Duration + Informational
                     + Informational_Duration +  ProductRelated + ProductRelated_Duration
                     + BounceRates + ExitRates + PageValues + SpecialDay + Month 
                     + OperatingSystems + Browser + Region +  TrafficType + VisitorType
                     + Weekend, data=datos, method = 'class',
                     control = rpart.control(xval = cv, minbucket = min_nod, cp = min_cp))

cp_optimo = tcar_total$cptable[which.min(tcar_total$cptable[,"xerror"]),"CP"]




for (i in 1:fold ) {
  print(i)
  i_test = 1:n_test + (i - 1)*n_test
  i_tr = vn[-i_test]
  datos_tr = datos[i_tr,]
  datos_test = datos[i_test,]
  tcar_total <- rpart( y ~ Administrative + Administrative_Duration + Informational
                       + Informational_Duration +  ProductRelated + ProductRelated_Duration
                       + BounceRates + ExitRates + PageValues + SpecialDay + Month 
                       + OperatingSystems + Browser + Region +  TrafficType + VisitorType
                       + Weekend, data=datos_tr, method = 'class',
                       control = rpart.control(xval = cv, minbucket = min_nod, cp = min_cp))
  tcar_pruned = prune(tcar_total, cp_optimo)
  tcar_pruned_table = table(predict(tcar_pruned, datos_test, t="class",), datos_test$y)
  v_tabla[i,] = matrix(tcar_pruned_table, nrow = 1)
  
  
}
v_tabla
v_suma = apply(v_tabla,2,sum)
aciertos = sum(v_suma[c(1,4)])
aciertos
fallos = sum(v_suma[c(2,3)])
fallos
p_aciertos = aciertos/n
p_aciertos

#Haciendo el cálculo para otros cp

fold = 10
n = 12330
12330 == 2*3^2*5*137
n_test = n/fold
n_tr = n - n_test
vn = 1:n
min_nod <- 1
min_cp <- 0
cp <- 0.0001
cv <- 10
v_tabla = matrix(rep(NA,fold*4), nrow = fold)
tcar_total <- rpart( y ~ Administrative + Administrative_Duration + Informational
                     + Informational_Duration +  ProductRelated + ProductRelated_Duration
                     + BounceRates + ExitRates + PageValues + SpecialDay + Month 
                     + OperatingSystems + Browser + Region +  TrafficType + VisitorType
                     + Weekend, data=datos, method = 'class',
                     control = rpart.control(xval = cv, minbucket = min_nod, cp = min_cp))

for (i in 1:fold ) {
  print(i)
  i_test = 1:n_test + (i - 1)*n_test
  i_tr = vn[-i_test]
  datos_tr = datos[i_tr,]
  datos_test = datos[i_test,]
  tcar_total <- rpart( y ~ Administrative + Administrative_Duration + Informational
                       + Informational_Duration +  ProductRelated + ProductRelated_Duration
                       + BounceRates + ExitRates + PageValues + SpecialDay + Month 
                       + OperatingSystems + Browser + Region +  TrafficType + VisitorType
                       + Weekend, data=datos_tr, method = 'class',
                       control = rpart.control(xval = cv, minbucket = min_nod, cp = min_cp))
  tcar_pruned = prune(tcar_total, cp)
  tcar_pruned_table = table(predict(tcar_pruned, datos_test, t="class",), datos_test$y)
  v_tabla[i,] = matrix(tcar_pruned_table, nrow = 1)
  
  
}
v_tabla
v_suma = apply(v_tabla,2,sum)
aciertos = sum(v_suma[c(1,4)])
aciertos
fallos = sum(v_suma[c(2,3)])
fallos
p_aciertos = aciertos/n
p_aciertos
