
"Vamos a trabajar con RandomForest. Este es un ensemble de arboles de decisión.
Los ensembles funcionan por votaciones, es decir, se generan arboles de decisión
para distintas variables y ante un ejemplo concreto si hay tres arboles que lo 
barajan y 2 dicen que sobrevive se tomará como que este ejemplo sobrevive. Los
RandomForest tienen una ventaja y es que evitan el problema del sobreentrenamiento.
Esto lo consiguen añadiendo aleatoriedad al conjunto de la sigueinte manera:

  -Bagging: Mediante el bagging lo que generamos es que los arboles se generan
con conjuntos de muestras aleatorias en cada vez. Podemos estimar que se dejan
fuera un 37% de las muestras y que algunas serán incluso repetidas.

  -Selección de variables: Los árboles generados van cambiando las variables
que toman para predecir."


library(randomForest)



"RandomForest no trabaja con elementos con missing values, por lo que
vamos a trabajar con el data set que ya solventamos este problema.
Para ello, en lugar de aplicar las transformaciones que vimos 
anteriormente a todo el dataset titanic2, lo que haremos será sobre
el combinado aplicar la función media"

combi$Age[is.na(combi$Age)] <- mean(combi$Age,na.rm=T)

  
summary(combi$CategoryAge2)


"En nuestro estudio de valores perdidos, también habia en alguna otra variable: "

summary(combi$Fare)
summary(combi$Embarked)

"Vamos arreglarlas. Como son pocas, con el comando which podemos obtener
donde están y  arreglarlas a mano:"

which(is.na(combi$Fare))
which(combi$Embarked == '')

combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)


combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)


"Por otro lado, el ramdomforest de R no admite factores con más de 32 valores,
por lo que vamos aumentar el rango de nuestra familyId a 3 para ser consideradas
pequeñas."


combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

summary(combi$FamilyID)

summary(combi$FamilyID2)

train <- combi[1:891,]
test <- combi[892:1309,]


"Ya tenemos los datos tratados por lo que podremos ejecutar nuestro
RandomForest para poder reproducir los experimentos hay que establecer
una semilla para la aleatoridad"

set.seed(12345)


fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize+ CategoryAge2,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)

varImpPlot(fit)

Prediction <- predict(fit, test)


submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(submit, file = "/Users/joseadiazg/Documents/Knime-WorkSpace/MachineLearning_Disaster_TID/output/randomforest1.csv", row.names = FALSE)


"***********************************************************************************"


"No mejoramos por lo que vamos a crear un Ramdom Forest que pueda aceptar
atributos de mayor calado para obtener mejores resultados"

install.packages('party')
library(party)


set.seed(12345)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + FamilyID+CategoryAge2,
                 data = train, 
                 controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type = "response")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(submit, file = "/Users/joseadiazg/Documents/Knime-WorkSpace/MachineLearning_Disaster_TID/output/randomforest2.csv", row.names = FALSE)


#MODELO CON ACCURACY 0.80861 hemos mejorado casi medio punto.

"********************************************************************************"

"Vamos a comprobar si mejora eligiendo las variables que hemos usado en nuestro
proceso de selección de variables"


set.seed(12345)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + Fare +
                 Title + FamilySize + FamilyID+CategoryAge2,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type = "response")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(submit, file = "/Users/joseadiazg/Documents/Knime-WorkSpace/MachineLearning_Disaster_TID/output/randomforest3.csv", row.names = FALSE)



"Parece que nuestras variables tampoco ayudan mucho, pero tenemos
el gran problema de la edad... que tenía muchos valores perdidos,
vamos a probar si en lugar de asignar a todos la media, hacemos
una predicción de la edad con los distintos valores. Antes de hacer
la predicción realizaremos en Knime una regresion logística sobre
Age, para comprobar que variables sob mejores para predecir esta."

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], 
                method="anova")

combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

train <- combi[1:891,]
test <- combi[892:1309,]

summary(combi$Age)

"También eliminaremos de nuestro modelo la variable categórica de edad, ya que 
parece que con la edad predicha podremos ajustar aún más que con esta variable."

set.seed(12345)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + FamilyID,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type = "response")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(submit, file = "/Users/joseadiazg/Documents/Knime-WorkSpace/MachineLearning_Disaster_TID/output/randomforest5.csv", row.names = FALSE)

#ACCURACY: 0.81340