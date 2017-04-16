#Comparativa de métodos

library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)
library(randomForest)
library(party)
require(xgboost)
library(ade4)
library(dummy)
library(data.table)
#Vamos a probar arboles de decisión, randomforest (bagging)
# y xgboosting (xgboost)

#Primero probamos sin aplicar nada de preprocesado ni ingenieria de caracteristicas

trainOriginal <- read.csv("/Users/joseadiazg/Documents/Knime-WorkSpace/MachineLearning_Disaster_TID/dataset/train.csv", header = T, na.strings=c(""))
testOriginal <- read.csv("/Users/joseadiazg/Documents/Knime-WorkSpace/MachineLearning_Disaster_TID/dataset/test.csv", header = T, na.strings=c(""))

#Arreglamos los valores perdidos porque hay métodos que no trabajan con ellos


sapply(trainOriginal,function(x) sum(is.na(x)))
sapply(testOriginal,function(x) sum(is.na(x)))


trainOriginal$Age[is.na(trainOriginal$Age)] <- mean(trainOriginal$Age,na.rm=T)
trainOriginal$Embarked[c(62,830)] = "S"
trainOriginal$Embarked <- factor(trainOriginal$Embarked)

testOriginal$Age[is.na(testOriginal$Age)] <- mean(testOriginal$Age,na.rm=T)
testOriginal$Fare[is.na(testOriginal$Fare)]<- mean(testOriginal$Fare,na.rm=T)

#Ya solo tenemos valores perdidos en cabin, pero esta no la usaremos


#Arboles de decisión

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=trainOriginal,
             method="class")

fancyRpartPlot(fit)

Prediction <- predict(fit, testOriginal, type = "class")

submit <- data.frame(PassengerId = testOriginal$PassengerId, Survived = Prediction)

write.csv(submit, file = "/Users/joseadiazg/Desktop/pruebasR/arbolesSinPreProcesado.csv", row.names = FALSE)

#ACC: 0.77033


"RandomForest

Este es un ensemble de arboles de decisión.
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


fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked,
                    data=trainOriginal, 
                    importance=TRUE, 
                    ntree=2000)

varImpPlot(fit)

Prediction <- predict(fit, testOriginal)

submit <- data.frame(PassengerId = testOriginal$PassengerId, Survived = Prediction)

write.csv(submit, file = "/Users/joseadiazg/Desktop/pruebasR/randomForestSinPreprocesado.csv", row.names = FALSE)

#0.77990


set.seed(12345)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked,
               data = trainOriginal, 
               controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, testOriginal, OOB=TRUE, type = "response")

submit <- data.frame(PassengerId = testOriginal$PassengerId, Survived = Prediction)

write.csv(submit, file = "/Users/joseadiazg/Desktop/pruebasR/randomForest2SinPreprocesado.csv", row.names = FALSE)


#0.78469


#Vamos a probar ahora un algoritmo de boosting, concretamente el XGboost. 

labels<- trainOriginal[,2]
labels <- as.numeric(labels)

feature_selection <- function(data) {
  selected <- c('SibSp', 'Parch', 'Sex', 'Age', 'Fare', 'Pclass', 'Embarked')
  newdata <- data[,selected]
  newdata$SibSp <- as.numeric(newdata$SibSp)
  newdata$Parch <- as.numeric(newdata$Parch)
  newdata$Age <- as.numeric(newdata$Age)
  newdata$Age[is.na(newdata$Age)] <- 0
  newdata$Sex <- as.character(newdata$Sex)
  newdata$Sex[newdata$Sex == 'male'] <- 1
  newdata$Sex[newdata$Sex == 'female'] <- 0
  newdata$Sex <- as.numeric(newdata$Sex)
  newdata$Fare <- as.numeric(newdata$Fare)
  newdata$Fare[is.na(newdata$Fare)] <- 0
  newdata$Pclass <- as.numeric(newdata$Pclass)
  newdata$Embarked <- as.character(newdata$Embarked)
  newdata$Embarked[(newdata$Embarked == 'S')] = 0
  newdata$Embarked[(newdata$Embarked == '')] = 0
  newdata$Embarked[(newdata$Embarked == 'C')] = 1
  newdata$Embarked[(newdata$Embarked == 'Q')] = 2
  newdata$Embarked <- as.numeric(as.character(newdata$Embarked))
  newdata <- as.matrix(newdata)
  return(newdata)
}



ml <- xgboost(data = feature_selection(trainOriginal), label = labels, 
              nfold = 5, nrounds = 10, objective = "binary:logistic")

pred <- predict(ml, feature_selection(testOriginal))
pred[pred > .7] <- 1
pred[pred <= .7] <- 0

# Output in csv for submission
submission <- data.frame(PassengerId = testOriginal$PassengerId, Survived = pred)

write.csv(submission, file = "/Users/joseadiazg/Desktop/xgboostOriginalP2.csv", row.names = FALSE)

#0.76077

#Con probabilidad 0.7 -> 0.77512

"********************************************************************************
*********************************************************************************
********************************************************************************"



#Partimos del combinado con todas las variables generadas y la edad
#Predicha en función de las demás variables

trainPreprocesado <- combi[1:891,]
testPreprocesado <- combi[892:1309,]

#Podemos ver las variables que tiene con el comando head

head(trainPreprocesado)
head(testPreprocesado)


#Arboles de decision

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + FamilyID + Title + FamilySize,
             data=trainPreprocesado, 
             method="class")

fancyRpartPlot(fit)

Prediction <- predict(fit, testPreprocesado, type = "class")

submit <- data.frame(PassengerId = testPreprocesado$PassengerId, Survived = Prediction)

write.csv(submit, file = "/Users/joseadiazg/Desktop/pruebasR/arbolesConPreProcesado.csv", row.names = FALSE)

#0.80383


#XGBOOST

#Obtenemos los labels de la clase

labels<- trainPreprocesado[,2]
labels <- as.numeric(labels)

summary(trainPreprocesado$Title)

feature_selection <- function(data) {
  selected <- c('SibSp', 'Parch', 'Sex', 'Age', 'Fare', 'Pclass', 'Embarked', 'Title', 'FamilySize')
  newdata <- data[,selected]
  newdata$SibSp <- as.numeric(newdata$SibSp)
  newdata$Parch <- as.numeric(newdata$Parch)
  newdata$Age <- as.numeric(newdata$Age)
  newdata$FamilySize <- as.numeric(newdata$FamilySize)
  newdata$Age[is.na(newdata$Age)] <- 0
  newdata$Title <- as.character(newdata$Title)
  newdata$Title[(newdata$Title == "Col")] =0
  newdata$Title[(newdata$Title == "Dr")] =1
  newdata$Title[(newdata$Title == "Lady")] =2
  newdata$Title[(newdata$Title == "Master")] =3
  newdata$Title[(newdata$Title == "Miss")] =4
  newdata$Title[(newdata$Title == "Mlle")] =5
  newdata$Title[(newdata$Title == "Mr")] =6
  newdata$Title[(newdata$Title == "Mrs")] =7
  newdata$Title[(newdata$Title == "Ms")] =8
  newdata$Title[(newdata$Title == "Rev")] =9
  newdata$Title[(newdata$Title == "Sir")] =10
  newdata$Title <- as.numeric(newdata$Title)
  newdata$Sex <- as.character(newdata$Sex)
  newdata$Sex[newdata$Sex == 'male'] <- 1
  newdata$Sex[newdata$Sex == 'female'] <- 0
  newdata$Sex <- as.numeric(newdata$Sex)
  newdata$Fare <- as.numeric(newdata$Fare)
  newdata$Fare[is.na(newdata$Fare)] <- 0
  newdata$Pclass <- as.numeric(newdata$Pclass)
  newdata$Embarked <- as.character(newdata$Embarked)
  newdata$Embarked[(newdata$Embarked == 'S')] = 0
  newdata$Embarked[(newdata$Embarked == '')] = 0
  newdata$Embarked[(newdata$Embarked == 'C')] = 1
  newdata$Embarked[(newdata$Embarked == 'Q')] = 2
  newdata$Embarked <- as.numeric(as.character(newdata$Embarked))
  newdata <- as.matrix(newdata)
  return(newdata)
}


ml <- xgboost(data = feature_selection(trainPreprocesado), label = labels, 
              nfold = 5, nrounds = 200, objective = "binary:logistic")

pred <- predict(ml, feature_selection(test))
pred[pred > .5] <- 1
pred[pred <= .5] <- 0

# Output in csv for submission
submission <- data.frame(PassengerId = testPreprocesado$PassengerId, Survived = pred)

write.csv(submission, file = "/Users/joseadiazg/Desktop/pruebasR/xgboostWithPrepro2.csv", row.names = FALSE)

#0.71292 con 2000 de preprocesado


#0.78947






