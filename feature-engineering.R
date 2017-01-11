
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)


"En nuestro primer script eliminamos la variable nombre al ser esta un identificador 
unico de cada uno de los viajeros, pero quizá podamos aplicar ingenieria de caracteristicas
a estos valores para obtener un nuevo atributo con el que afinar nuestro modelo"

titanic$Name[1-10]

"Podemos ver que los nombres tienen títulos... algo que estará muy ligado a la clase y que podrá
tener mucha relevancia a la hora de clasificar los pasajeros."

#Cargamos los data set para manejarlos manteniendo una copia de seguridad

train <- titanic
test <- titanicTest

"Tenemos que hacer los mismos cambios en los dos dataset (train y test), por lo que 
podemos unir ambos dataset en un combinado para trabajar sobre el."

test$Survived<-NA

combi<-rbind(train, test)

combi$Name <- as.character(combi$Name)
combi$Name[1]


"Ahora haciendo uso del comando strsplit podemos cortar el nombre y quedarnos con el titulo"

strsplit(combi$Name[1], split='[,.]')

strsplit(combi$Name[1], split='[,.]')[[1]][2]

"Ya tenemos el título, ahora con la función sapply podemos obtenerlo en una nueva variable Title."

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

"Puede ser que hayamos introducido espacios en blanco, estos podemos quitarlos con la función 
subs."

combi$Title <- sub(' ', '', combi$Title)

"Mostramos los datos"

table(combi$Title)


"La variabilidad es bastante alta, por lo que vamos a unir algunas variables. Este comando 
funciona de la siguiente manera, por un lado, comprueba si alguno de los elementos de nuestro
array c() esta (%in%) en la variable estudiada, Title, y si esta añade lo que tenemos a la derecha
de la flecha."

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'


"Por último cambiamos la variable a un factor que es por defecto como R los interpreta."

combi$Title <- factor(combi$Title)


"Por ultimo pasamos el data set modificados a neustro test y train"

train <- combi[1:891,]
test <- combi[892:1309,]


"Vamos a hacer una nueva prediccion con nuestro arbol de decision con 
este nuevo elemento."

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title,
             data=train, 
             method="class")


Prediction <- predict(fit, test, type = "class")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(submit, file = "/Users/joseadiazg/Documents/Knime-WorkSpace/MachineLearning_Disaster_TID/output/title.csv", row.names = FALSE)


#NUEVO MODELO 0.80383

"*************************************************************************"



"Vamos ahora a analizar las familias, pensando que tenemos dos variables, SibSp y Parch que 
estan muy relacionadas y pensando que quizá familias con muchos miembros tuvieran más probabilidad 
de sobrevivir al ayudarse."

combi$FamilySize <- combi$SibSp+combi$Parch +1


combi$FamilySize <- factor(combi$FamilySize)


train <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age+ Fare+ Title + FamilySize,
             data=train, 
             method="class")


Prediction <- predict(fit, test, type = "class")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(submit, file = "/Users/joseadiazg/Documents/Knime-WorkSpace/MachineLearning_Disaster_TID/output/titlefamilysie.csv", row.names = FALSE)


"De momento empeora por casi tres puntos nuestro resultado, vamos a ver si podemos extraer alguna variable más.
Ya que familias enteras es complicado que se salvaran por lo que vamos a indagar a ver si al menos algunas
miembros de la familia si. Para ello vamos a sacar los apellidos de la manera que lo hicimos con el titulo."

combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(combi$FamilyID))

famIDs

famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

"Ya podemos hacer una nueva predicción"

train <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + Fare + Title + FamilySize + FamilyID,
             data=train, 
             method="class")

Prediction <- predict(fit, test, type = "class")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(submit, file = "/Users/joseadiazg/Documents/Knime-WorkSpace/MachineLearning_Disaster_TID/output/finalFeatureEngienering.csv", row.names = FALSE)


"Parece que aún no mejoramos nuestro modelo por lo que vamos a añadir una ultima variable para ver como se comporta:"

summary(combi$Age)

combi$CategoryAge[combi$Age<=3]<-"Bebe"
combi$CategoryAge[combi$Age>3 & combi$Age <=14]<-"Niño"
combi$CategoryAge[combi$Age >14]<-"Adulto"

combi$CategoryAge <- factor(combi$CategoryAge)

summary(combi$CategoryAge)

train <- combi[1:891,]
test <- combi[892:1309,]


fit <- rpart(Survived ~ Pclass + Sex  + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID + CategoryAge,
             data=train, 
             method="class")

Prediction <- predict(fit, test, type = "class")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(submit, file = "/Users/joseadiazg/Documents/Knime-WorkSpace/MachineLearning_Disaster_TID/output/finalFeatureEngienering2.csv", row.names = FALSE)


"No mejoramos por lo que de momento tendremos que "
