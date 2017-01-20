"En el anterior script obteniamos maualmente las reglas para asignar a ciertos grupos
de población la clase buscada. Esto podemos hacerlo mecanicamente con un árbol de decision"
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)

"En R, tenemos rpart para dibujar nuestros arboles, pero podemos hacer uso de otras 
librerias externas que nos daran más juego."

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=titanic2,
             method="class")

plot(fit)
text(fit)

fancyRpartPlot(fit)



"En base a este algoritmo vamos a hacer una predicción."

Prediction <- predict(fit, titanicTest, type = "class")

submit <- data.frame(PassengerId = titanicTest$PassengerId, Survived = Prediction)

write.csv(submit, file = "/Users/joseadiazg/Documents/Knime-WorkSpace/MachineLearning_Disaster_TID/output/arbol.csv", row.names = FALSE)

#ACCURACY 0.77033

"Parece que no mejora nuestro modelo manual, por lo que vamos a dejar solo
las variables que nuestro proceso de seleccion de variables nos dijo que
era imporante"

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Fare,
             data=titanic2,
             method="class")

Prediction <- predict(fit, titanicTest, type = "class")

submit <- data.frame(PassengerId = titanicTest$PassengerId, Survived = Prediction)

write.csv(submit, file = "/Users/joseadiazg/Documents/Knime-WorkSpace/MachineLearning_Disaster_TID/output/arbol2.csv", row.names = FALSE)

"Mejoramos pero como podemos ver aún no es lo suficiente para aumentar
la precisión de nuestro anterior modelo."

#ACCURACY 0.77990



fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=titanic2,
             method="class", 
             control=rpart.control(minsplit=2, cp=0))

fancyRpartPlot(fit)

"Si analizamos lo que hemos realizado hemos eliminado las variables de control
por lo que el algoritmo obtiene una regla para cada uno de los distintos pasajeros.
Obteniendo un 100% de acc en training pero sobreaprendiendo en test."


Prediction <- predict(fit, titanicTest, type = "class")

submit <- data.frame(PassengerId = titanicTest$PassengerId, Survived = Prediction)

write.csv(submit, file = "/Users/joseadiazg/Documents/Knime-WorkSpace/MachineLearning_Disaster_TID/output/arboloverfit.csv", row.names = FALSE)

