#Análisis exploratorio (selección de características) y regresión logística

"Primero haremos una limpieza y estudio exploratorio de los datos de los datos. 

Tener datos limpios y de buena calidad es un factor importante antes de comenzar a
trabajar con nuestro data set. Uno de los principales problemas a descubrir y solventar
son los valores perdidos, sobre esta cuestión será la primera que trabajaremos."

library(Amelia)

#Cargamos el dataset y asignamos un valor concreto a los valores perdidos

titanic <- read.csv("/Users/joseadiazg/Documents/Knime-WorkSpace/MachineLearning_Disaster_TID/dataset/train.csv", header = T, na.strings=c(""))
titanicTest <- read.csv("/Users/joseadiazg/Documents/Knime-WorkSpace/MachineLearning_Disaster_TID/dataset/test.csv", header = T, na.strings=c(""))

"Podemos comenzar a ver las distribuciones de las variables gráficamente
para hacernos una idea de los datos con los que estamos trabajando."

barplot(table(titanic$Survived),
        names.arg = c("Murio", "Vivio"),
        main="Survived", col="black")

barplot(table(titanic$Pclass), 
        names.arg = c("Primera", "Segunda", "Tercera"),
        main="Pclass (clase del viajero)", col="firebrick")

barplot(table(titanic$Sex),
        names.arg = c("Mujer", "Hombre"),
        main="Sex (genero)", col="darkviolet")

hist(titanic$Age, main="Age", xlab = NULL, col="brown")

barplot(table(titanic$SibSp), main="SibSp (hijos y esposa a bordo)", 
        col="darkblue")

barplot(table(titanic$Parch), main="Parch (Padres e hijos abordo)", 
        col="gray50")

hist(titanic$Fare, main="Fare (Precio del ticket)", xlab = NULL, 
     col="darkgreen")

barplot(table(titanic$Embarked), 
        names.arg = c("Cherbourg", "Queenstown", "Southampton"),
        main="Embarked (Lugar donde embarcó)", col="sienna")  

#Sería más interesante verlas de manera doble para ello podemos usar:

mosaicplot(titanic$Pclass ~ titanic$Survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")

#Con la clase podremos afinar poco nuestro modelo... vamos a probar con Sex.

mosaicplot(titanic$Sex ~ titanic$Survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Sex", ylab="Survived")


"Parece que tenemos un ganador... en base a Sex podremos hacer nuestra 
primera predicción salvando a todas las mujeres y asignando que no sobreviven
a todos los hombres.Vamos a ver con otras variales, quiza con Embarked 
podramos obtener donde montó la Tripulación y afinar en que estas personas
murieron dado que solo se puso a salvo a los pasajeros"


mosaicplot(titanic$Embarked ~ titanic$Survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Embarked", ylab="Survived")

#Embarked ofrece poco... pero vamos a ver que pasa con Cabin por ejemplo:

mosaicplot(titanic$Cabin ~ titanic$Survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Cabin", ylab="Survived")

"Lo que ocurre en este cuadro se llama valores perdidos, y debemos
arreglarlos para poder Seguir trabajando con el data set correctamente.
Vamos a ver la distribución de los missing values y la variabilidad
de las variables para entender mejor los datos." 

sapply(titanic,function(x) sum(is.na(x)))

#También podemos visualizarlo gráficamente

missmap(titanic, main = "Missing values vs observed")

titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age,na.rm=T)
titanic$Embarked[c(62,830)] = "S"
titanic$Embarked <- factor(titanic$Embarked)


"Vemos como hay 177 con la edad perdida, 687 con la cabina vacía y
2 con el embarque. Estos puntos ya son útiles, ya que aplicando simple
lógica podemos ver que los valores perdidos en la edad serán un factor
importante y decisivo ya que acorde a datos históricos en el titanic
niños y mujeres tuvieron preferencia en el acceso a los botes."

#Vamos a estudiar la variabilidad

sapply(titanic, function(x) length(unique(x)))

"Directamente vemos que tenemos muchas variables que no nos sirven por
lo que podemos seleccionar solo algunas, a priori, eliminaremos
las que sabemos a ciencia cierta que no sirven para nada. 
Vemos claramente que:
  
  -PassengerId es solo un id, pues su variabilidad coincide con el número
      de muestras en el dataset.

  -Con el nombre sucede exactamente lo mismo.

  -Cabin es clara candidata a ser eliminada pues su cantidad de valores 
      perdidos  la hacen inservible.

  -Ticket es el número de ticket por lo que tampoco es relevante por
      lo que las eliminaremos."


titanic2 <- titanic[,-c(1,4, 9, 11)]
head(titanic2)


"Nos hemos quitado el problema de los missing values en Cabin, 
pero seguimos teniendo problemas  con la variable edad y esta como
hemos visto antes no es menester de quitarla. Una posible solución 
es asignar a los valores perdidos la media de los demás que si
conocemos, el más común, la mediana...."

titanic2$Age[is.na(titanic2$Age)] <- mean(titanic2$Age,na.rm=T)


#Respecto a embarked eliminaremos las dos filas que llevan un valor perdido. 

titanic2 <- titanic2[!is.na(titanic2$Embarked),]
rownames(titanic2) <- NULL

#Entrenamos el modelo

model <- glm(Survived ~.,family=binomial(link='logit'),data=titanic2)

summary(model)

#Aplicamos el test anova 

anova(model, test="Chisq")

