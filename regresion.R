#Análisis exploratorio (seleccion de caracteristicas) y regresion logistica entre variables en R

"Primero haremos una limpieza y estudio exploratorio de los datos de los datos. 

Tener datos limpios y de buena calidad es un factor importante antes de comenzar a
trabajar con nuestro data set. Uno de los principales problemas a descubrir y solventar
son los valores perdidos, sobre esta cuestion será la primera que trabajaremos."

#Cargamos el dataset y asignamos un valor concreto a los valores perdidos

titanic <- read.csv("/Users/joseadiazg/Documents/Knime-WorkSpace/MachineLearning_Disaster_TID/dataset/train.csv", header = T, na.strings=c(""))

#Mostramos los datos, podemos comprobar como los vacios salen marcados como NA

head(titanic)

#Vamos a ver la distribución de los missing values y la variabilidad de las variables para entender mejor los datos. 

sapply(titanic,function(x) sum(is.na(x)))

#También podemos visualizarlo gráficamente

install.packages("Amelia")
library(Amelia)
missmap(titanic, main = "Missing values vs observed")

"Vemos como hay 177 con la edad perdida, 687 con la cabina vacia y 2 con el embarque.
Estos puntos ya son útiles, ya que aplicando simple lógica podemos ver que los valores perdidos
en la edad serán un factor importante y decisivo ya que acorde a datos historicos en el titanic
niños y mujeres tuvieron preferencia en el acceso a los botes."

#Vamos a estudiar la variabilidad

sapply(titanic, function(x) length(unique(x)))



"Directamente vemos que tenemos muchas variables que no nos sirven por lo que podemos 
seleccionar solo algunas, a priori, eliminamremos las que sabemos a ciencia cierta 
que no sirven para nada. Vemos claramente que:
  
  -PassengerId es solo un id, pues su variabilidad coincide con el numero de muestras en el dataset.
  -Cabin es clara candidata a ser eliminada pues su cantidad de valores perdidos la hacen inservible.
  -Ticket es el número de ticket por lo que tampoco es relevante por lo que las eliminaremos."


titanic2 <- titanic[,-c(1, 9, 11)]
head(titanic2)


"Nos hemos quitado el problema de los missing values en Cabin, pero seguimos teniendo problemas 
con la variable edad y esta como hemos visto antes no es menester de quitarla. Una posible solución es
asignar a los valores perdidos la media de los demás que si conocemos, el más común, la mediana...."

titanic2$Age[is.na(titanic2$Age)] <- mean(titanic2$Age,na.rm=T)


#Respecto a embarked eliminaremos las dos filas que llevan un valor perdido. 

titanic2 <- titanic2[!is.na(titanic2$Embarked),]
rownames(titanic2) <- NULL


##Podemos obtener los scatter para hacernos una idea y ver por donde atacar.

pairs(titanic2)

##Parece que la clase puede ser un factor importante

modelo=Pclass~Survived
regre=lm(modelo,titanic2)
summary(regre)

##Comprobemos con el precio del ticket

modelo2=Fare~Survived
regre2=lm(modelo,titanic2)
summary(regre2)

