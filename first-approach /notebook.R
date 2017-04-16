#Análisis exploratorio (seleccion de caracteristicas) y regresion logistica entre variables en R

"Primero haremos una limpieza y estudio exploratorio de los datos de los datos. 

Tener datos limpios y de buena calidad es un factor importante antes de comenzar a
trabajar con nuestro data set. Uno de los principales problemas a descubrir y solventar
son los valores perdidos, sobre esta cuestion será la primera que trabajaremos."

#Cargamos el dataset y asignamos un valor concreto a los valores perdidos

titanic <- read.csv("/Users/joseadiazg/Documents/Knime-WorkSpace/MachineLearning_Disaster_TID/dataset/train.csv", header = T, na.strings=c(""))
titanicTest <- read.csv("/Users/joseadiazg/Documents/Knime-WorkSpace/MachineLearning_Disaster_TID/dataset/test.csv", header = T, na.strings=c(""))

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
-Con el nombre sucede exactamente lo mismo. 
-Cabin es clara candidata a ser eliminada pues su cantidad de valores perdidos la hacen inservible.
-Ticket es el número de ticket por lo que tampoco es relevante por lo que las eliminaremos."


titanic2 <- titanic[,-c(1,4, 9, 11)]
head(titanic2)


"Nos hemos quitado el problema de los missing values en Cabin, pero seguimos teniendo problemas 
con la variable edad y esta como hemos visto antes no es menester de quitarla. Una posible solución es
asignar a los valores perdidos la media de los demás que si conocemos, el más común, la mediana...."

titanic2$Age[is.na(titanic2$Age)] <- mean(titanic2$Age,na.rm=T)


#Respecto a embarked eliminaremos las dos filas que llevan un valor perdido. 

titanic2 <- titanic2[!is.na(titanic2$Embarked),]
rownames(titanic2) <- NULL



#Entrenamos el modelo

model <- glm(Survived ~.,family=binomial(link='logit'),data=titanic2)

summary(model)

#Aplicamos el test anova 

anova(model, test="Chisq")

"En base al estudio realizado anteriormente, nos centraremos en la variables que el
estudio de regresion nos ofreció, es decir: Sex, Age y Pclass"

#Vamos a ver la relacion total de los que sobreviven 

table(titanic2$Survived)

#Tambien podemos verlo en manera de proporcion 

prop.table(table(titanic2$Survived))

"Un 61% muere por lo que una primera predicción trivial sería que todos mueren. Pero 
vamos a estudiar primero que pasa con el sexo, dado que la regresion nos decia que 
sex=male era casi con toda probabilidad causante de Survived=0"

prop.table(table(titanic2$Sex, titanic2$Survived),1) 

"El 74% de las mujeres vive, mientras que el 81% de los hombres muere. Esto nos hace ver
que el segundo modelo predictivo podria ser mujer=sobrevive hombre=muere."


test<-titanicTest #Guardamos un nuevo test para hacer pruebas con el 

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

#PRIMER MODELO ACCURACY: 0.76555


"*************************************************************************************"


"Vamos ahora a indagar en la variable edad, recordemos que esta ya tratada y con sus 
valores perdidos tratados."

summary(titanic2$Age)

"Esta variable es continua, una opción para que sea mas descriptiva y pueda referenciar
a una muestra mayor de nuestro conjunto de training es pasarla a categorica, por ejemplo
diferenciando entre adultos y niños para ello procedemos como sigue:"

titanic2$Child<-0
titanic2$Child[titanic2$Age<18]<-1

#Vemos ahora las proporciones de sobrevivir en función de estos datos:

aggregate(Survived ~ Child + Sex, data=titanic2, FUN=sum)

aggregate(Survived ~ Child + Sex, data=titanic2, FUN=function(x) {sum(x)/length(x)})


"A simple vista parece que si se es mujer la probabilidad de sobrevivir es mayor y si 
si es hombre menor, indiferentemente de si es niño o no, pero, si atendemos a valores 
historicos, se consideraba a un niño adulto con mucho menos de 18 años. Vamos a ver si 
cambia algo"

titanic2$Child<-0
titanic2$Child[titanic2$Age<14]<-1

aggregate(Survived ~ Child + Sex, data=titanic2, FUN=sum)

aggregate(Survived ~ Child + Sex, data=titanic2, FUN=function(x) {sum(x)/length(x)})

"Podemos comprobar como ahora, casi el 57% de los menores de 14 años hombres sobreviven, 
lo que puede ser una ligera mejora en nuestros resultados. Nos guardamos por tanto este
valor (14 años) como corte. Pero de momento la edad no nos aporta mucho."

"*******************************************************************************"

"Vamos a estudiar el comportamiento de otras variables importantes, la clase que el test
de regresion nos dijo que era muy importante y el precio del ticket que sin duda está ligado
a la clase"

"De nuevo estamos ante un problema ya que la clase puede tener tres valores, pero fare es una
variable continua que poco nos aporta, por lo que podemos reducirla a tres valores facilmente
manejables"

titanic2$Fare2 <- '30+'
titanic2$Fare2[titanic2$Fare < 30 & titanic2$Fare >= 20] <- '20-30'
titanic2$Fare2[titanic2$Fare < 20 & titanic2$Fare >= 10] <- '10-20'
titanic2$Fare2[titanic2$Fare < 10] <- '<10'


"Ahora vamos a estudiar las proporciones para cada variable"

aggregate(Survived ~ Fare2 + Pclass + Sex, data=titanic2, FUN=function(x) {sum(x)/length(x)})


"Obtenemos por tanto dos puntos clave, referidos a las mujeres en 3 clase con tickets
de mas de 20$, que con casi toda probabilidad perecen en el accidente. En base a esto,
podemos hacer una nueva predicción."

#SEGUNDO MODELO ACCURACY 0.77990

"**************************************************************************************"

"¿Que pasaria si metemos la variable edad anteriormente estudiada en nuestro modelo?"

aggregate(Survived ~ Child + Fare2 + Pclass + Sex, data=titanic2, FUN=function(x) {sum(x)/length(x)})


"Aunque parececia que anteriormente no nos ofreceria mejores resultados, ahora hemos conseguido
'salvar' algunos hombres al fin, por lo que nuestro modelo nos ofrece ya mejores resultados."


#TERCER MODELO ACCURACY 0.78947


