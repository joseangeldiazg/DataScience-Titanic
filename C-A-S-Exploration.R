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
lo que puede ser una ligera mejora en nuestros resultados."




