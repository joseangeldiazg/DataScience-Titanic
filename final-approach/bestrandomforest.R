# Cargamos librerias 

suppressMessages(library(caret))                                 # Machine learning
suppressMessages(library(rpart))
suppressMessages(library(e1071))
suppressMessages(library(randomForest))
suppressMessages(library(mlbench))
suppressMessages(library(party))
suppressMessages(library(ggplot2))                               # Plotting
suppressMessages(library(lattice))
suppressMessages(library(grid))
suppressMessages(library(gridExtra, warn.conflicts = FALSE))
suppressMessages(library(ggrepel))
suppressMessages(library(vcd))
suppressMessages(library(rpart.plot))
suppressMessages(library(rattle))
suppressMessages(library(ROCR))
suppressMessages(library(mice))                                  # Data imputation
suppressMessages(library(xtable))                                # Pretty printing dataframes
suppressMessages(library(plyr, warn.conflicts = FALSE))          # Manipulating dataframes
suppressMessages(library(Hmisc))
suppressMessages(library(Amelia))                                # Missing data
suppressMessages(library(dplyr, warn.conflicts = FALSE))
suppressMessages(library(stringr))  


"Obtenemos los datos"

train <- read.csv('./data/train.csv', na.strings=c("NA", "NULL", ""), stringsAsFactors = F)
test  <- read.csv('./data/test.csv', na.strings=c("NA", "NULL", ""), stringsAsFactors = F)



"Juntamos ambos origenes de datos para poder trabajar con ellos juntos. La opcion 
fill de rbind, si alguno de los data set le falta una columna la rellenará."

all <- rbind.fill(train, test)


"Creamos algunas funciones utiles para el renderizado de tablas"

renderTable <- function(data) {
  print(xtable(data), type = "html")
}

sampleDataFrame <- function(data, size) {
  sampleIndex <- sample(1:nrow(train), size)
  return(data[sampleIndex, ])
}


"Vamos a ver de que tipo son nuestros datos"

str(train)


"Vamos a ver si tenemos valores perdidos"

missmap(train, main = "Missing Values (Training Data-set)", col = c("red", "lightgrey"))

missmap(test, main = "Missing Values (Testing Data-set)", col = c("red", "lightgrey"))


"Vemos que si que los tenemos por lo que vamos a obtener cuantos"


missingCabinRows <- nrow(all[is.na(all$Cabin), ])
missingAgeRows <- nrow(all[is.na(all$Age), ])
missingFareRows <- nrow(all[is.na(all$Fare), ])
missingEmbarkedRows <- nrow(all[is.na(all$Embarked), ])




"Para las variables continuas podremos usar la funcion mice"

estimateMissingVariables <- function(data) 
{
  predictors <- c("Age", "Sex", "Fare", "Pclass", "SibSp", "Parch", "Embarked", "Title")
  set.seed(345)
  capture.output(model <- mice(data[, names(data) %in% predictors], method='rf'))
  output <- complete(model)
  data$Age <- output$Age
  data$Fare <- output$Fare
  return(data)
}

fixedAll <- estimateMissingVariables(all)

"Ahora realizaremos histogramas de los valores perdidos para ver la
distribución antes y después de predecir los valores y ver si se 
mantiene la distribucion mejora."


customHistogram <- function(data, column, title) {
  missing = nrow(data[is.na(data[,  column]), ])
  ggplot(data=data, aes_string(x = column)) +
    geom_histogram(bins = 20, na.rm = TRUE, fill = "blue", alpha = 0.2) +
    xlab(paste(column, "(", title, ", NA Count: ", missing, ")"))
}


allAge <- customHistogram(all, "Age", "Original")
fixedAllAge <- customHistogram(fixedAll, "Age", "Fixed")

allFare <- customHistogram(all, "Fare", "Original")
fixedAllFare <- customHistogram(fixedAll, "Fare", "Fixed")

grid.arrange(allAge, fixedAllAge, 
             allFare, fixedAllFare,
             ncol=2, nrow=2)


"Antes de guardar los datos haremos una copia de seguridad de los datos 
sin modificar."

trainOrigen <- train
testOrigen <- test
allOrigen <- all

"Añadimos los datos sin valores perdidos"

all <- fixedAll
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


"Vamos a estudiar los valores perdidos en la variable embarked"

missingEmbarkedIndex <- is.na(train$Embarked)
renderTable(train[missingEmbarkedIndex, ])

"Como es una variable categorica, podremos predecirla usando un arbol de decisión."

estimateMissingEmbarked <- function(data) {
  missing <- data[is.na(data$Embarked), ]
  present <- data[!is.na(data$Embarked), ]
  
  fol <- formula(Embarked ~ Sex + Age + Fare + Pclass + SibSp + Parch)
  model <- rpart(fol, method='class', data=present)
  missing$Embarked <-predict(model, missing, type="class")
  all <- rbind.fill(missing, present)
  all <- all[with(all, order(PassengerId)), ]
  return(all)
}

all <- estimateMissingEmbarked(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


"Vemos que ha predecido que han embarcado en C, cuando la mayoria es S, por lo 
que probablemente maualmente nosotros lo habriamos hecho mal"

renderTable(train[missingEmbarkedIndex, ])


"Ahora vamos a general algunas caracteristicas nuevas. El primer paso 
será convertir variables categoricas a factores, que son mejor manejadas."

toFactor <- function(data) {
  columns <- intersect(names(data), c("Survived", "Sex", "Embarked", "Pclass", "Ticket"))
  data[, columns] <- lapply(data[, columns] , factor)
  return(data)
}


all <- toFactor(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


str(train)

"Obtenemos el título de cada una de las personas del pasaje."

namePattern <- "(.+),\\s*(.+?)\\..+"

extractTitle <- function(name) {
  return(str_match_all(name, namePattern)[[1]][3])
}

addTitle <- function(data) {
  data$Title <- sapply(data$Name, extractTitle)
  data[, "Title"] <- as.factor(data[, "Title"])
  return(data)
}

all <- addTitle(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]

"Obtenemos el apellido."

strsplit(all$Name[1], split='[,.]')
strsplit(all$Name[1], split='[,.]')[[1]][1]

addSurname<- function(data) {
  data$Surname <- sapply(data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
  data[, "Surname"] <- as.factor(data[, "Surname"])
  return(data)
}

all <- addSurname(all)
train <- addSurname(train)
test <- addSurname(test)


"Vamos a estudiar la distribución de muestras por titulo."

countBarchart <- function(data, column, title) {
  ggplot(data, aes_string(x=column)) + 
    geom_bar(fill = "blue", alpha = 0.2) +
    geom_text(stat='count', aes(label=sprintf("%d\n(%d %%)", ..count.., round(..count.. * 100/sum(..count..), 0)), vjust=0)) +
    xlab(paste(column, "(", title, ")"))
}

countBarchart(train, "Title", "Overall")


"Vemos que hay muchos por lo que algunos se comportaran como outliers que 
podremos reunir en titulos como raros. Dejaremos solo los mas representados
y Dr como títulos propios"


addTitleWO <- function(data) {
  frequent <- c("Mr", "Miss", "Mrs", "Master", "Dr", "Rev")
  data$TitleWO <- sapply(data$Name, extractTitle)
  data$TitleWO[!(data$Title %in% frequent)] <- "Rare"
  data[, "TitleWO"] <- as.factor(data[, "TitleWO"])
  return(data)
}

all <- addTitleWO(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]

countBarchart(train, "TitleWO", "Overall")


"Un recurso útil para la visualización de datos en función de otros 
pueden ser los histogramas."


mosaicplot(train$TitleWO ~ train$Survived, 
           main="Perecidos por Titulo", shade=FALSE, 
           color=TRUE, xlab="Title", ylab="Survived")


"Parece ser que nuestra variable título ofrecerá grandes resultados.
Vamos a continuar con el análisis exploratorio de las demás variables."


mosaicplot(train$Sex ~ train$Survived, 
           main="Perecidos por Titulo", shade=FALSE, 
           color=TRUE, xlab="Title", ylab="Survived")


"Para la edad, dado que en un histograma veriamos poco,
dada la variabilidad, usaremos una función para pintar un gráfico
de barras."

categoricalResultHistogram <- function(data, column, categoryColumn, breaks) {
  groupColumn <- paste0(column, "Group")
  suppressWarnings(data[, groupColumn] <- cut2(data[, column], g=breaks, digits=0))
  survivors <- plyr::count(data, vars=c(groupColumn, categoryColumn))
  survivors <- group_by_(survivors, groupColumn) %>% dplyr::mutate(Percentage = round(freq * 100 / sum(freq)))
  
  ggplot(data = survivors, aes_string(x = groupColumn, y = "Percentage", fill = categoryColumn)) +
    geom_bar(stat="identity", position = "dodge") +
    geom_text(aes(label=sprintf("%d\n(%d %%)", freq, Percentage))) +
    xlab(column)
}

categoricalResultHistogram(train, "Age", "Survived", 10)


"Vemos que los  niños tienen más opciones de sobrevivir. Vamos a
analizar los outliers"

createBoxPlotLabels <- function(data, column) {
  meta <- boxplot.stats(data[, column])
  labels <-data.frame(value=round(median(data[, column]), 4), label="Median")
  labels <-rbind(labels,data.frame(value=round(mean(data[, column]), 4), label="Mean"))
  labels <-rbind(labels,data.frame(value=meta$stats[2], label="1st Quartile"))
  labels <-rbind(labels,data.frame(value=meta$stats[4], label="3rd Quartile"))
  if(length(meta$out) > 0) labels <-rbind(labels,data.frame(value=round(median(meta$out), 4), label="Outliers Median"))
  if(length(meta$out) > 0) labels <-rbind(labels,data.frame(value=round(mean(meta$out), 4), label="Outliers Mean"))
  return(labels)
}

customBoxPlot <- function(data, column, title) {
  labels <- createBoxPlotLabels(data, column)
  ggplot(data, aes_string(x="factor(0)", y=column)) +
    geom_boxplot(fill = "blue", alpha=0.2) +
    geom_point(position = position_jitter(width = 0.2), color = "darkblue") +
    geom_label_repel(data=labels,
                     aes(x=factor(0), y=value, label=paste0(label, ": ", value)),
                     colour="red", angle=0, size=3,
                     point.padding = unit(1.0, "lines"), box.padding = unit(0.5, "lines")) +
    xlab(title)
}

customBoxPlot(all, "Age", "Overall")

"Vamos a reemplazar los outliers con la media. Para ello, usaremos nuevas 
funciones."

quarter3Indexes <- function(data, column) {
meta <- boxplot.stats(data[, column])
q3 <- meta$stats[4]
return(which( data[, column] > q3))
}

outliersMedian <- function(data, column) {
  meta <- boxplot.stats(data[, column])
  return(median(meta$out))
}

addAgeWO <- function(data) {
  data$AgeWO <- data$Age
  data$AgeWO[data$Age <13] <- 12
  q3Median <- median(data$Age[quarter3Indexes(data, "Age")])
  data$AgeWO[quarter3Indexes(data, "Age")] <- q3Median
  return(data)
}

all <- addAgeWO(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]

categoricalResultHistogram(train, "AgeWO", "Survived", 10)


"Vemos que aunque hemos sustituido los outliers por encima del
tercer cuartil, la distribucion del histograma, sigue siendo 
parecida al inicial. Para compararlos podemos hacer lo siguiente"

agewo<-categoricalResultHistogram(train, "AgeWO", "Survived", 10)
age<-categoricalResultHistogram(train, "Age", "Survived", 10)

grid.arrange(agewo, age, 
             ncol=2, nrow=2)


"Vamos a añadir una nueva variable isChild a nuestro modelo. Pero
antes haremos un estudio de esta variable para ver cual será el
punto de corte mejor."

categoricalResultCountBarchart <- function(data, column, categoryColumn) {
  survivors <- plyr::count(data, vars=c(column, categoryColumn))
  survivors <- group_by_(survivors, column) %>% dplyr::mutate(Percentage = round(freq * 100 / sum(freq)))
  
  ggplot(data = survivors, aes_string(x = column, y = "Percentage", fill = categoryColumn)) +
    geom_bar(stat="identity", position = "dodge") +
    geom_text(aes(label=sprintf("%d\n(%d %%)", freq, Percentage)))
}


addIsChild <- function(data) {
  data$IsChild <- data$Age < 12
  data[, "IsChild"] <- as.factor(data[, "IsChild"])
  return(data)
}


all <- addIsChild(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]

isChild12<-categoricalResultCountBarchart(train, "IsChild", "Survived")


"Ahora cambiamos a 14 el punto de corte"

addIsChild <- function(data) {
  data$IsChild <- data$Age < 14
  data[, "IsChild"] <- as.factor(data[, "IsChild"])
  return(data)
}

all <- addIsChild(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


isChild14<-categoricalResultCountBarchart(train, "IsChild", "Survived")

"Y comparamos ambos gráficos"

grid.arrange(isChild14, isChild12, ncol=2, nrow=1)


"Vemos que con 14 años la probabilidad de sobrevivir siendo niño
es mayor por lo que la dejaremos en este punto de corte."

categoricalResultCountBarchart(train, "IsChild", "Survived")



"Vamos a hacer ahora un estudio del precio del ticket y si sobrevivieron
primero con la distribución y luego con los outliers"

categoricalResultHistogram(train, "Fare", "Survived", 6)


customBoxPlot(all, "Fare", "Overall")

"Vemos que claramente hay outliers, por lo que vamos a intentar reducir estas
significancias."

quarter1Indexes <- function(data, column) {
  meta <- boxplot.stats(data[, column])
  q1 <- meta$stats[2]
  return(which(data[, column] < q1))
}


outliersMedian <- function(data, column) {
  meta <- boxplot.stats(data[, column])
  return(median(meta$out))
}

addFareWO <- function(data) {
  data$FareWO <- data$Fare
  q1Median <- median(data$Age[quarter1Indexes(data, "Fare")])
  q3Median <- median(data$Age[quarter3Indexes(data, "Fare")])
  data$FareWO[quarter1Indexes(data, "Fare")] <- q1Median
  data$FareWO[quarter3Indexes(data, "Fare")] <- q3Median
  return(data)
}

all <- addFareWO(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


"Obtenemos los histogramas, antes y después para ver si se mantienen
lo visto anteriormente en la distrubución"

fareHistogram <- customHistogram(all, "Fare", "Overall")
fareWOHistogram <- customHistogram(all, "FareWO", "Overall")

grid.arrange(fareHistogram, fareWOHistogram, ncol = 2)

categoricalResultHistogram(train, "FareWO", "Survived", 6)

"Vemos como se definen las diferencias pero se mantiene la distribución"
"Por último vamos a centrarnos en como la clase afecta en morir mo vivir."

mosaicplot(train$Pclass ~ train$Survived, 
           main="Muertes por clase", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")

"Vemos por tanto que si el pasajero corresponde a primera clase, tendrá
más opciones de vivir"


