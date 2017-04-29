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


"Ahora vamos a general algunas características nuevas. El primer paso 
	será convertir variables categóricas a factores, que son mejor manejadas."

toFactor <- function(data)
{
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

extractTitle <- function(name) 
{
  return(str_match_all(name, namePattern)[[1]][3])
}

addTitle <- function(data) 
{
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

addSurname<- function(data) 
{
  data$Surname <- sapply(data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
  data[, "Surname"] <- as.factor(data[, "Surname"])
  return(data)
}

all <- addSurname(all)
train <- addSurname(train)
test <- addSurname(test)


"Vamos a estudiar la distribución de muestras por titulo."

countBarchart <- function(data, column, title) 
{
  ggplot(data, aes_string(x=column)) + 
    geom_bar(fill = "blue", alpha = 0.2) +
    geom_text(stat='count', aes(label=sprintf("%d\n(%d %%)", ..count.., round(..count.. * 100/			sum(..count..), 0)), vjust=0)) +
    xlab(paste(column, "(", title, ")"))
}

countBarchart(train, "Title", "Overall")


"Vemos que hay muchos por lo que algunos se comportaran como
outliers que podremos reunir en títulos como raros. Dejaremos solo
los más representados y Dr como títulos propios"


addTitleWO <- function(data) 
{
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


"Para la edad, dado que en un histograma veríamos poco,
dada la variabilidad, usaremos una función para pintar un gráfico
de barras."

categoricalResultHistogram <- function(data, column, categoryColumn, breaks) 
{
  groupColumn <- paste0(column, "Group")
  suppressWarnings(data[, groupColumn] <- cut2(data[, column], g=breaks, digits=0))
  survivors <- plyr::count(data, vars=c(groupColumn, categoryColumn))
  survivors <- group_by_(survivors, groupColumn) %>% dplyr::mutate(Percentage =  round(freq * 100 / sum(freq)))
  
  ggplot(data = survivors, aes_string(x = groupColumn, y = "Percentage", fill = categoryColumn)) +
    geom_bar(stat="identity", position = "dodge") +
    geom_text(aes(label=sprintf("%d\n(%d %%)", freq, Percentage))) +
    xlab(column)
}

categoricalResultHistogram(train, "Age", "Survived", 10)


"Vemos que los  niños tienen más opciones de sobrevivir. Vamos a
analizar los outliers"

createBoxPlotLabels <- function(data, column) 
{
  meta <- boxplot.stats(data[, column])
  labels <-data.frame(value=round(median(data[, column]), 4), label="Median")
  labels <-rbind(labels,data.frame(value=round(mean(data[, column]), 4), label="Mean"))
  labels <-rbind(labels,data.frame(value=meta$stats[2], label="1st Quartile"))
  labels <-rbind(labels,data.frame(value=meta$stats[4], label="3rd Quartile"))
  if(length(meta$out) > 0) labels <-rbind(labels,data.frame(value=round(median(meta$out), 4), label="Outliers Median"))
  if(length(meta$out) > 0) labels <-rbind(labels,data.frame(value=round(mean(meta$out), 4), label="Outliers Mean"))
  return(labels)
}

customBoxPlot <- function(data, column, title) 
{
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

"Vamos a reemplazar los outliers con la media. Para ello, 
usaremos nuevas  funciones."

quarter3Indexes <- function(data, column) 
{
  meta <- boxplot.stats(data[, column])
  q3 <- meta$stats[4]
  return(which( data[, column] > q3))
}

outliersMedian <- function(data, column) 
{
  meta <- boxplot.stats(data[, column])
  return(median(meta$out))
}

addAgeWO <- function(data) 
{
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
tercer cuartil, la distribución del histograma, sigue siendo 
parecida al inicial. Para compararlos podemos hacer lo siguiente"

agewo<-categoricalResultHistogram(train, "AgeWO", "Survived", 10)
age<-categoricalResultHistogram(train, "Age", "Survived", 10)

grid.arrange(agewo, age, 
             ncol=2, nrow=2)


"Vamos a añadir una nueva variable isChild a nuestro modelo. Pero
antes haremos un estudio de esta variable para ver cual será el
punto de corte mejor."

categoricalResultCountBarchart <- function(data, column, categoryColumn)
{
  survivors <- plyr::count(data, vars=c(column, categoryColumn))
  survivors <- group_by_(survivors, column) %>% dplyr::mutate(Percentage = round(freq * 100 / sum(freq)))
  
  ggplot(data = survivors, aes_string(x = column, y = "Percentage", fill = categoryColumn)) +
    geom_bar(stat="identity", position = "dodge") +
    geom_text(aes(label=sprintf("%d\n(%d %%)", freq, Percentage)))
}


addIsChild <- function(data) 
{
  data$IsChild <- data$Age < 12
  data[, "IsChild"] <- as.factor(data[, "IsChild"])
  return(data)
}


all <- addIsChild(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]

isChild12<-categoricalResultCountBarchart(train, "IsChild", "Survived")


"Ahora cambiamos a 14 el punto de corte"

addIsChild <- function(data) 
{
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

quarter1Indexes <- function(data, column) 
{
  meta <- boxplot.stats(data[, column])
  q1 <- meta$stats[2]
  return(which(data[, column] < q1))
}


outliersMedian <- function(data, column) 
{
  meta <- boxplot.stats(data[, column])
  return(median(meta$out))
}

addFareWO <- function(data)
{
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
lo visto anteriormente en la distribución"

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

"Analicemos ahora las variables Parch y Sisbsp"

mosaicplot(train$Parch ~ train$Survived, 
           main="Muertes por Parch", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")


mosaicplot(train$SibSp ~ train$Survived, 
           main="Muertes por SibSp", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")


"Vemos que cuando las variables toman valor 1, es decir un hijo,
padre/madre, o esposa a bordo las prob de sobrevivir son mayores. Uniremos
estas variables en una sola que aglutine todos los miembros de la familia."

addFamilySize <- function(data) {
  data$FamilySize <- data$SibSp + data$Parch + 1
  return(data)
}

all <- addFamilySize(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]

"Una idea que se nos viene a la mente es estudiar la probabilidad de las
familias de permanecer juntas, por ello, crearemos una nueva función con 
el % de sobrevivir según los apellidos"

computeSurvivalRatePerColumn <- function(data, column) 
{
  rates <- plyr::count(data, vars=c(column, "Survived"))
  rates <- group_by_(rates, column) %>% dplyr::mutate(SurvivalRate = round(freq * 100 / sum(freq)))
  rates <- rates[rates$Survived == 1, ]
  rates <- rates[, which(names(rates) %in% c(column, "SurvivalRate"))]
  names(rates)[names(rates) == "SurvivalRate"] <- paste0("SurvivalRateBy", column)
  return(rates)
}

addSurvivalRate <- function(column, data, rateData)
{
  rates <- computeSurvivalRatePerColumn(rateData, column)
  rateColumn <- paste0("SurvivalRateBy", column)
  
  if(rateColumn %in% names(data)) 
  {
    data <- data[ , -which(names(data) %in% c(rateColumn))]
  }
  data <- left_join(data, rates,by=column)
  data[is.na(data[, rateColumn]), rateColumn] <- 0
  return(data)
}

all <- addSurvivalRate("Surname", all, train)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


"Otra variable que podemos añadir es si es madre para ello
podemos fijaremos la edad en 21 años, sexo mujer y parch>0"

addIsMother <- function(data) 
{
  data$IsMother <- data$Age > 21 & data$Sex == "female" & data$Parch > 0
  data[, "IsMother"] <- as.factor(data[, "IsMother"])
  return(data)
}

all <- addIsMother(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]

categoricalResultCountBarchart(train, "IsMother", "Survived")


"Vemos que las madres tienen mayor probabilidad de sobrevivir.
Otra variable que podemos añadir es si viaja solo, ya que una 
persona viajando sola solo tuviera que cuidar de si mismo y 
ofrecería mayores probabilidades de sobrevivir"

addIsAlone <- function(data)
{
  data$IsAlone <- data$Parch==0 & data$SibSp==0
  data[, "IsAlone"]<- as.factor(data[,"IsAlone"])
  return(data)
}

all <- addIsAlone(all)
train <- addIsAlone(train)
test <- addIsAlone(test)

categoricalResultCountBarchart(train, "IsAlone", "Survived")


"Vamos a crear una variable para identificar familias. Para ello, 
dado que dos personas de familia distintas pueden tener el mismo 
apellido, haremos uso también del tamaño de la familia, por lo que 
personas con el mismo apellido y un mismo numero de miembros de la familia
casi con toda probabilidad serán familiares"


addFamilyID <- function(data) {
  data$FamilyID <- paste0(data$Surname, as.character(data$FamilySize))
  data[, "FamilyID"] <- as.factor(data[, "FamilyID"])
  return(data)
}

all <- addFamilyID(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


"Habrá algunos outliers, por lo que cuando sean menores de 3 le 
asignaremos familia pequeña"



MIN_FAMILY_SIZE <- 3

tooSmallFamiliesIndexes <- function(data)
{
  return(which(data$FamilySize < MIN_FAMILY_SIZE))
}

addFamilyIDWO <- function(data) 
{
  data$FamilyIDWO <- paste0(data$Surname, as.character(data$FamilySize))
  data$FamilyIDWO[tooSmallFamiliesIndexes(data)] <- paste0("FamilySize<", toString(MIN_FAMILY_SIZE))
  data[, "FamilyIDWO"] <- as.factor(data[, "FamilyIDWO"])
  return(data)
}

all <- addFamilyIDWO(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]

all <- addSurvivalRate("FamilyIDWO", all, train)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


"Vamos a realizar un estudio de la cabina para intentar lidiar con los valores
perdidos"


extractDeck <- function(cabin) {
  return(toString(unique(strsplit(cabin, "[^A-Z]+")[[1]])))
}

addDeck <- function(data) {
  data$Deck <- sapply(data$Cabin, extractDeck)
  data[, "Deck"] <- as.factor(data[, "Deck"])
  return(data)
}

all <- addDeck(all)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]

countBarchart(train, "Deck", "Overall")


categoricalResultCountBarchart(train, "Deck", "Survived")

"Vamos a ver si quizá pasajeros que compartan el ticket también comparten
la cabina."

normalizeList <- function(elems) 
{
  elems <- unique(elems)
  elems <- elems[!is.na(elems)]
  s <- paste(elems, collapse = " ")
  if(str_length(trimws(s)) == 0) {
    s <- NA
  }
  return(s)
}

cabins <- all[with(all, order(Ticket)), which(names(all) %in% c("Ticket", "Cabin"))]
cabins <- cabins %>%
  group_by(Ticket) %>%
  summarise(Cabin = list(Cabin))
cabins$Cabin <- sapply(cabins$Cabin, normalizeList)

ticketsMissngCabinRows <- nrow(cabins[is.na(cabins$Cabin), ])

"Parece ser que no. Una posibilidad será añadir la cabina an 
en función de la clase."


"Análisis del ticket"


aggregateFunction <- function(s) 
{
  return(paste(s, collapse = " ~ "))
}

tickets <- train[with(train, order(Ticket)), which(names(train) %in% c("Ticket", "Name", "Surname", "Title", "Sex"))]
tickets <- tickets %>%
  group_by(Ticket) %>%
  summarise(Names = aggregateFunction(Name), 
            Surnames = aggregateFunction(Surname), 
            Titles = aggregateFunction(Title), 
            Genders = aggregateFunction(Sex),
            People = n())
tickets <- tickets[tickets$People > 1, ]

all <- addSurvivalRate("Ticket", all, train)
train <- all[all$PassengerId %in% train$PassengerId, ]
test <- all[all$PassengerId %in% test$PassengerId, ]


"CLASIFICACION"


"Creamos constructores para cada uno de los métodos que usaremos."

RandomForestBuilder <- function() {
  mtry = 1
  
  name = paste("Random Forest (randomForest) mtry:", mtry)
  
  model = function(fml, data) {
    set.seed(345)
    numVars <- length(attr(terms(fml), "term.labels"))
    maxMtry <- floor(sqrt(numVars))
    
    if (mtry > maxMtry) {
      return(randomForest(formula(fml), data = data))
    }
    return(randomForest(formula(fml), data = data, mtry = mtry))
  }
  
  predictions = function(model, data) {
    return(stats::predict(model, newdata = data, type = "class"))
  }
  
  probabilities = function(model, data) {
    result <- predict(model, newdata=data, type="prob")
    return(result[, 2])
  }
  
  return(list(name=name, model=model, predictions=predictions, probabilities=probabilities))
}

RandomForestRandomCVBuilder <- function() {
  name = "Random Forest with Random Search Cross Validation (rpart)"
  
  model = function(fml, data) {
    set.seed(345)
    return(randomForestRandomCrossValidation(fml, data, "Accuracy", radius=10, repeats=3))
  }
  
  predictions = function(model, data) {
    return(stats::predict(model, newdata = data, type = "raw"))
  }
  
  probabilities = function(model, data) {
    result <- predict(model, newdata=data, type="prob")
    return(result[, 2])
  }
  
  return(list(name=name, model=model, predictions=predictions, probabilities=probabilities))
}

DecisionTreeBuilder <- function() {
  name = "Decision Tree (rpart)"
  
  model = function(fml, data) {
    set.seed(345)
    return(rpart(fml, data = data, method="class"))
  }
  
  predictions = function(model, data) {
    return(stats::predict(model, newdata = data, type = "class"))
  }
  
  probabilities = function(model, data) {
    result <- predict(model, newdata=data, type="prob")
    return(result[, 2])
  }
  
  return(list(name=name, model=model, predictions=predictions, probabilities=probabilities))
}

ConditionalInferenceBuilder <- function() {
  mtry = 1
  
  name = paste("Conditional Inference Forest (party) mtry: ", mtry)
  
  model = function(fml, data) {
    set.seed(345)
    
    numVars <- length(attr(terms(fml), "term.labels"))
    maxMtry <- floor(sqrt(numVars))
    
    if (mtry > maxMtry) {
      return(cforest(fml, data = data, controls=cforest_unbiased()))
    }
    return(cforest(fml, data = data, controls=cforest_unbiased(mtry = mtry)))
  }
  
  predictions = function(model, data) {
    return(stats::predict(model, newdata = data, type = "response"))
  }
  
  probabilities = function(model, data) {
    result <- predict(model, newdata=data, type="prob")
    result <- lapply(result, `[`, 2)
    return(result)
  }
  
  return(list(name=name, model=model, predictions=predictions, probabilities=probabilities))
}

"Creamos funciones para la evaluación en training de los clasificadores"


fmeasure <- function(confusion) {
  p <- confusion$byClass["Pos Pred Value"]
  r <- confusion$byClass["Sensitivity"]
  f <- 2 * ((p * r) / (p + r))
  return(f)
}

accuracy <- function(confusion) {
  return(confusion$overall["Accuracy"])
}

createFormulaName <- function(fml) {
  formulaName <- paste(format(fml), collapse = "")
  formulaName <- str_replace_all(formulaName, "[\\s]", "")
  return(formulaName)
}

evaluateModel <- function(fml, ModelBuilder, predictionColumn, trainData, testData) {
  formula <- createFormulaName(fml)
  modelBuilder <- ModelBuilder()
  model <- modelBuilder$model(fml, trainData)
  predictions <- modelBuilder$predictions(model, testData)
  confusion <- confusionMatrix(data = predictions, reference = testData$Survived)
  evaluation <- data.frame(model = modelBuilder$name, 
                           formula = formula,
                           accuracy = accuracy(confusion), 
                           fmeasure = fmeasure(confusion), 
                           stringsAsFactors=FALSE)
  return(evaluation)
}


evaluateModels <- function(formulas, ModelBuilders, predictionColumn, data, testData = NULL) {
  set.seed(3456)
  if(is.null(testData)) {
    trainIdx <- createDataPartition(data$Survived, p = 0.6, list = FALSE, times = 1)
    trainData <- data[trainIdx,]
    testData <- data[-trainIdx,]
  } else {
    trainData <- data
  }
  
  evaluations <- data.frame(model = character(), 
                            formula = character(), 
                            accuracy = numeric(0), 
                            fmeasure = numeric(0), 
                            stringsAsFactors=FALSE)
  
  for (ModelBuilder in ModelBuilders) {
    for (fml in formulas) {
      evaluation <- evaluateModel(fml, ModelBuilder, predictionColumn, trainData, testData)
      evaluations <- bind_rows(evaluations, evaluation)
    }
  }
  renderTable(evaluations)
}


trainIdx <- createDataPartition(train$Survived, p = 0.6, list = FALSE, times = 1)
trainData <- train[trainIdx,]
testData <- train[-trainIdx,]


trainData <- addSurvivalRate("Surname", trainData, trainData)
testData <- addSurvivalRate("Surname", testData, trainData)


trainData <- addSurvivalRate("Ticket", trainData, trainData)
testData <- addSurvivalRate("Ticket", testData, trainData)

trainData <- addSurvivalRate("FamilyID", trainData, trainData)
testData <- addSurvivalRate("FamilyID", testData, trainData)

trainData <- addSurvivalRate("FamilyIDWO", trainData, trainData)
testData <- addSurvivalRate("FamilyIDWO", testData, trainData)


"Creamos distintas fórmulas de combinaciones para ver cuales son las que 
mejor se comportan a la hora de entrenar el modelo"

formulas <- c(Survived ~ Sex,
              Survived ~ Sex + Age,
              Survived ~ Sex + Age + Fare,
              Survived ~ Sex + Age + Fare + Pclass,
              Survived ~ Sex + Age + Fare + Pclass + SibSp,
              Survived ~ Sex + Age + Fare + Pclass + SibSp + Parch,
              Survived ~ Sex + Age + Fare + Pclass + FamilySize,
              Survived ~ Sex + Age + Fare + Pclass + FamilySize + Embarked,
              Survived ~ Sex + Age + Fare + Pclass + FamilySize + Embarked + Title,
              Survived ~ Sex + Age + Fare + Pclass + FamilySize + Embarked + TitleWO,
              Survived ~ Sex + AgeWO + Fare + Pclass + FamilySize + Embarked + TitleWO,
              Survived ~ Sex + AgeWO + FareWO + Pclass + FamilySize + Embarked + TitleWO,
              Survived ~ Sex + AgeWO + FareWO + Pclass + FamilySize + Embarked + TitleWO + IsChild,
              Survived ~ Sex + AgeWO + FareWO + Pclass + FamilySize + Embarked + TitleWO + IsChild + IsMother
              Survived ~ Sex + AgeWO + FareWO + Pclass + FamilySize + Embarked + TitleWO + IsChild + IsMother + IsAlone)

models <- c(RandomForestBuilder)


evaluateModel( Survived ~ Sex + AgeWO + FareWO + Pclass + FamilySize + Embarked + TitleWO + IsChild + IsMother,
               RandomForestBuilder, "Survived", trainData, testData)


evaluateModels(formulas, models, "Survived", trainData, testData)




"Evaluación basada en curvas ROC"

createPrediction <- function(fml, ModelBuilder, testData) {
  modelBuilder <- ModelBuilder()
  model <- modelBuilder$model(fml, trainData)
  probabilities <- modelBuilder$probabilities(model, testData)
  return(prediction(as.numeric(probabilities), testData$Survived))
}

createPerformance <- function(prediction) {
  return(performance(prediction, measure = "tpr", x.measure = "fpr"))
}

createAuc <- function(prediction) {
  auc <- performance(prediction, measure = "auc")
  return(auc@y.values[[1]])
}

buildModelName <- function(fml, ModelBuilder, auc) {
  formulaName <- createFormulaName(fml)
  aucName <- paste("(AUC: ", auc, ")")
  modelBuilder <- ModelBuilder()
  name <- paste0(modelBuilder$name, "\n", formulaName, "\n", aucName, "\n")
  return(name)
}

createRocPlot <- function(roc) {
  return(list(geom_ribbon(data = roc, aes(x=FPR, fill = Model, ymin=0, ymax=TPR), alpha = 0.2),
              geom_line(data = roc, aes(x=FPR, y=TPR, color = Model))))
}

generateAllROCs <- function(formulas, ModelBuilders, trainData, testData) {
  rocPlots <- c()
  for (ModelBuilder in ModelBuilders) {
    for (fml in formulas) {
      modelBuilder <- ModelBuilder()
      prediction <- createPrediction(fml, ModelBuilder, testData)
      performance <- createPerformance(prediction)
      auc <- createAuc(prediction)
      roc <- data.frame(FPR=unlist(performance@x.values),
                        TPR=unlist(performance@y.values),
                        Model=rep(buildModelName(fml, ModelBuilder, auc), each=length(performance@x.values)))
      rocPlots <- c(rocPlots, createRocPlot(roc))
    }
  }
  
  roc <- data.frame(FPR=c(0.0, 1.0),
                    TPR=c(0.0, 1.0),
                    Model=rep("Line of No-discrimination\n", each=2))
  rocPlots <- c(rocPlots, createRocPlot(roc))
  ggplot() + rocPlots + coord_fixed()
}

formulas <- c(Survived ~ Sex,
              Survived ~ Sex + Age,
              Survived ~ Sex + Age + Fare,
              Survived ~ Sex + Age + Fare + Pclass,
              Survived ~ Sex + Age + Fare + Pclass + SibSp,
              Survived ~ Sex + Age + Fare + Pclass + SibSp + Parch,
              Survived ~ Sex + Age + Fare + Pclass + FamilySize,
              Survived ~ Sex + Age + Fare + Pclass + FamilySize + Embarked,
              Survived ~ Sex + Age + Fare + Pclass + FamilySize + Embarked + Title,
              Survived ~ Sex + Age + Fare + Pclass + FamilySize + Embarked + TitleWO,
              Survived ~ Sex + AgeWO + Fare + Pclass + FamilySize + Embarked + TitleWO,
              Survived ~ Sex + AgeWO + FareWO + Pclass + FamilySize + Embarked + TitleWO,
              Survived ~ Sex + AgeWO + FareWO + Pclass + FamilySize + Embarked + TitleWO + IsChild,
              Survived ~ Sex + AgeWO + FareWO + Pclass + FamilySize + Embarked + TitleWO + IsChild + IsMother,
              Survived ~ Sex + AgeWO + FareWO + Pclass + FamilySize + Embarked + TitleWO + IsChild + IsMother + IsAlone)

models <- c(RandomForestBuilder)


"Dado que estamos ante un problema con una clase en clara minoria, quizá basarnos
solo en el Acc sea muy optimista a la hora de evaluar por ello, nos basaremos también
en la curva ROC evitando el sesgo hacia la clase mayoritaria."

generateAllROCs(formulas, models, trainData, testData)

"EVALUACIÓN TEST"

buildOutputName <- function(fml, modelName) {
  formulaName <- paste0(format(fml), collapse = "")
  output <- paste0(modelName, formulaName)
  output <- str_replace_all(output, "[^\\w]", "")
  output <- paste0(output, ".csv")
  return(output)
}

generatePredictions <- function(fml, ModelBuilder, predictionColumn, trainData, testData) {
  modelBuilder <- ModelBuilder()
  model <- modelBuilder$model(fml, trainData)
  predictions <- modelBuilder$predictions(model, testData)
  solution <- data.frame(PassengerID = testData$PassengerId, Survived = predictions)
  output <- buildOutputName(fml, modelBuilder$name)
  write.csv(solution, file = output, row.names = FALSE)
  testData$Survived <- predictions
  return(testData)
}

generateAllPredictions <- function(formulas, ModelBuilders, predictionColumn, trainData, testData) {
  for (ModelBuilder in ModelBuilders) {
    for (fml in formulas) {
      evaluation <- generatePredictions(fml, ModelBuilder, predictionColumn, trainData, testData)
    }
  }
}

fml <-Survived ~ Sex + AgeWO + FareWO + Pclass + SibSp + Parch + FamilySize + Embarked + TitleWO + FamilyIDWO


test <- generatePredictions(Survived ~ Sex + Age+ FareWO + Pclass + FamilySize + Embarked + TitleWO, RandomForestBuilder, "Survived", train, test)

evaluateModel( Survived ~ Sex + Age+ FareWO + Pclass + FamilySize + Embarked + TitleWO, RandomForestBuilder , "Survived", trainData, testData)

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

write.csv(submit, file = "/Users/joseadiazg/Desktop/datos/randomForestTune6.csv", row.names = FALSE)
