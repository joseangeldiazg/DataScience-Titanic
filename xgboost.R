#Comparativa de métodos

#Partimos del combinado con todas las variables generadas y la edad
#Predicha en función de las demás variables

train <- combi[1:891,]
test <- combi[892:1309,]

#Podemos ver las variables que tiene con el comando head

head(train)

#XGBOOST


require(xgboost)

#Obtenemos los labels de la clase

labels<- train[,2]
labels <- as.numeric(labels)

summary(train$Title)

feature_selection <- function(data) {
  selected <- c('SibSp', 'Parch', 'Sex', 'Age', 'Fare', 'Pclass', 'Embarked', 'Title')
  newdata <- data[,selected]
  newdata$SibSp <- as.numeric(newdata$SibSp)
  newdata$Parch <- as.numeric(newdata$Parch)
  newdata$Age <- as.numeric(newdata$Age)
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

head(feature_selection)

ml <- xgboost(data = feature_selection(train), label = labels, 
              nfold = 5, nrounds = 5, objective = "binary:logistic")

pred <- predict(ml, feature_selection(test))
pred[pred > .5] <- 1
pred[pred <= .5] <- 0

# Output in csv for submission
submission <- data.frame(PassengerId = test$PassengerId, Survived = pred)

write.csv(submission, file = "/Users/joseadiazg/Desktop/xgboostwithtitle.csv", row.names = FALSE)

#0.78947

# Cross validation
cv <- xgb.cv(data = feature_selection(train), label = labels, nfold = 5, nrounds = 2, objective = 'binary:logistic')




