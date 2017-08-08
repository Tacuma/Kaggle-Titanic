#Tacuma SOlomon
#R code for Kaggle Titanic Data Set
#Uses rpart Decision Tree
#C5.0 Decision Tree
#Random Forest
#Crosstable

setwd("~/R")  #Sets working directory

#Imports the train and test Dataframes
train <- read.csv("~/R/train.csv")   
test <- read.csv("~/R/test.csv")    
str(train)

table(train$Survived)
prop.table(table(train$Survived))


#Comparing sex, class and survival rate categories
library(gmodels)
CrossTable(y = train$Sex, x = train$Survived)
CrossTable(y = train$Pclass, x = train$Sex)
CrossTable(y = train$Pclass, x = train$Survived)

##Feature Engineering
#Examining the Dataset and adding new Features
#Adding Child Feature
train$Child <- 0
train$Child[train$Age < 18] <- 1


#Building an Aggregate Table to view 
aggregate(Survived ~ Child + Sex, data = train, FUN = sum)
aggregate(Survived ~ Child + Sex, data = train, FUN = length)
aggregate(Survived ~ Child + Sex, data = train, FUN = function(x) {sum(x)/length(x)})

train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN = function(x) {sum(x)/length(x)})

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

submit = data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = 'modifiedwomensurvived.csv', row.names = FALSE)


#Used Rpart Decision Tree
library("rpart", lib.loc="~/R/win-library/3.3")
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")
plot(fit)
text(fit)
install.packages('rattle')
install.packages("https://cran.r-project.org/bin/windows/contrib/3.3/RGtk2_2.20.31.zip", repos=NULL)
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "rparttree.csv", row.names = FALSE)

#Used C5.0 Decision Tree
library(C50)
train2 <- data.frame(PClass = train$Pclass, Sex = train$Sex, Age = train$Age, SibSp = train$SibSp,
                     Parch = train$Parch, Fare = train$Fare, Embarked = train$Embarked)
levels(train2$Embarked)[1] = "missing"
titanic_model <- C5.0(x = train2, y = factor(train$Survived))
summary(titanic_model)
submit <- data.frame(PassengerId = test$PassengerId, Survived = p)
write.csv(submit, file = "C5.0titanicprediction.csv", row.names = FALSE)

#Chopping up the data to add new variables
test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN = function(x){strsplit(x, split='[,.]')[[1]][2]})
table(combi$Title)
combi$Title[combi$Title %in% c('Mme', "Mlle")] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
table(combi$Title)
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN = function(x){strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)
train <- combi[1:891,]
test <- combi[892:1309,]

#Using rpart on the reengineered dataset
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch
             + Fare + Embarked + Title + FamilySize + FamilyID,
             data = train,
             method = "class")
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "rparttree.csv", row.names = FALSE)


#Using the decision tree to predict missing age values (clever shit)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + 
                Embarked + Title + FamilySize, data=combi[!is.na(combi$Age),],
                method = "anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])


#Filling Missing Embarked Values
which(combi$Embarked == "")
combi$Embarked[c(62, 830)] = "S"
combi$Embarked <- factor(combi$Embarked)

#Filling in missing fare values
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm = TRUE)

#Creating another field called FamilyID2 to split the family values
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
str(combi$FamilyID)

#Resplit the data-set into train and test
train <- combi[1:891,]
test <- combi[892:1309,]

#Setting up Randomforest
install.packages('randomForest')
library(randomForest)
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch +
                      Fare + Embarked + Title + FamilySize + FamilyID2,
                    data = train,
                    importance = TRUE,
                    ntree=2000)
varImpPlot(fit)
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)


#Conditional Inference Trees
install.packages("party")
library(party)
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                   Embarked + Title + FamilySize + FamilyID,
                 data = train, 
                 controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "cforest.csv", row.names = FALSE)
