test<-read.csv(file="Downloads/TitanicShinyApplication-master/data/test.csv",header = TRUE)
train<-read.csv(file="Downloads/TitanicShinyApplication-master/data/train.csv",header = TRUE)
str(train)
is.na(train)
apply(is.na(train))
length(unique(train$PassengerId))
summary(train)
names(train)
#tells about the no. of columns in the file
attributes(test)
table(train$Survived)
prop.table(table(train$Survived))
#describes that about 38% people only got survived
test$Survived <- rep(0, 418)
#creates one Survived field in the test data frame
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "died.csv", row.names = FALSE)
#creates a file died.csv with data frame submit consistiong of only id & survived persons 
summary(train$Sex)
#tells about no. of males and females in train file
prop.table(table(train$Sex, train$Survived))
prop.table(table(train$Sex, train$Survived),1)
#tells that 74% of females and 19% of males only got survived...
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
summary(train$Age)
train$Child <- 0
test$Child <- 0
train$Child[train$Age < 18] <- 1
test$Child[test$Age < 18] <- 1
#assigns population under 18 with 1 and rest with 0
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
#tells about the proportion of male and female survived
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
#tells about the total no. of male and female are present
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
#tells the avg.
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
test$Fare2<-0
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
#describes majority of males, regardless of class or fare still didn't survived,also most of the class 3 women who paid more than $20 for their ticket also miss out on a lifeboat
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
#prefixes all survived records to zero except femle without Pclass=3 and Fare having greater than 20
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data=train,method="class")
plot(fit)
text(fit)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "decisiontree.csv", row.names = FALSE)
#fits the decision tree in the submit data frame
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data=train, method="class", control=rpart.control(minsplit=2, cp=0))
#cp stops splits that aren???t deemed important enough
#minsplit governs how many passengers must sit in a bucket before even looking for a split
plot(fit)
text(t)
library(rpart.plot)
library(RColorBrewer)
new.fit <- prp(fit,snip=TRUE)$obj
#An interactive version of the decision tree will appear in the plot tab where you simply click on the nodes that you want to kill
train$Name[1]
#access an individual by using the row number, 1, as an index instead
test$Survived <- NA
combi <- rbind(train, test)
#combines both fields of train and test
combi$Name <- as.character(combi$Name)
#typecast into char
combi$Name[1]
strsplit(combi$Name[1], split='[,.]')
#to break apart the original name over the provided two symbols
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
#sapply runs through the rows of the vector of names, and sends each name to the function
combi$Title <- sub(' ', '', combi$Title)
table(combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
#%in% operator checks to see if a value is part of the vector we???re comparing it to
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)
train <- combi[1:891,]
test <- combi[892:1309,]
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,data=train,  method="class")
