#Install class package
#install.packages('class')
# Load class package
library(class)
library(ggplot2)
library(gridExtra)
library(caret)
library(MLmetrics)
library(dplyr)
library(e1071)
library(psych)
library(smotefamily)
library(ROSE)




#install.packages("MLmetrics")
getwd()
#dir()
setwd("C:/Users/Ganeshi/Documents")

#read the data set of red wine
file.exists("Red_Wine_Data.csv")
kdata =read.csv("Red_Wine_Data.csv")

#head(data)
names(kdata)
dim(kdata)

# get the summary result of the data set
summary(kdata)
sapply(kdata,function(x) sum(is.na(x)))
sum(is.na(kdata))

# So there haven't any missing values________________________

#checking foe duplicate data in the data set
dup = sum(duplicated(kdata)==TRUE)
dup
my_data1 = unique(kdata)
t=my_data1$quality
nrow(my_data1)

# So 240 observations are duplicate observations we remove that observations. Then we have
# Then we have only 1359 observations.________________________________

#reprocessing the quality variable as a categorical, using factor.

my_data1$quality = ifelse(my_data1$quality<=4,1 , 
                          ifelse(my_data1$quality>4 & my_data1$quality<=6, 
                                 2,3))
my_data1$quality <- as.factor(my_data1$quality)
table(my_data1$quality)


#slitting data into training and testing
set.seed(100)
split=sample(2,nrow(my_data1),replace=T,prob=c(0.8,0.2))
train= subset(my_data1,split==1)
test=subset(my_data1,split==2)
nrow(train)
nrow(test)

#Checking for duplicate data in train and test set
test = unique(test)
train = unique(train)

str(my_data1)
my_data= my_data1

train_scaled = scale(train[-12])
test_scaled = scale(test[-12])


library(class)
classifier_knn = knn(train = train_scaled, 
                test = test_scaled,cl= train$quality,
                k=10)
classifier_knn

# Confusiin Matrix 
cm <- table(test$quality, classifier_knn) 
cm 


accuracy <- sum(diag(cm))/length(test$quality)
sprintf("Accuracy: %.2f%%", accuracy*100)


###Scale both the training and testing set using preprocess

preProcValues <- preProcess(train, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, train)
testTransformed <- predict(preProcValues, test)

#### Model Evalution for different K values####
knnModel <- train(
  quality~ ., 
  data = trainTransformed, 
  method = "knn", 
  trControl = trainControl(method = "cv"), 
  tuneGrid = data.frame(k = seq(3,20,1))
)

#Training best performing Model
best_model<- knn3(
  quality ~ .,
  data = trainTransformed,
  k = knnModel$bestTune$k
)

#Model evalution
predictions <- predict(best_model, testTransformed,type = "class")
# Calculate confusion matrix
cm <- confusionMatrix(predictions, testTransformed$quality,mode="everything")
cm
# 



# ###############################################Smote #####################
# 

#smote
train_smote <- SMOTE(X =  train[,-12], 
                     target = train[,12], 
                     dup_size = 1)
train_smote<- train_smote$data # extract only the balanced dataset
train_smote$class <- as.factor(train_smote$class)

prop.table(table(train_smote$class))

#Checking for duplicate data in train and test set
train = unique(train_smote)
test= unique(test)

str(my_data1)
my_data= my_data1

train_scaled = scale(train[-12])
test_scaled = scale(test[-12])


library(class)

classifier_knn = knn(train = train_scaled, 
                     test = test_scaled,cl= train$class,
                     k=10)
classifier_knn

# Confusiin Matrix 
# cm <- table(test$class, classifier_knn) 
# cm 


accuracy <- sum(diag(cm))/length(test$class)
sprintf("Accuracy: %.2f%%", accuracy*100)


###Scale both the training and testing set using preprocess

preProcValues <- preProcess(train, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, train)

testTransformed <- predict(preProcValues, test)

#### Model Evalution for different K values####
knnModel <- train(
  class~ ., 
  data = trainTransformed, 
  method = "knn", 
  trControl = trainControl(method = "cv"), 
  tuneGrid = data.frame(k = seq(3,39,1))
)

#Training best performing Model
best_model<- knn3(
  class ~ .,
  data = trainTransformed,
  k = knnModel$bestTune$k
)
F1_Score(predictions, trainTransformed$class)

#Model evalution
predictions <- predict(best_model, testTransformed,type = "class")
# Calculate confusion matrix
cm <- confusionMatrix(predictions, testTransformed$class,mode="everything")
cm

F1_Score(predictions, testTransformed$class)

