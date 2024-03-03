# Install and load the packages
#loading the required libraries
library(caret)
library(scales)
library(vip)
library(nnet)
library(e1071)
library(caTools)
library(MLmetrics)
library(car)
library(dplyr)
#install.packages("ROSE")
library(ROSE)
#install.packages("smotefamily")
library(smotefamily)
library(class)
library(e1071)
library(glmnet)

#install.packages("MLmetrics")
getwd()
#dir()
setwd("C:/Users/Ganeshi/Documents")

#read the data set of red wine
file.exists("Red_Wine_Data.csv")
data =read.csv("Red_Wine_Data.csv")

#head(data)
names(data)
dim(data)

# get the summary result of the data set
summary(data)
sapply(data,function(x) sum(is.na(x)))
sum(is.na(data))

# So there haven't any missing values________________________

#checking foe duplicate data in the data set
dup = sum(duplicated(data)==TRUE)
dup
my_data1 = unique(data)
t=my_data1$quality
nrow(my_data1)

# So 240 observations are duplicate observations we remove that observations. Then we have
# Then we have only 1359 observations.________________________________
#Splitting data set
set.seed(100)
library(caTools)
set.seed(100)
split = sample(2,nrow(my_data1),replace=T, prob=c(0.8,0.2))

train = subset(my_data1,split ==1)#Training data
test = subset(my_data1, split ==2)

nrow(train)
nrow(test)

#Checking for duplicate data in train and test set
test = unique(test)
train = unique(train)

str(my_data1)
my_data= my_data1

#reprocessing the quality variable as a categorical, using factor.
#For test set
test$quality = ifelse(test$quality<=4,1 , 
                      ifelse(test$quality>4 & test$quality<=6, 
                             2,3))
test$quality <- as.factor(test$quality)
table(test$quality)

#for train set
train$quality = ifelse(train$quality<=4,1 , 
                       ifelse(train$quality>4 & train$quality<=6, 
                              2,3))
train$quality <- as.factor(train$quality)
table(train$quality)


# You can see there Normal quality Category has high percentage than other 2 categories.
#Therefore the resampling categories are inbalanced data.
# Using sampling techniques and remedy to balance the dataset.

########################

#UP sampling
train_up <- upSample(x = train %>% select(-quality),
                     y = train$quality,
                     yname = "quality") 

prop.table(table(train_up$quality))
#smote
train_smote <- SMOTE(X =  train[,-12], 
                     target = train[,12], 
                     dup_size = 1)
train_smote<- train_smote$data # extract only the balanced dataset
train_smote$class <- as.factor(train_smote$class)

prop.table(table(train_smote$class))

######################################################################
#                    Ridge Regression                                #
######################################################################
x=model.matrix(class~.,data=train_smote)
#fix(x)
y=as.numeric(as.character(train_smote$class))

# Fitting ridge regression
fit.ridge=glmnet(x,y,alpha=0)

#Produce Ridge trace plot
plot(fit.ridge,xvar="lambda",label=TRUE,lw=2)

# Doing cross validation to select the best lambda
cv.ridge=cv.glmnet(x,y,alpha=0)
plot(cv.ridge)    
bestlam =cv.ridge$lambda.min 
bestlam

# Fitting the ridge regression model under the best lambda
out=glmnet(x,y,alpha=0,lambda = bestlam)
coef(out)


##################Predictions on training data###############################
#Use fitted best model to make predictions
y_predicted = predict(out,s= bestlam,newx = x)

# Convert predicted values to class labels
predicted_classes = ifelse(y_predicted  > 0, 2, 1)  # Assuming binary classification

# Confusion Matrix
conf_matrix_train_smote = table(Actual = y, Predicted = predicted_classes)
print("Confusion Matrix on train_smote:")
print(conf_matrix_train_smote)

# Use caret's confusionMatrix for more detailed output
cm = confusionMatrix(factor(predicted_classes), factor(y))
print("Detailed Confusion Matrix:")
print(cm)

# F1 Score
f1_score_train_smote = F1_Score(predicted_classes, y)
f1_score_train_smote


###########################Predictions on test data###########################

x_test = model.matrix(quality ~ ., data = test)
y_test = as.numeric(as.character(test$quality))
y_predicted_test = predict(fit.ridge, s = bestlam, newx = x_test)

# Convert predicted values to class labels
predicted_classes_test = ifelse(y_predicted_test > 0, 2, 1)  # Assuming binary classification

# Confusion Matrix for test data
conf_matrix_test = table(Actual = y_test, Predicted = predicted_classes_test)
print("Confusion Matrix on test set:")
print(conf_matrix_test)

# Use caret's confusionMatrix for more detailed output
cm_test = confusionMatrix(factor(predicted_classes_test), factor(y_test))
print("Detailed Confusion Matrix for Test Set:")
print(cm_test)

# F1 Score for test data
f1_score_test = F1_Score(predicted_classes_test, y_test)
print(paste("F1 Score on test set:", f1_score_test))

