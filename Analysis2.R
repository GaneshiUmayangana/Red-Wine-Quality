# Install and load the packages
#install.packages("randomForest")
library(randomForest)
library(dplyr)
#install.packages("ROSE")
library(ROSE)
#install.packages("smotefamily")
library(smotefamily)
library(caret)
library(class)
library(e1071)
library(MLmetrics)
library(ISLR)
library(glmnet)
library(pls)
library(leaps)

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



# You can see there Normal quality Category has high percentage than other 2 categories.
#Therefore the resampling categories are inbalanced data.
# Using sampling techniques and remedy to balance the dataset.

########################

#UP sampling
#train_up <- upSample(x = train %>% select(-quality),
                     #y = train$quality,
                     #yname = "quality") 

#prop.table(table(train_up$quality))

#smote
train_smote <- SMOTE(X =  train[,-12], 
                     target = train[,12], 
                     dup_size = 1)
train_smote<- train_smote$data # extract only the balanced dataset
train_smote$class <- as.factor(train_smote$class)

prop.table(table(train_smote$class))



###################################
#loading the required libraries
library(ggplot2)
library(caret)
library(scales)
library(vip)
library(nnet)
library(e1071)
library(caTools)
library(MLmetrics)
library(car)
library(dplyr)

###Multinomial Logistic Regression All Explanatory Variables #################

# Fit the multinomial logistic regression model
mlogitModel <- multinom(quality ~. ,data =train, maxit = 1000) 
mlogitModel

# Calculate VIF values

labels = rownames(coefficients(mlogitModel))
ref = setdiff(mlogitModel$lab,labels)
t(sapply(labels,function(i){
  train$quality=as.numeric(train$quality==i)
  vif(glm(quality ~ .,data=train,family="binomial"))
}))

##################Predictions on training data###############################
predictedML1 <- predict(mlogitModel,train,na.action =na.pass, type="probs")
predicted_classML1 <- predict(mlogitModel,train)

s=confusionMatrix(as.factor(predicted_classML1),as.factor(train$quality),mode="everything")
s

ggplotConfusionMatrix <- function(m){
  
  # Define wine colors
  wine_colors <- c("#FC9272", "#EF3B2C", "#b11226")
  mytitle <- paste("Accuracy", percent_format()(m$overall[1]),
                   "Kappa", percent_format()(m$overall[2]))
  p <-
    ggplot(data = as.data.frame(m$table) ,
           aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = wine_colors[1], high = wine_colors[3]) +
    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
    theme(legend.position = "none") +
    ggtitle(mytitle)
  return(p)
}
ggplotConfusionMatrix(s)

#F score
F1_Score(predicted_classML1,train$quality)


###########################Predictions on test data###########################
pred_test <- predict(mlogitModel,test,na.action =na.pass, type="probs")
pred_class_test <- predict(mlogitModel,test)

s1=confusionMatrix(as.factor(pred_class_test),as.factor(test$quality),mode="everything")
s1

ggplotConfusionMatrix(s1)


#F1 score
F1_Score(pred_class_test,test$quality)

library(caret)
confusionMatrix(pred_class_test,test$quality,mode="everything")

################################################################################
#            Best Subset Selection                                             #
################################################################################
regfit.full<-regsubsets(quality~.,data=train)
summary(regfit.full)
#It gives by default best-subsets up to size 7; 
#lets increase that to 19, i.e. all the variables
regfit.full<-regsubsets(quality~.,data=train, nvmax=11)
regfull.summary<-summary(regfit.full)
summary(regfit.full)
names(regfull.summary)


plot(regfull.summary$cp,xlab="Number of Variables",ylab="Cp")
which.min(regfull.summary$cp)
points(5,regfull.summary$cp[5],pch=20,col="red")

plot(regfull.summary$adjr2,xlab="Number of Variables",ylab="adjr2")
which.max(regfull.summary$adjr2)
points(5,regfull.summary$adjr2[5],pch=20,col="red")


# There is a plot method for regsubset
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="adjr2")

# coefficents of the selected models
coef(regfit.full,5)


################Multinomial Logistic Regression with Best Subset Selection#################

mlogitModel2 <- multinom(quality~volatile.acidity  + chlorides + pH+ sulphates +alcohol  ,data =train, maxit = 1000) 
mlogitModel2

# Calculate VIF values

labels2 = rownames(coefficients(mlogitModel2))
ref = setdiff(mlogitModel2$lab,labels2)
t(sapply(labels2,function(i){
  train$quality=as.numeric(train$quality==i)
  vif(glm(quality~volatile.acidity  + chlorides + pH+ sulphates +alcohol,data=train,family="binomial"))
}))

##################Predictions on training data###############################
predictedML2 <- predict(mlogitModel2,train,na.action =na.pass, type="probs")
predicted_classML2<- predict(mlogitModel2,train)

s2=confusionMatrix(as.factor(predicted_classML2),as.factor(train$quality),mode="everything")
s2


ggplotConfusionMatrix(s2)

#F1 score
F1_Score(predicted_classML1,train$quality)


###########################Predictions on test data###########################
pred_test <- predict(mlogitModel2,test,na.action =na.pass, type="probs")
pred_class_test <- predict(mlogitModel2,test)

s3=confusionMatrix(as.factor(pred_class_test),as.factor(test$quality),mode="everything")
s3

ggplotConfusionMatrix(s3)


#F1 score
F1_Score(pred_class_test,test$quality)

library(caret)
confusionMatrix(pred_class_test,test$quality,mode="everything")



################Multinomial Logistic Regression with Smote#################
# Fit the multinomial logistic regression model
mlogitModel3 <- multinom(class~. ,data =train_smote , maxit = 1000) 
mlogitModel3

# Calculate VIF values

labels = rownames(coefficients(mlogitModel3))
ref = setdiff(mlogitModel$lab,labels)
t(sapply(labels,function(i){
  train_smote $class=as.numeric(train_smote$class==i)
  vif(glm(class ~ .,data=train_smote ,family="binomial"))
}))


##################Predictions on training data###############################
predictedML1 <- predict(mlogitModel3,train,na.action =na.pass, type="probs")
predicted_classML1 <- predict(mlogitModel3,train)

s=confusionMatrix(as.factor(predicted_classML1),as.factor(train$quality),mode="everything")
s


#F score
F1_Score(predicted_classML1,train$quality)


###########################Predictions on test data###########################
pred_test <- predict(mlogitModel3,test,na.action =na.pass, type="probs")
pred_class_test <- predict(mlogitModel3,test)

s1=confusionMatrix(as.factor(pred_class_test),as.factor(test$quality),mode="everything")
s1

ggplotConfusionMatrix(s1)


#F1 score
F1_Score(pred_class_test,test$quality)

library(caret)
confusionMatrix(pred_class_test,test$quality,mode="everything")
