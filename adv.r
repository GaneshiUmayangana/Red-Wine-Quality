##########Advance Analysis###########

######################### Packages and Libraries ############################

#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("caTools")
#install.packages("ggplot2")
#install.packages('pls')
#install.packages('mdtools')
#install.packages("leaps")
#install.packages('caret')
#install.packages("MASS")
#install.packages("smotefamily")
#install.packages("glmnet")
#install.packages("ordinal")
#install.packages("rms")


library(readxl)
library(tidyverse)
library(caTools)
library(dplyr)
library(ggplot2)
library(pls)
library(mdatools)
library(leaps)
library(MASS)
library(caret)
library(smotefamily)
library(ROSE)
library(glmnet)
library(ordinal)
library(rms)



####################### basic overview of the data set ########################
winequality_red <- read.csv("Red_Wine_Data.csv")
dim(winequality_red) # number of observations and number of variables
names(winequality_red) #variable names
attach(winequality_red)
summary(winequality_red)


#checking for missing values
sapply(winequality_red,function(x) sum(is.na(x)))
sum(is.na(winequality_red))
# *******no missing values in the data set******* 

#################### checking duplicate observation ###################################
data_set = winequality_red[!duplicated(winequality_red),]
winequality_red %>%
  distinct()
dim(data_set)  #new data set without duplicates 


data_set$quality = ifelse(data_set$quality<=4,1 , 
                           ifelse(data_set$quality>4 & data_set$quality<=6, 
                                  2,3))
data_set$quality <- as.factor(data_set$quality)
table(data_set$quality)
attach(data_set)
#quality_chr = ifelse(data_set$quality == 1, "Poor",
                     #ifelse(data_set$quality == 2, "Good",
                            #ifelse(data_set$quality == 3, "Excellent", NA)))

#data_set = cbind(data_set, quality_chr )
#view(data_set)

############## outlier analysis ################

detect_outliers = function(data) {
  outliers = numeric()
  for (i in 1:ncol(data)) {
    if (is.numeric(data[[i]])) {  # Check if the column is numeric
      Q1 = quantile(data[[i]], 0.25)
      Q3 = quantile(data[[i]], 0.75)
      IQR = Q3 - Q1
      threshold = 1.5  
      outliers = c(outliers, which(data[[i]] < (Q1 - threshold * IQR) | data[[i]] > (Q3 + threshold * IQR)))
    }
  }
  unique(outliers)
}

# Detect outliers in dataset ##################
outliers = detect_outliers(data_set)
length(outliers)
(length(data_set)/length(outliers))*100  #outlier percentage 

# Detect outliers in trasnformed dataset ######################

### use log10 transformation to skewd prdictors and create a new data set ###############
transformed_dataset <- data_set %>%
  mutate(
    `citric acid` = log10(`citric.acid`),
    `residual sugar` = log10(`residual.sugar`),
    chlorides = log10(chlorides),
    `free sulfur dioxide` = log10(`free.sulfur.dioxide`),
    `total sulfur dioxide` = log10(`total.sulfur.dioxide`),
    alcohol = log10(alcohol)
  )
view(transformed_dataset)

outliers_tr = detect_outliers(transformed_dataset)
length(outliers_tr )
(length(transformed_dataset)/length(outliers_tr ))*100  #outlier percentage 

### use sqrt transformation to skewd prdictors and create a new data set ###############
transformed_dataset2 = data_set %>%
  mutate(
    `citric acid` = sqrt(`citric.acid`),
    `residual sugar` = sqrt(`residual.sugar`),
    chlorides = sqrt(chlorides),
    `free sulfur dioxide` = sqrt(`free.sulfur.dioxide`),
    `total sulfur dioxide` = sqrt(`total.sulfur.dioxide`),
    alcohol = sqrt(alcohol)
  )

view(transformed_dataset2)
outliers_tr2 = detect_outliers(transformed_dataset2)
length(outliers_tr2 )
(length(transformed_dataset2)/length(outliers_tr2 ))*100  #outlier percentage 

############## summary of the outliers ##############################
outliers_df = data_set[outliers, ]
summary(outliers_df)

############### split the data set into tainning and testing ############

set.seed(100)
split = sample(2,nrow(data_set),replace=T, prob=c(0.8,0.2))
train_data = subset(data_set,split ==1)#Training data
test_data = subset(data_set, split ==2)
nrow(train_data)
nrow(test_data)
dim(train_data)
attach(train_data)
dim(test_data)
attach(test_data)

######## best subset selection method ##############

# current_names = names(train_data)
# new_names = gsub(".", "_", current_names)
# names(train_data) = new_names
# 
# current_names = names(test_data)
# new_names = gsub(".", "_", current_names)
# names(test_data) = new_names


best_model = regsubsets(quality ~.,data = train_data, nvmax = 11)
best_model.summary = summary(best_model)
which.max(best_model.summary$adjr2)
par(mar = c(5, 4, 4, 2) + 0.1)
plot(best_model.summary$adjr2,xlab="Number of Variables",ylab="adjr2")
points(8,best_model.summary$adjr2[7],pch=20,col="red")
coef(best_model,8)


################ Ordinal Logistic Regression with Best Subset Selection#################
ologitModel = polr(quality ~ volatile.acidity +citric.acid+ residual.sugar + chlorides + total.sulfur.dioxide + pH + sulphates + alcohol,data =train_data)
ologitModel

##################Predictions on training data###############################
options(max.print = 10000)
pred1  = predict(ologitModel,newdata = train_data,type = "class")
(length(pred1))
(conf_matrix = confusionMatrix(pred1, train_data$quality,mode="everything"))
#F1_Score(pred1,train_data$quality)


##################Predictions on test data###############################
options(max.print = 10000)
pred2  = predict(ologitModel,newdata =test_data,type = "class")
(length(pred2))
(conf_matrix = confusionMatrix(pred2,test_data$quality,mode="everything"))

############ since we got imbalanced data lets check the accuracies by using SMOTE on training data set and re fit the model #################################
(table(train_data$quality)) ##majority of the data set in the level 2 class
#view(train_data)

#UP sampling
train_up <- upSample(x = train_data[, -which(names(train_data) == "quality")],
                     y = train_data$quality,
                     yname = "quality")
prop.table(table(train_up$quality))
(table(train_up$quality))

#Down sampling
train_down <- downSample(x = train_data[, -which(names(train_data) == "quality")],
                     y = train_data$quality,
                     yname = "quality")
prop.table(table(train_down$quality))
(table(train_down$quality))

####################################################### smote model ###########################################################
set.seed(100)
train_smote = SMOTE(X =  train_data[,-12], 
                     target = train_data[,12], 
                     dup_size = 3)
train_smote = train_smote$data # extract only the balanced dataset
view(train_smote)
train_smote$class = as.factor(train_smote$class)
prop.table(table(table(train_smote$class)))

best_model_smote = regsubsets(class~.,data = train_smote, nvmax = 11)
best_model_smote.summary = summary(best_model_smote)
which.max(best_model_smote.summary$adjr2)
par(mar = c(3, 3, 2, 2) + 0.05)
plot(best_model_smote.summary$adjr2,xlab="Number of Variables",ylab="adjr2")
points(8,best_model_smote.summary$adjr2[8],pch=20,col="red")
coef(best_model_smote,8)

ologitModel1 = polr(class ~ fixed_acidity + volatile_acidity +residual_sugar + 
                      chlorides +total_sulfur_dioxide + pH + sulphates + alcohol ,data =train_smote) 
ologitModel1

pred3  = predict(ologitModel1,newdata = train_smote,type = "class")
(length(pred3))
(conf_matrix = confusionMatrix(pred3, train_smote$class))

pred4  = predict(ologitModel1,newdata =test_data,type = "class")
(length(pred4))
(conf_matrix = confusionMatrix(pred4,test_data$quality))

############################################################## up-sample model ###########################################################
set.seed(100)
best_model_up = regsubsets(quality~.,data = train_up, nvmax = 11)
best_model_up.summary = summary(best_model_up)
which.max(best_model_up.summary$adjr2)
par(mar = c(3, 3, 2, 2) + 0.05)
plot(best_model_up.summary$adjr2,xlab="Number of Variables",ylab="adjr2")
points(10,best_model_smote.summary$adjr2[10],pch=20,col="red")
coef(best_model_up,10)

ologitModel2 = polr(quality ~ fixed_acidity + volatile_acidity +  citric_acid +residual_sugar 
                    +chlorides +free_sulfur_dioxide+ total_sulfur_dioxide + pH + sulphates + alcohol ,data =train_up) 
ologitModel2

pred5  = predict(ologitModel2,newdata = train_up,type = "class")
(length(pred5))
(conf_matrix = confusionMatrix(pred5, train_up$quality))

pred6  = predict(ologitModel2,newdata =test_data,type = "class")
(length(pred6))
(conf_matrix = confusionMatrix(pred6,test_data$quality))

############################################################## down-sample model ###########################################################
set.seed(100)
best_model_down = regsubsets(quality~.,data = train_down, nvmax = 11)
best_model_down.summary = summary(best_model_down)
which.max(best_model_down.summary$adjr2)
par(mar = c(3, 3, 2, 2) + 0.05)
plot(best_model_up.summary$adjr2,xlab="Number of Variables",ylab="adjr2")
points(6,best_model_smote.summary$adjr2[6],pch=20,col="red")
coef(best_model_down,6)

ologitModel3 = polr(quality ~ volatile_acidity + residual_sugar +chlorides + pH + sulphates + alcohol ,data =train_down) 
ologitModel3

pred7  = predict(ologitModel3,newdata = train_down,type = "class")
(length(pred7))
(conf_matrix = confusionMatrix(pred7, train_down$quality))

pred8  = predict(ologitModel3,newdata =test_data,type = "class")
(length(pred8))
(conf_matrix = confusionMatrix(pred8,test_data$quality))

##### so the best model in ologitmodel which didnt do to the original data set smote,up or down sampling 
