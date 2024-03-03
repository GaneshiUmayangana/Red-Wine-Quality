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

################
#Local Outlying Factor(LOF)
#install.packages("dbscan")
#install.packages("fpc")
library(dplyr)
library(fpc)
library(dbscan)
predictors <- select(train, -quality)
response <-train$quality

##Optimal value for k
k <- seq(2,20, by = 1)
for (k in 2:20){
  lof_scores <- lof(predictors, k)
  outliers <- which(lof_scores > 2)
  print(outliers)
  
}

# Compute LOF scores
lof_scores <- lof(predictors, k=10)
outliers <- which(lof_scores > 2)
outliers
plot(lof_scores)

###########################################################

class_balance <- table(train$quality) / length(train$quality)
# Assign a weight to each observation based on its class
weights <- ifelse(train$quality == 1, 1 / class_balance[1],
                  ifelse(train$quality == 2, 1 / class_balance[2], 1 / class_balance[3]))
################Multinomial Logistic Regression with Best Subset Selection#################
model <- multinom(quality ~volatile.acidity + residual.sugar + chlorides+ pH+ sulphates +alcohol+total.sulfur.dioxide ,weights=weights,data =train, maxit = 1000) 
model

# Get residuals
residuals <- residuals(model, type = "deviance")

# Plot residuals
hist(residuals, breaks = 20)

# Boxplot of residuals
a=boxplot(residuals)
a$out


#####################################################################
# Calculate Mahalanobis distance
mah <- mahalanobis(mydata[-12], 
                   center = colMeans(mydata[-12]), 
                   cov = cov(mydata[-12]))


cutoff <- qchisq(p = 0.95 , df = ncol(mydata))
cutoff
# Plot Mahalanobis distance
plot(mah, type = "h")
abline(h = cutoff, col = "#b11226") # add a cutoff line
sum(mah>cutoff)
