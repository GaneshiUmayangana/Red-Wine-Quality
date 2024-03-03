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
#install.packages("yardstick")
#install.packages("dbscan")
#install.packages("MLmetrics")
#install.packages("RColorBrewer")
#install.packages("pROC")

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
library(yardstick)
library(dbscan)
library(MLmetrics)
library(RColorBrewer)
library(pROC)

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
(table(data_set$quality))

#quality_chr = ifelse(data_set$quality == 1, "Poor",
                     #ifelse(data_set$quality == 2, "Good",
                            #ifelse(data_set$quality == 3, "Excellent", NA)))

#data_set = cbind(data_set, quality_chr )
#view(data_set)

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
view(train_data)

############## outlier analysis ################

########## DBSCAN ###################
eps = 0.5
minPts = 5

# Apply DBSCAN

result = dbscan(train_data[-12], eps = eps, MinPts = minPts)
outliers_db <- train_data[result$cluster == -1, ]
view(outliers_db)
count_outliers <- nrow(outliers_db)
print(count_outliers)

###################### common outliers #################
detect_common_outliers <- function(data) {
  outliers_list <- list()  # Create a list to store outliers for each column
  
  # Iterate over each column in the dataset
  for (i in 1:ncol(data)) {
    if (is.numeric(data[[i]])) {  # Check if the column is numeric
      Q1 <- quantile(data[[i]], 0.25)  # Calculate the first quartile (25th percentile)
      Q3 <- quantile(data[[i]], 0.75)  # Calculate the third quartile (75th percentile)
      IQR <- Q3 - Q1  # Calculate the interquartile range (IQR)
      threshold <- 1.5  # Set the threshold for outlier detection (1.5 times the IQR)
      
      # Identify outliers using the IQR method and store their indices
      outliers_list[[i]] <- which(data[[i]] < (Q1 - threshold * IQR) | data[[i]] > (Q3 + threshold * IQR))
    }
  }
  
  # Find the common outliers across all columns
  common_outliers <- Reduce(intersect, outliers_list)
  
  return(common_outliers)
}

(outliers_com = detect_common_outliers(train_data))
length(outliers_com)
(length(data_set)/length(outliers_com))*100  #outlier percentage 

#for each predictor###############

fit = polr(quality ~ ., data = train_data)

# Get the predictor variables
predictor_variables = names(train_data)[!names(train_data) %in% "quality"]  # Exclude the response variable

par(mar=c(1, 1, 1, 1))  # Adjust the margin values as needed

# Create boxplots for each predictor variable
for (predictor in predictor_variables) {
  boxplot(train_data[[predictor]], main = predictor, xlab = predictor, ylab = "Value")
}

####################################################

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
outliers = detect_outliers(train_data)
length(outliers)
(length(data_set)/length(outliers))*100  #outlier percentage 
#train_data
# Detect outliers in trasnformed dataset ######################

### use log10 transformation to skewd prdictors and create a new data set ###############
transformed_dataset <- train_data %>%
  mutate(
    `citric acid` = log10(`citric acid`),
    `residual sugar` = log10(`residual sugar`),
    chlorides = log10(chlorides),
    `free sulfur dioxide` = log10(`free sulfur dioxide`),
    `total sulfur dioxide` = log10(`total sulfur dioxide`),
    alcohol = log10(alcohol)
  )
view(transformed_dataset)

outliers_tr = detect_outliers(transformed_dataset)
length(outliers_tr )
(length(transformed_dataset)/length(outliers_tr ))*100  #outlier percentage 

### use sqrt transformation to skewd prdictors and create a new data set ###############
transformed_dataset2 = train_data %>%
  mutate(
    `citric acid` = sqrt(`citric acid`),
    `residual sugar` = sqrt(`residual sugar`),
    chlorides = sqrt(chlorides),
    `free sulfur dioxide` = sqrt(`free sulfur dioxide`),
    `total sulfur dioxide` = sqrt(`total sulfur dioxide`),
    alcohol = sqrt(alcohol)
  )

view(transformed_dataset2)
outliers_tr2 = detect_outliers(transformed_dataset2)
length(outliers_tr2 )
(length(transformed_dataset2)/length(outliers_tr2 ))*100  #outlier percentage 

############## summary of the outliers ##############################
outliers_df = data_set[outliers, ]
summary(outliers_df)


################ Ordinal Logistic Regression with all variable#################

ologitModel_all = polr(quality ~.,data =train_data)
ologitModel_all

options(max.print = 10000)
pred_all_tr  = predict(ologitModel_all,newdata = train_data,type = "class")
(length(pred_all_tr))
(conf_matrix_all_tr = confusionMatrix(pred_all_tr, train_data$quality,mode = 'everything'))

options(max.print = 10000)
pred_all_ts = predict(ologitModel_all ,newdata =test_data,type = "class")
(length(pred_all_ts))
(conf_matrix_all_ts = confusionMatrix(pred_all_ts,test_data$quality,mode = 'everything'))

F1_Score(pred_all_ts,test_data$quality)

######## best subset selection method ##############

(table(train_data$quality))

current_names = names(train_data)
new_names = gsub(" ", "_", current_names)
names(train_data) = new_names

current_names = names(test_data)
new_names = gsub(" ", "_", current_names)
names(test_data) = new_names

best_model = regsubsets(quality ~.,data = train_data, nvmax = 11)
best_model.summary = summary(best_model)
which.max(best_model.summary$adjr2)
par(mar = c(3, 1, 1, 1) + 0.1)
plot(best_model.summary$adjr2,xlab="Number of Variables",ylab="adjr2")
points(5,best_model.summary$adjr2[5],pch=20,col="red")
coef(best_model,5)

################ Ordinal Logistic Regression with Best Subset Selection#################
ologitModel = polr(quality ~ volatile.acidity + chlorides +  pH + sulphates + alcohol,data =train_data)
ologitModel

##################Predictions on training data###############################
options(max.print = 10000)
pred1  = predict(ologitModel,newdata = train_data,type = "class")
(length(pred1))
(conf_matrix = confusionMatrix(pred1, train_data$quality,mode = 'everything'))


##################Predictions on test data###############################
options(max.print = 10000)
pred2  = predict(ologitModel,newdata =test_data,type = "class")
(length(pred2))
(conf_matrix = confusionMatrix(pred2,test_data$quality,mode = 'everything'))

F1_Score(pred2 ,test_data$quality)

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
points(10,best_model_smote.summary$adjr2[10],pch=20,col="red")
coef(best_model_smote,10)

ologitModel1 = polr(class ~ fixed.acidity+volatile.acidity + citric.acid +residual.sugar + chlorides +total.sulfur.dioxide+ density + pH + sulphates + alcohol ,data =train_smote) 
ologitModel1

pred3  = predict(ologitModel1,newdata = train_smote,type = "class")
(length(pred3))
(conf_matrix = confusionMatrix(pred3, train_smote$class,mode = 'everything'))

pred4  = predict(ologitModel1,newdata =test_data,type = "class")
(length(pred4))
(conf_matrix = confusionMatrix(pred4,test_data$quality,mode = 'everything'))
F1_Score(pred4 ,test_data$quality)
############################################################## up-sample model ###########################################################
set.seed(100)
best_model_up = regsubsets(quality~.,data = train_up, nvmax = 11)
best_model_up.summary = summary(best_model_up)
which.max(best_model_up.summary$adjr2)
par(mar = c(3, 3, 2, 2) + 0.05)
plot(best_model_up.summary$adjr2,xlab="Number of Variables",ylab="adjr2")
points(10,best_model_smote.summary$adjr2[10],pch=20,col="red")
coef(best_model_up,10)

ologitModel2 = polr(quality ~ fixed_acidity + volatile_acidity +  citric_acid +residual_sugar +chlorides + total_sulfur_dioxide+density + pH + sulphates + alcohol ,data =train_up) 
ologitModel2

pred5  = predict(ologitModel2,newdata = train_up,type = "class")
(length(pred5))
(conf_matrix = confusionMatrix(pred5, train_up$quality,mode = 'everything'))

pred6  = predict(ologitModel2,newdata =test_data,type = "class")
(length(pred6))
(conf_matrix = confusionMatrix(pred6,test_data$quality,mode = 'everything'))
F1_Score(pred6 ,test_data$quality)
############################################################## down-sample model ###########################################################
set.seed(100)
best_model_down = regsubsets(quality~.,data = train_down, nvmax = 11)
best_model_down.summary = summary(best_model_down)
which.max(best_model_down.summary$adjr2)
par(mar = c(3, 3, 2, 2) + 0.05)
plot(best_model_up.summary$adjr2,xlab="Number of Variables",ylab="adjr2")
points(8,best_model_smote.summary$adjr2[8],pch=20,col="red")
coef(best_model_down,8)

ologitModel3 = polr(quality ~ fixed_acidity+volatile_acidity + residual_sugar +chlorides +density + pH + sulphates + alcohol ,data =train_down) 
ologitModel3

pred7  = predict(ologitModel3,newdata = train_down,type = "class")
(length(pred7))
(conf_matrix = confusionMatrix(pred7, train_down$quality,mode = 'everything'))

pred8  = predict(ologitModel3,newdata =test_data,type = "class")
(length(pred8))
(conf_matrix = confusionMatrix(pred8,test_data$quality,mode = 'everything'))
F1_Score(pred8 ,test_data$quality)
############################################################## weighted-sample model ##############################################

class_freq = c(48, 906, 144)  # Number of observations for each quality level
weights = 1 / class_freq
#weights = c(20.5,1,6.3)
train_data$weights = weights[train_data$quality]

best_model_weight = regsubsets(quality ~.-weights,data = train_data,weights =train_data$weights,  nvmax = 11)
best_model_weight.summary = summary(best_model_weight)
which.max(best_model_weight.summary$adjr2)
par(mar = c(3, 1, 1, 1) + 0.1)
plot(best_model_weight.summary$adjr2,xlab="Number of Variables",ylab="adjr2")
points(9,best_model_weight.summary$adjr2[9],pch=20,col="red")
coef(best_model_weight,9)

ologitModel_weight = polr(quality ~ volatile_acidity +citric_acid +  residual_sugar+ chlorides+ total_sulfur_dioxide + density   + pH + sulphates + alcohol,weights =train_data$weights, data =train_data)
ologitModel_weight 

##### so the best model in ologitmodel which didnt do to the original data set smote,up or down sampling 
pred9  = predict(ologitModel_weight ,newdata = train_data,type = "class")
(length(pred9))
(conf_matrix = confusionMatrix(pred9, train_data$quality,mode = 'everything'))

##################Predictions on test data###############################
options(max.print = 10000)
pred10  = predict(ologitModel_weight ,newdata =test_data,type = "class")
(length(pred10))
(conf_matrix = confusionMatrix(pred10,test_data$quality,mode = 'everything'))
F1_Score(pred10 ,test_data$quality)

############# random forest #####################

set.seed(100)
#install.packages('randomForest')
library(randomForest)
train_random_forest <- function(ntrees, train_data, test_data) {
  rf <- randomForest(class ~ ., data = train_data, mtry = 4, ntree = ntrees, importance = TRUE)
  plot(rf, main = c("Error Rate", "Variable Importance", "Proximity", "Partial Dependence"))
  predictions <- predict(rf, test_data[, 1:11])
  conf_matrix <- confusionMatrix(data = predictions, reference = test_data$quality)
  return(list(model = rf, predictions = predictions, confusion_matrix = conf_matrix))
}

# List to store results
results <- list()

# List to store OOB errors for each number of trees
oob_errors <- numeric(length(results))

# Numbers of trees to try
ntrees_list <- c(501, 1001, 1501, 2001)

# Train random forests for each number of trees
for (i in 1:length(ntrees_list)) {
  cat("Training random forest with", ntrees_list[i], "trees\n")
  result <- train_random_forest(ntrees_list[i], train_smote, test_data)
  results[[as.character(ntrees_list[i])]] <- result
  
  # Store OOB error rate
  oob_errors[i] <- tail(result$model$err.rate[, "OOB"], 1)
}

# Plot error rates for each number of trees
plot(ntrees_list, oob_errors, type = "b", 
     xlab = "Number of Trees", ylab = "Out-of-Bag Error Rate",
     main = "Out-of-Bag Error Rate vs. Number of Trees")

# Print confusion matrices
for (i in 1:length(ntrees_list)) {
  cat("Confusion matrix for", ntrees_list[i], "trees:\n")
  print(results[[as.character(ntrees_list[i])]]$confusion_matrix)
}

# Variable importance plot
par(mfrow = c(2, 2))  # Set up a grid for multiple plots
for (i in 1:length(ntrees_list)) {
  cat("Variable importance plot for", ntrees_list[i], "trees\n")
  varImpPlot(results[[as.character(ntrees_list[i])]]$model, main = paste("Variable Importance (", ntrees_list[i], "Trees)"))
}


############# KNN #####################

set.seed(100)
#install.packages("class")
library(class)
data_norm = function(x){(x-min(x))/(max(x)-min(x))}

smote_set_norm =as.data.frame(lapply(train_smote[,-12],data_norm))
train_set_norm =as.data.frame(lapply(train_data[,-12],data_norm))
test_set_norm =as.data.frame(lapply(test_data[,-12],data_norm))
#summary(smote_set_norm)
#summary(test_set_norm)
train_labels = train_smote$class

#############
# Install and load necessary libraries
# install.packages("class")
# install.packages("caret")
library(class)
library(caret)

# Assuming you have your data in train_set_norm and train_labels

# Create a training control object for k-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Set up the parameter grid (range of k values to explore)
grid <- expand.grid(k = seq(1, 20, by = 2))  # You can adjust the sequence as needed

# Train the KNN model using cross-validation
knn_model <- train(
  x = smote_set_norm[-12],
  y = train_labels,
  method = "knn",
  tuneGrid = grid,
  trControl = ctrl
)

# Display the results
print(knn_model)

# Extract the optimal k value
optimal_k <- knn_model$bestTune$k
cat("Optimal k value:", optimal_k, "\n")


###############train
pred_knn = knn(smote_set_norm[-12],train_set_norm[-12],cl=train_labels,k=5)
conf_matrix_knn = table(Actual = train_data$quality, Predicted = pred_knn)
print(conf_matrix_knn)
(accuracy = (sum(diag(conf_matrix_knn)) / sum(conf_matrix_knn))*100)

testS1=confusionMatrix(pred_knn,train_data$quality,mode = "everything")
testS1
#F score
F1_Score(pred_knn ,train_data$quality)

#########test
pred_knn = knn(smote_set_norm[-12],test_set_norm[-12],cl=train_labels,k=5)
conf_matrix_knn = table(Actual = test_data$quality, Predicted = pred_knn)
print(conf_matrix_knn)
(accuracy = (sum(diag(conf_matrix_knn)) / sum(conf_matrix_knn))*100)

testS=confusionMatrix(pred_knn,test$quality,mode = "everything")
testS
F1_Score(pred_knn ,test$quality)
############################################################################

############### misclassification plot #################################
test_data$predicted_class = pred_knn
test_data$misclassified = ifelse(test_data$quality != test_data$predicted_class, "Misclassified", "Correct")

ggplot(data = test_data, aes(x = quality, y = predicted_class, color = misclassified)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("blue", "red"), labels = c("Correct", "Misclassified")) +
  labs(title = "Misclassification Plot", color = "Misclassified") +
  theme_minimal()

#####confussion matrix plot

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
ggplotConfusionMatrix(testS)