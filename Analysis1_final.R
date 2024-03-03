#getwd()
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

str(my_data1)
#reprocessing the quality variable as a categorical, using factor.
my_data1$quality = as.factor(my_data1$quality)
a=table(my_data1$quality)
a
my_data= my_data1
levels(my_data$quality)=c("Poor","Poor","Normal","Normal","Excellent","Excellent")
b=table(my_data$quality)
b
str(my_data)

# Calculate percentages
quality_percentages <- prop.table(b) * 100

# Display the percentages
print(quality_percentages)

##
summary(my_data$sulphates[my_data$quality=="Excellent"])
summary(my_data$free.sulfur.dioxide[my_data$quality=="Excellent"])
summary(my_data$total.sulfur.dioxide[my_data$quality=="Excellent"])

# Check the data types of each column
sapply(my_data, class)


# install.packages("ggplot2")
library(ggplot2)

# Create a histogram of the "quality" variable
ggplot(my_data1, aes(x = quality)) +
  geom_bar(fill = "#b11226", color = "black", position = "dodge") +
  theme_minimal() +
  labs(title = "Histogram of Quality Variable",
       x = "Quality",
       y = "Count")

ggplot(my_data, aes(x = quality)) +
  geom_bar(fill = "#b11226", color = "black", position = "dodge") +
  theme_minimal() +
  labs(title = "Histogram of Quality Variable",
       x = "Quality",
       y = "Count")


# install.packages(c("ggplot2", "dplyr"))
library(ggplot2)
library(dplyr)

# Select only numeric variables
numeric_data <- my_data %>%
  select_if(is.numeric)

# Calculate Spearman's correlation coefficients
cor_matrix <- cor(numeric_data, method = "spearman")

# Reshape the correlation matrix for plotting
cor_long <- as.data.frame(as.table(cor_matrix))
colnames(cor_long) <- c("Variable1", "Variable2", "Correlation")

# Create a heatmap using ggplot2 with correlation values annotated
ggplot(cor_long, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), vjust = 1) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Spearman's Correlation Heatmap",
       x = "Variable",
       y = "Variable")

# Calculate point-biserial correlation between "quality" and each explonatory variables

# Install and load the knitr package
# install.packages("knitr")
library(knitr)

# Your existing code
r <- character(0)
cor <- numeric(0)

for (i in 1:(ncol(my_data)-1)) {
  r[i] <- names(my_data)[i]
  cor[i] <- cor(as.numeric(my_data$quality), my_data[[i]], method = "spearman")
}

# Create a data frame from vectors r and cor
result_table <- data.frame(Variable = r, Correlation = cor)

# Print the result as a formatted table
kable(result_table, format = "markdown")



###############Histograms of the explanatory variable


ggplot(my_data, aes(x = fixed.acidity)) +
  geom_histogram(fill = "#b11226", color = "black", bins = 30) +
  theme_minimal() +
  labs(title = "Distribution of Fixed Acidity",
       x = "Fixed Acidity (g/dm^3)",
       y = "Count")

ggplot(my_data, aes(x = volatile.acidity)) +
  geom_histogram(fill = "#b11226", color = "black", bins = 30) +
  theme_minimal() +
  labs(title = "Distribution of Volatile Acidity",
       x = "Volatile Acidity (g/dm^3)",
       y = "Count")

ggplot(my_data, aes(x = citric.acid)) +
  geom_histogram(fill = "#b11226", color = "black", bins = 30) +
  theme_minimal() +
  labs(title = "Distribution of Citric Acid",
       x = "Citric Acid (g/dm^3)",
       y = "Count")
ggplot(my_data, aes(x = sulphates)) +
  geom_bar(fill = "#b11226", color = "black", position = "dodge") +
  theme_minimal() +
  labs(title = "Distribution of Sulphates",
       x = "sulphates  (g/dm^3)",
       y = "Count")

ggplot(my_data, aes(x = free.sulfur.dioxide )) +
  geom_histogram(fill = "#b11226", color = "black", position = "dodge") +
  theme_minimal() +
  labs(title = "Distribution of Free Sulfur Dioxide",
       x = "Free Sulfur Dioxide (g/dm^3)",
       y = "Count")


ggplot(my_data, aes(x = total.sulfur.dioxide)) +
  geom_histogram(fill = "#b11226", color = "black", bins = 30) +
  theme_minimal() +
  labs(title = "Distribution of Total Sulfur Dioxide",
       x = "Total Sulfur Dioxide (g/dm^3)",
       y = "Count")

ggplot(my_data, aes(x = pH)) +
  geom_histogram(fill = "#b11226", color = "black", bins = 30) +
  theme_minimal() +
  labs(title = "Distribution of pH",
       x = "pH ",
       y = "Count")

ggplot(my_data, aes(x = density )) +
  geom_histogram(fill = "#b11226", color = "black", bins = 30) +
  theme_minimal() +
  labs(title = "Distribution of Density ",
       x = "Density",
       y = "Count")

ggplot(my_data, aes(x = residual.sugar)) +
  geom_histogram(fill = "#b11226", color = "black", bins = 30) +
  theme_minimal() +
  labs(title = "Distribution of Residual Sugar ",
       x = "Residual Sugar (g/dm^3)",
       y = "Count")

ggplot(my_data, aes(x = chlorides)) +
  geom_histogram(fill = "#b11226", color = "black", bins = 30) +
  theme_minimal() +
  labs(title = "Distribution of Chlorides ",
       x = "Chlorides (g/dm^3)",
       y = "Count")

ggplot(my_data, aes(x = alcohol)) +
  geom_histogram(fill = "#b11226", color = "black", bins = 30) +
  theme_minimal() +
  labs(title = "Distribution of Alcohol ",
       x = "Alcohol (g/dm^3)",
       y = "Count")



########Boxplots############

ggplot(my_data, aes(x = quality, y = fixed.acidity, fill = quality)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Poor" = "#FC9272", "Normal" = "#EF3B2C", "Excellent" = "#A50F15")) +
  labs(title = "Box Plot of Fixed Acidity by Quality",
       x = "Quality",
       y = "Fixed Acidity")


ggplot(my_data, aes(x = quality, y = volatile.acidity, fill = quality)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Poor" = "#FC9272", "Normal" = "#EF3B2C", "Excellent" = "#A50F15")) +
  labs(title = "Box Plot of Volatile Acidity by Quality Rank",
       x = "Quality",
       y = "Volatile Acidity")

ggplot(my_data, aes(x = quality, y = citric.acid, fill = quality)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Poor" = "#FC9272", "Normal" = "#EF3B2C", "Excellent" = "#A50F15")) +
  labs(title = "Box Plot of Citric Acid by Quality Rank",
       x = "Quality",
       y = "Citric Acid")

ggplot(my_data, aes(x = quality, y = pH, fill = quality)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Poor" = "#FC9272", "Normal" = "#EF3B2C", "Excellent" = "#A50F15")) +
  labs(title = "Box Plot of pH by Quality Rank",
       x = "Quality",
       y = "pH")

ggplot(my_data, aes(x = quality, y =  sulphates, fill = quality)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Poor" = "#FC9272", "Normal" = "#EF3B2C", "Excellent" = "#A50F15")) +
  labs(title = "Box Plot of Sulphates by Quality Rank",
       x = "Quality",
       y = "Sulphates")

ggplot(my_data, aes(x = quality, y =free.sulfur.dioxide, fill = quality)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Poor" = "#FC9272", "Normal" = "#EF3B2C", "Excellent" = "#A50F15")) +
  labs(title = "Box Plot of Free SO2  by Quality Rank",
       x = "Quality",
       y = "Free Sulfur Dioxide")

ggplot(my_data, aes(x = quality, y =total.sulfur.dioxide, fill = quality)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Poor" = "#FC9272", "Normal" = "#EF3B2C", "Excellent" = "#A50F15")) +
  labs(title = "Box Plot of Total SO2  by Quality Rank",
       x = "Quality",
       y = "Total Sulfur Dioxide")

ggplot(my_data, aes(x = quality, y =chlorides, fill = quality)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Poor" = "#FC9272", "Normal" = "#EF3B2C", "Excellent" = "#A50F15")) +
  labs(title = "Box Plot of Chlorides  by Quality Rank",
       x = "Quality",
       y = "Chlorides")

ggplot(my_data, aes(x = quality, y =density, fill = quality)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Poor" = "#FC9272", "Normal" = "#EF3B2C", "Excellent" = "#A50F15")) +
  labs(title = "Box Plot of Density  by Quality Rank",
       x = "Quality",
       y = "Density")

ggplot(my_data, aes(x = quality, y =alcohol, fill = quality)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Poor" = "#FC9272", "Normal" = "#EF3B2C", "Excellent" = "#A50F15")) +
  labs(title = "Box Plot of Alcohol  by Quality Rank",
       x = "Quality",
       y = "Alcohol")

ggplot(my_data, aes(x = quality, y =residual.sugar, fill = quality)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Poor" = "#FC9272", "Normal" = "#EF3B2C", "Excellent" = "#A50F15")) +
  labs(title = "Box Plot of Residual Sugar by Quality Rank",
       x = "Quality",
       y = "Residual Sugar")


########Residual plots#######

ggplot(my_data, aes(x = fixed.acidity, y = citric.acid)) +
  geom_point(color = "#EF3B2C", size = 3) +
  labs(title = "Residual Plot of Fixed Acidity vs Citric Acid",
       x = "Fixed Acidity",
       y = "Citric Acid")

ggplot(my_data, aes(x = volatile.acidity, y = citric.acid)) +
  geom_point(color = "#EF3B2C", size = 3) +
  labs(title = "Residual Plot of Volatile Acidity vs Citric Acid",
       x = "Volatile Acidity",
       y = "Citric Acid")

ggplot(my_data, aes(x = fixed.acidity, y = pH )) +
  geom_point(color = "#EF3B2C", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Residual Plot of Fixed Acidity vs pH",
       x = "Fixed Acidity",
       y = "pH")

ggplot(my_data, aes(x = volatile.acidity, y = pH )) +
  geom_point(color = "#EF3B2C", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Residual Plot of Volatile Acidity vs pH",
       x = "Volatile Acidity",
       y = "pH")

ggplot(my_data, aes(x = citric.acid , y = pH )) +
  geom_point(color = "#EF3B2C", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Residual Plot of Citric Acid  vs pH",
       x = "Citric Acid",
       y = "pH")

ggplot(my_data, aes(x = residual.sugar , y = density )) +
  geom_point(color = "#EF3B2C", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Residual Plot of Residual Sugar  vs Density",
       x = "Residual Sugar",
       y = "Density")

ggplot(my_data, aes(x = alcohol , y = density )) +
  geom_point(color = "#EF3B2C", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Residual Plot of Alcohol  vs Density",
       x = "Alcohol",
       y = "Density")

ggplot(my_data, aes(x = alcohol , y = residual.sugar )) +
  geom_point(color = "#EF3B2C", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Residual Plot of Alcohol  vs Residual Sugar",
       x = "Alcohol",
       y = "Residual Sugar")



########################
#Splitting data set
set.seed(100)
library(caTools)
split <- sample.split(my_data, SplitRatio = 0.8)#80% for training and 20% for testing
split
train <- subset(my_data, split == "TRUE")
test <- subset(my_data, split == "FALSE")

########### observing any patterns in the dataset ##################### 

#PC model
numeric_data = train[, sapply(train, is.numeric)]
pca_result = prcomp(numeric_data, scale. = TRUE)

# Display summary of PCA results
summary(pca_result)
pc_scores = pca_result$x[,1:2]
plot(pc_scores[,1],pc_scores[,2],xlab = "Principal Component 1 (PC1)",ylab = "Principal Component 2 (PC2)",mar = c(4, 4, 2, 2))

#pls model


k=as.numeric(t) 
#k
Quality_Ordinal = ifelse(k<=4,1 ,ifelse(k>4 & k<=6,2,3)) 
dataset_new = cbind(my_data1,Quality_Ordinal)
#view(dataset_new)
dim(dataset_new)

set.seed(1)
split <- sample.split(dataset_new, SplitRatio = 0.8)#80% for training and 20% for testing
split
train1 <- subset(dataset_new, split == "TRUE")
test1 <- subset(dataset_new, split == "FALSE")

xc= train1[,1:11]
yc =train1[,13]
#xc
#yc
dim(xc)

xt= test1[,1:11]
yt =test1[,13]
dim(xt)

yc <- as.numeric(as.character(yc))

# PLS model
library(mdatools)
model <- pls(xc, yc, scale = TRUE, cv = 1, info = "Wine Quality Prediction")

# Print the summary of the PLS model
summary(model)
plot(model)
plotXScores(model,show=1,labels=F)
plotXYLoadings(model,show=1,labels =F)




#Trivariate plot
# Load required libraries

library(mdatools)

# Trivariate Plot Function
trivariate_plot <- function(data, x_var, y_var, z_var, title) {
  ggplot(data, aes_string(x = x_var, y = y_var, color = z_var)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
    labs(title = title, x = x_var, y = y_var) +
    theme_minimal()
}

# Generate and arrange trivariate plots
plot1 <- trivariate_plot(my_data, "alcohol", "density", "quality", "Alcohol, Density, and Quality")
plot2 <- trivariate_plot(my_data, "residual.sugar", "density", "quality", "Residual Sugar, Density, and Quality")
plot3 <- trivariate_plot(my_data, "free.sulfur.dioxide", "total.sulfur.dioxide", "quality", "Free SO2, Total SO2, and Quality")
plot4 <- trivariate_plot(my_data, "volatile.acidity", "citric.acid", "quality", "Volatile Acidity, Citric Acid, and Quality")

plot5 <- trivariate_plot(my_data, "volatile.acidity", "sulphates", "quality", "Volatile Acidity, Sulphates, and Quality")
plot6 <- trivariate_plot(my_data, "volatile.acidity", "alcohol", "quality", "Volatile Acidity, Alcohol, and Quality")
plot7 <- trivariate_plot(my_data, "citric.acid", "sulphates", "quality", "Citric Acid, Sulphates, and Quality")
plot8 <- trivariate_plot(my_data, "citric.acid", "alcohol", "quality", "Citric Acid, Alcohol, and Quality")

# Plot arrangement
library(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4, nrow = 2)
grid.arrange(plot5, plot6, plot7, plot8, nrow = 2)
