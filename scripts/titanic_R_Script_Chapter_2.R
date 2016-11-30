# 
#   Created : 30-Nov-2016
#
# Loading the Data
titanicTrainData = read.csv('https://raw.githubusercontent.com/datasigntist/mlforall/master/datasets/train.csv') 

# Previewing the Data
# Fetch the first 6 rows of the data
head(titanicTrainData)

# Previewing the Data
# Fetch the first 10 rows of the data
head(titanicTrainData,10)

# Fetch the last 6 rows of the data
tail(titanicTrainData)

# Fetch the first 2 rows of last 6 rows the data
head(tail(titanicTrainData),2)

# Dimension of the data
dim(titanicTrainData)
nrow(titanicTrainData)
ncol(titanicTrainData)

# Prints the snapshot of the data
str(titanicTrainData)

# Previewing the Data
# Fetch the first 6 rows of the data
head(titanicTrainData$Name)

# Previewing the Data
# Fetch the first 6 rows of the passengers name
passengersName = head(titanicTrainData$Name)
passengersName[3]
titanicTrainData[3,]
titanicTrainData[3,4]

# Get the distribution of Sex of the passenger
table(titanicTrainData$Sex)
prop.table(table(titanicTrainData$Sex))*100

# Plots the distribution of Passengers by Sex
barplot(prop.table(table(titanicTrainData$Sex))*100)

# Plots the distribution of Age
hist(titanicTrainData$Age)

# Prints the summary of Age
summary(titanicTrainData$Age)

# Creates a subset of the data by selecting PassengerId, Sex, Pclass and Survived
titanicTrainData_Subset = titanicTrainData[, c(1,2,3,12,5)]

# Proportion of passenger class
prop.table(table(titanicTrainData_Subset$Pclass))

# Prints the summary of the dataset
summary(titanicTrainData_Subset)

# Plots the distribution of Passengers by Embarked
barplot(prop.table(table(titanicTrainData_Subset$Embarked))*100)

# Replace the missing values
titanicTrainData_Subset[titanicTrainData_Subset$Embarked=="",]$Embarked = 'S'

# Unique values of Pclass
unique(titanicTrainData_Subset$Pclass)

# Convert Pclass and Survived to factor as they are discrete
titanicTrainData_Subset$Pclass = as.factor(titanicTrainData_Subset$Pclass)
summary(titanicTrainData_Subset)

# sample command
sample(1:20,0.8*20)

# Find Passengers who can be part of the train exercise
trainingDataSetRows = sample(1:nrow(titanicTrainData_Subset),0.8* nrow(titanicTrainData_Subset))

# Create the training and validate dataset
titanicTrainData_Subset_Training = titanicTrainData_Subset[trainingDataSetRows,]
titanicTrainData_Subset_Validate = titanicTrainData_Subset[-trainingDataSetRows,]

# Get the number of rows of Training and Validate
nrow(titanicTrainData_Subset_Training)
nrow(titanicTrainData_Subset_Validate)

# Installing the libraries
install.packages('ggplot2')
install.packages('rpart')
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')

# Loading the libraries
library(ggplot2)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Building a Predictive Model using rpart
firstTree = rpart(Survived ~ Sex + Pclass +Embarked, data = titanicTrainData_Subset_Training,method = "class")

# Plot the decision Tree
fancyRpartPlot(firstTree)

# Get the first 6 passengers of Validate dataset
head(titanicTrainData_Subset_Validate[,c(1,3,4,5,2)])

# Apply the predict function
predictedResult = predict(firstTree,newdata  = titanicTrainData_Subset_Validate[,c(3:5)],type="class")

# Compare the Predicted vs Actual
table(titanicTrainData_Subset_Validate$Survived,predictedResult)