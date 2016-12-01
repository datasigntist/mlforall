# 
#   Created : 30-Nov-2016
#
####Script Part 3.1
# Loading the Data
titanicTrainData = read.csv('https://raw.githubusercontent.com/datasigntist/mlforall/master/datasets/train.csv') 

####Script Part 3.2
# Loading the ggplot2 library
library(ggplot2)

####Script Part 3.3
# Scatter Plot
qplot(data=titanicTrainData,Age,Survived,size=Age)

####Script Part 3.4
# Find the passenger above the age of 80
titanicTrainData[!is.na(titanicTrainData$Age) & titanicTrainData$Age>=80,]

####Script Part 3.5
# Summary of Age
summary(titanicTrainData$Age)

####Script Part 3.6
# Removing the outlier
titanicTrainData[is.na(titanicTrainData$Age),]$Age=0
titanicTrainData = titanicTrainData[titanicTrainData$Age<80,]

####Script Part 3.7
# Extraction of title
library(stringr)
str_split(titanicTrainData[1,4],',')

####Script Part 3.8
class(str_split(titanicTrainData[1,4],','))
tempList = str_split(titanicTrainData[1,4],',')
length(tempList)

tempList[[1]]
tempList[[1]][2]
str_trim(tempList[[1]][2])

####Script Part 3.9
str_split(str_trim(tempList[[1]][2]),'\\.')
str_split(str_trim(tempList[[1]][2]),'\\.')[[1]][1]
tempList = str_split(titanicTrainData[1,4],',')
str_split(str_trim(str_split(titanicTrainData[1,4],',')[[1]][2]),'\\.')[[1]][1]

####Script Part 3.10
# sapply demo and extraction of titles across the passengers
numData = c(2,4,6,8,10,12)
sapply(1:length(numData), function(x) numData[x]*numData[x])

####Script Part 3.11
sapply(1:nrow(titanicTrainData), function(x) str_split(str_trim(str_split(titanicTrainData[x,4],',')[[1]][2]),'\\.')[[1]][1])

####Script Part 3.12
titleList = sapply(1:nrow(titanicTrainData), function(x) str_split(str_trim(str_split(titanicTrainData[x,4],',')[[1]][2]),'\\.')[[1]][1])
titleData = data.frame(t(t(table(titleList))))[,c(1,3)]
qplot(data=titleData,titleList,weight=Freq,geom="bar")

####Script Part 3.13
titanicTrainData$Title = titleList

####Script Part 3.14
# Building the aggregate
aggMeanAgeTitle = aggregate(titanicTrainData[titanicTrainData$Age!=0,]$Age,by=list(titanicTrainData[titanicTrainData$Age!=0,]$Title),FUN="mean")
colnames(aggMeanAgeTitle) = c('Title','Mean_Age')
aggMeanAgeTitle

####Script Part 3.15
# Merging by Title
titanicTrainData = merge(titanicTrainData,aggMeanAgeTitle,on="Title")
titanicTrainData = titanicTrainData[order(titanicTrainData$PassengerId),]
head(titanicTrainData)

####Script Part 3.16
head(titanicTrainData[titanicTrainData$Age==0,c("PassengerId","Age","Mean_Age")])

####Script Part 3.17
# Replacing the Age with Mean_Age
titanicTrainData[titanicTrainData$Age==0,]$Age=titanicTrainData[titanicTrainData$Age==0,]$Mean_Age

####Script Part 3.18
titanicTrainData$Title = as.factor(titanicTrainData$Title)
titanicTrainData$Pclass = as.factor(titanicTrainData$Pclass)
titanicTrainData$Survived = as.factor(titanicTrainData$Survived)
titanicTrainData[titanicTrainData$Embarked=="",]$Embarked="S"

####Script Part 3.19
# Sample and seed demo
sample(1:10,size=0.5*10)
sample(1:10,size=0.5*10)

set.seed(1234)
sample(1:10,size=0.5*10)
set.seed(1234)
sample(1:10,size=0.5*10)

####Script Part 3.20
# Stratified Sampling
library(caTools)
titanicTrainData_Subset = titanicTrainData[, c("PassengerId","Title","Pclass","Sex","Age","Embarked","Survived","SibSp","Parch")]
set.seed(1234)
trainingDataSetRows = sample.split(titanicTrainData_Subset$Sex, SplitRatio=0.8)
# Create the training and validate dataset
titanicTrainData_Subset_Training = titanicTrainData_Subset[trainingDataSetRows,]
titanicTrainData_Subset_Validate = titanicTrainData_Subset[!trainingDataSetRows,]

####Script Part 3.21
# Building the predictive model
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
firstTree = rpart(Survived ~  Title+Pclass+Sex+Age+Embarked+SibSp+Parch, data = titanicTrainData_Subset_Training,method = "class")

# Apply the predict function
predictedResult = predict(firstTree,newdata  = titanicTrainData_Subset_Validate[,c("Title","Pclass","Sex","Age","Embarked","SibSp","Parch")],type="class")

# Compare the Predicted vs Actual
table(titanicTrainData_Subset_Validate$Survived,predictedResult)