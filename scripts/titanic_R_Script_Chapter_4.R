# 
#   Created : 30-Nov-2016
#
#####################################################
# Loading the Data
titanicTrainData = read.csv('https://raw.githubusercontent.com/datasigntist/mlforall/master/datasets/train.csv') 
#####################################################

#####################################################
# Calculating the entropy of the superset
entropyValue = 
  -sum(
    sapply(1:length(table(titanicTrainData$Survived)), 
           function(x) 
             table(titanicTrainData$Survived)[x]/nrow(titanicTrainData)*
             log2(table(titanicTrainData$Survived)[x]/nrow(titanicTrainData))))
paste('Entropy : ',as.character(entropyValue))
#####################################################

#####################################################
# Calculating the gini index of the superset
giniIndexValue = 
  1-sum(
    sapply(1:length(table(titanicTrainData$Survived)), 
           function(x) 
             (table(titanicTrainData$Survived)[x]/nrow(titanicTrainData))^2))
paste("Gini Index : ",as.character(giniIndexValue))
#####################################################

#####################################################
# Calculating the classification error of the superset
classifErrorValue = 
  1-max(
    sapply(1:length(table(titanicTrainData$Survived)), 
           function(x) 
             (table(titanicTrainData$Survived)[x]/nrow(titanicTrainData))))
paste("Classification Error : ",as.character(classifErrorValue))
#####################################################

#####################################################
# Get the entropy value of Sex and Survived which is used for calculating the information gain
for(rowNum in 
    1:nrow(table(titanicTrainData$Sex,titanicTrainData$Survived)))
{
  print(
    -sum(sapply(1:length(table(titanicTrainData$Sex,
                               titanicTrainData$Survived)[rowNum,]), 
                function(x) 
                  table(titanicTrainData$Sex,titanicTrainData$Survived)[rowNum,][x]/
                  sum(table(titanicTrainData$Sex,titanicTrainData$Survived)[rowNum,])*
                  log2(table(titanicTrainData$Sex,titanicTrainData$Survived)[rowNum,][x]/
                         sum(table(titanicTrainData$Sex,titanicTrainData$Survived)[rowNum,])))))
}
#####################################################

#####################################################
# Get the entropy value of Pclass and Survived which is used for calculating the information gain
for(rowNum in 
    1:nrow(table(titanicTrainData$Pclass,titanicTrainData$Survived)))
{
  print(
    -sum(
      sapply(1:length(table(titanicTrainData$Pclass,
                            titanicTrainData$Survived)[rowNum,]), 
             function(x) 
               table(titanicTrainData$Pclass,titanicTrainData$Survived)[rowNum,][x]/
               sum(table(titanicTrainData$Pclass,titanicTrainData$Survived)[rowNum,])*
               log2(table(titanicTrainData$Pclass,titanicTrainData$Survived)[rowNum,][x]/
                      sum(table(titanicTrainData$Pclass,titanicTrainData$Survived)[rowNum,])))))
}
#####################################################
