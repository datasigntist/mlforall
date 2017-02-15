# 
#   Created : 19-Jan-2017
#   Updated : 15-Feb-2017
#
####Script Part 3.1.1
# Set the current working directory
setwd("//Users//vishwanathanraman//Documents//mlforall//datasets//cricinfo")

paramYear = 2014
urlToDownloadTemplate = "http://stats.espncricinfo.com/ci/engine/records/team/match_results.html?class=2;id=paramYear;team=6;type=year"

while (paramYear<=2016)
{
  urlToDownload = gsub("paramYear",paramYear,urlToDownloadTemplate)
  destFileName = paste0(paramYear,"_India_Matches.html")
  if (!file.exists(destFileName)){
    download.file(urlToDownload,destFileName)
  }
  paramYear = paramYear + 1
}
####

####Script Part 3.1.2
library(XML)
library(ggplot2)

cricData = readHTMLTable("2014_India_Matches.html")

cricData
####

####Script Part 3.1.3
cricDataLines = readLines("2014_India_Matches.html")
grep("/ci/engine/match/[0-9]+.html",cricDataLines)
cricDataLines[846]
####

####Script Part 3.1.4
library(stringr)
str_extract(cricDataLines[846],"[0-9]+.html")
####

####Script Part 3.1.5
scoreCardReferences = grep("/ci/engine/match/[0-9]+.html",cricDataLines)
for(loop in 1:length(scoreCardReferences))
{
  print(str_extract(cricDataLines[scoreCardReferences[loop]],"[0-9]+.html"))
}
####

####Script Part 3.1.6
scoreCardLinks = unique(sapply(1:length(scoreCardReferences),
       function(x) str_extract(cricDataLines[scoreCardReferences[x]],"[0-9]+.html")))
cricDataSet = cricData[[1]]
cricDataSet$ScorecardLink = scoreCardLinks
####

####Script Part 3.1.7
paramYear = 2014

while (paramYear<=2016)
{
  fileName = paste0(paramYear,"_India_Matches.html")
  tempcricData = readHTMLTable(fileName)
  tempcricDataLines = readLines(fileName)
  tempscoreCardReferences = grep("/ci/engine/match/[0-9]+.html",tempcricDataLines)
  tempscoreCardLinks = unique(sapply(1:length(tempscoreCardReferences),
                                 function(x) str_extract(tempcricDataLines[tempscoreCardReferences[x]],"[0-9]+.html")))
  tempcricDataSet = tempcricData[[1]]
  tempcricDataSet$ScorecardLink = tempscoreCardLinks
  tempcricDataSet$year = paramYear
  
  if (paramYear==2014)
    cricDataSet = tempcricDataSet
  else
    cricDataSet = rbind(cricDataSet,tempcricDataSet)
  paramYear = paramYear + 1
}
####

####Script Part 3.1.8
tempData = as.data.frame(prop.table(table(cricDataSet$Winner))*100)
colnames(tempData) = c("Country","Wins")
tempData = tempData[tempData$Country!='no result' & tempData$Country!='tied',]
tempData = tempData[order(-tempData$Wins),]
tempData = transform(tempData, Country=reorder(Country, Wins) ) 
qplot(data=tempData,Country,weight=Wins,geom="bar")+coord_flip()
####

####Script Part 3.1.9
cricDataSet$PlayedinIndia = "No"
cricDataSet[cricDataSet$Ground %in% 
              c('Kochi','Delhi','Dharamsala','Cuttack'
                ,'Ahmedabad','Hyderabad (Deccan)','Kolkata','Ranchi',
                'Kanpur','Indore','Rajkot','Chennai','Mumbai',
                'Mohali','Visakhapatnam'),]$PlayedinIndia ="Yes"
tempDatacricDataSet = cricDataSet[cricDataSet$PlayedinIndia=="No",]
tempData = as.data.frame(prop.table(table(tempDatacricDataSet$Winner,tempDatacricDataSet$PlayedinIndia))*100)
colnames(tempData) = c("Country","PlayedinIndia","Wins")
tempData = tempData[tempData$Country!='no result' & tempData$Country!='tied',]
tempData = tempData[order(-tempData$Wins),]
tempData = transform(tempData, Country=reorder(Country, Wins) ) 
qplot(data=tempData,Country,weight=Wins,geom="bar")+coord_flip()
####

####Script Part 3.1.10
tempDatacricDataSet = cricDataSet[cricDataSet$PlayedinIndia=="Yes",]
tempData2 = as.data.frame(prop.table(table(tempDatacricDataSet$Winner,tempDatacricDataSet$PlayedinIndia))*100)
colnames(tempData2) = c("Country","PlayedinIndia","Wins")
tempData2 = tempData2[tempData2$Country!='no result' & tempData2$Country!='tied',]
tempDataConsolidated = rbind(tempData,tempData2)
ggplot(tempDataConsolidated, aes(x=Country, y=Wins, fill=factor(PlayedinIndia)))+
  geom_bar(position="dodge", stat="identity")+coord_flip()
####

####Script Part 3.1.11
cricDataSet = cricDataSet[cricDataSet$Winner!='no result',]
cricDataSet$WinnerChase = FALSE
cricDataSet$didIndiaChase = FALSE
cricDataSet[grep('wickets',cricDataSet$Margin),]$WinnerChase = TRUE
cricDataSet[cricDataSet$Winner=="India" & cricDataSet$WinnerChase==TRUE,]$didIndiaChase = TRUE
cricDataSet[cricDataSet$Winner!="India" & !is.na(str_extract(as.character(cricDataSet$Margin),'runs')),]$didIndiaChase = TRUE
tempData3 = cricDataSet[cricDataSet$Winner != "tied",]
tempData4 = as.data.frame(prop.table(table(tempData3$Winner,tempData3$didIndiaChase))*100)
colnames(tempData4) = c("Country","Did_India_Chase","Wins")
ggplot(tempData4, aes(x=Country, y=Wins, fill=factor(Did_India_Chase)))+
  geom_bar(position="dodge", stat="identity")+coord_flip()+ylab('Win %')
####

####Script Part 3.1.12
cricDataSet$didIndiaWin = FALSE
cricDataSet[cricDataSet$Winner=="India",]$didIndiaWin = TRUE
####

####Script Part 3.1.13
chisq.test(table(cricDataSet$PlayedinIndia,cricDataSet$didIndiaWin))
####

####Script Part 3.1.14
chisq.test(table(cricDataSet$didIndiaChase,cricDataSet$didIndiaWin))
####

####Script Part 3.1.15
downloadPatternLink = "http://www.espncricinfo.com/ci/engine/match/"
for(loop in 1:nrow(cricDataSet))
{
  if (!file.exists(cricDataSet[loop,]$ScorecardLink)){
    download.file(paste0(downloadPatternLink,cricDataSet[loop,]$ScorecardLink),cricDataSet[loop,]$ScorecardLink)
  }
}
####

####Script Part 3.1.16
dataFile = readLines("667641.html")
str_extract(dataFile[grep("view=comparison",dataFile)],"/.+/engine/match/[0-9]+.html.view=comparison")
####

####Script Part 3.1.17
if (!file.exists("667641_overcomparison.html")){
download.file(paste0("www.espncricinfo.com/",str_extract(dataFile[grep("view=comparison",dataFile)],"/.+/engine/match/[0-9]+.html.view=comparison")),
                     "667641_overcomparison.html")
}
####

####Script Part 3.1.18
dataFile = readHTMLTable("667641_overcomparison.html")
tempDataTable = dataFile[[1]]
tempDataTable = tempDataTable[,1:(ncol(tempDataTable)-1)]
colnames(tempDataTable) = c('Over_1','Score_1','Runs_1','RunRate_1','Rate_5ov_2','Over_2','Score_2','Runs_2','RunRate_2','Rate_5ov_2','Rate_Req','Runs_Req','Balls_Rem')
tempDataTable = tempDataTable[-1,]
tempDataTable$Runs_1 = as.character(tempDataTable$Runs_1)
tempDataTable$Runs_2 = as.character(tempDataTable$Runs_2)
tempDataTable$Runs_1 = as.integer(sapply(1:nrow(tempDataTable), function(x) sub('\\*','',tempDataTable[x,]$Runs_1)))
tempDataTable$Runs_2 = as.integer(sapply(1:nrow(tempDataTable), function(x) sub('\\*','',tempDataTable[x,]$Runs_2)))
tempDataTable$Score_1 = as.character(tempDataTable$Score_1 )
tempDataTable$Score_1_Runs = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_1,'/')[[1]][1]))
tempDataTable$Score_1_Wkts = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_1,'/')[[1]][2]))
tempDataTable$Score_2 = as.character(tempDataTable$Score_2 )
tempDataTable$Score_2_Runs = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_2,'/')[[1]][1]))
tempDataTable$Score_2_Wkts = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_2,'/')[[1]][2]))
tempDataTableFilt = tempDataTable[,c('Over_1','Runs_2','Score_2_Runs','Score_2_Wkts')]
tempDataTableFilt$Over_1 = as.integer(as.character(tempDataTableFilt$Over_1))
####

####Script Part 3.1.19
library(dplyr)
ggplot(data=tempDataTableFilt[!is.na(tempDataTableFilt$Score_2_Runs),],aes(x=Over_1,y=Score_2_Runs,group = 1))+
  geom_point()+xlab('Overs')+ylab('Runs')+geom_point(data=as.data.frame(tempDataTableFilt[!is.na(tempDataTableFilt$Score_2_Runs),] 
                                                                        %>% group_by(Score_2_Wkts) %>%summarise(Over_1 = min(Over_1),Score_2_Runs=min(Score_2_Runs))),
                                                     aes(x=Over_1,y=Score_2_Runs,color='red',size=10))
####

####Script Part 3.1.20
ggplot(data=tempDataTableFilt[!is.na(tempDataTableFilt$Score_2_Runs),],aes(x=Over_1,y=Score_2_Runs/Over_1,group = 1))+
  geom_line()+xlab('Overs')+ylab('Run Rate')
####

####Script Part 3.1.21
for(loop in 1:nrow(cricDataSet)) {
  dataFile = readLines(cricDataSet[loop,]$ScorecardLink)
  if (!file.exists(paste0(str_extract(cricDataSet[loop,]$ScorecardLink,"[0-9]+"),"_overcomparison.html"))) {
  download.file(paste0("www.espncricinfo.com",str_extract(dataFile[grep("view=comparison",dataFile)],"/.+/engine/match/[0-9]+.html.view=comparison")),
                paste0(str_extract(cricDataSet[loop,]$ScorecardLink,"[0-9]+"),"_overcomparison.html"))
  }
}
####

####Script Part 3.1.22
cricDataSetFilt = cricDataSet[cricDataSet$Winner!='tied',]
####

####Script Part 3.1.23
for(loop in 1:nrow(cricDataSetFilt)) {
  didIndiaChase = cricDataSetFilt[loop,]$didIndiaChase
  fileName = paste0(str_extract(cricDataSetFilt[loop,]$ScorecardLink,"[0-9]+"),"_overcomparison.html")
  dataFile = readHTMLTable(fileName)
  tempDataTable = dataFile[[1]]
  tempDataTable = tempDataTable[,1:(ncol(tempDataTable)-1)]
  colnames(tempDataTable) = c('Over_1','Score_1','Runs_1','RunRate_1','Rate_5ov_2','Over_2','Score_2','Runs_2','RunRate_2','Rate_5ov_2','Rate_Req','Runs_Req','Balls_Rem')
  tempDataTable = tempDataTable[-1,]
  tempDataTable$Runs_1 = as.character(tempDataTable$Runs_1)
  tempDataTable$Runs_2 = as.character(tempDataTable$Runs_2)
  tempDataTable$Runs_1 = as.integer(sapply(1:nrow(tempDataTable), function(x) sub('\\*','',tempDataTable[x,]$Runs_1)))
  tempDataTable$Runs_2 = as.integer(sapply(1:nrow(tempDataTable), function(x) sub('\\*','',tempDataTable[x,]$Runs_2)))
  tempDataTable$Score_1 = as.character(tempDataTable$Score_1 )
  tempDataTable$Score_1_Runs = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_1,'/')[[1]][1]))
  tempDataTable$Score_1_Wkts = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_1,'/')[[1]][2]))
  tempDataTable$Score_2 = as.character(tempDataTable$Score_2 )
  tempDataTable$Score_2_Runs = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_2,'/')[[1]][1]))
  tempDataTable$Score_2_Wkts = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_2,'/')[[1]][2]))
  if (didIndiaChase) {
    tempDataTableFilt = tempDataTable[,c('Over_1','Runs_2','Score_2_Runs','Score_2_Wkts')]
  } else {
    tempDataTableFilt = tempDataTable[,c('Over_1','Runs_1','Score_1_Runs','Score_1_Wkts')]
  }
  tempDataTableFilt$Over_1 = as.integer(as.character(tempDataTableFilt$Over_1))
  colnames(tempDataTableFilt) = c('Over_1','Runs','Cumul_Runs_Scored','Wickets_lost')
  tempDataTableFilt$fileReference = str_extract(cricDataSetFilt[loop,]$ScorecardLink,"[0-9]+")
  if (loop==1){
    cricDataSetOverComp = tempDataTableFilt
  } else {
    cricDataSetOverComp = rbind(cricDataSetOverComp,tempDataTableFilt)
  }
}
####


####Script Part 3.1.24
head(cricDataSetOverComp)
####

####Script Part 3.1.25
cricDataSetOverComp = cricDataSetOverComp[!is.na(cricDataSetOverComp$Runs),]
####

####Script Part 3.1.26
aggcricDataSetOverComp = as.data.frame(cricDataSetOverComp %>% group_by(fileReference) %>% summarise(totalRuns = sum(Runs)))
cricDataSetOverComp = merge(cricDataSetOverComp,aggcricDataSetOverComp,by="fileReference")
cricDataSetOverComp$RunRate = cricDataSetOverComp$Cumul_Runs_Scored/cricDataSetOverComp$Over_1
head(cricDataSetOverComp)
####

####Script Part 3.1.27
totalRuns_Predictor = lm(data=cricDataSetOverComp,totalRuns~Over_1+Cumul_Runs_Scored+Wickets_lost+RunRate)
summary(totalRuns_Predictor)
####

####Script Part 3.1.28
testData = data.frame(Over_1=20,Cumul_Runs_Scored=92,Wickets_lost=3,RunRate=4.6)
testData = rbind(testData,data.frame(Over_1=30,Cumul_Runs_Scored=167,Wickets_lost=3,RunRate=7.0))
testData = rbind(testData,data.frame(Over_1=40,Cumul_Runs_Scored=261,Wickets_lost=3,RunRate=10.6))
testData = rbind(testData,data.frame(Over_1=45,Cumul_Runs_Scored=308,Wickets_lost=4,RunRate=9.4))
predict(totalRuns_Predictor,newdata=testData)
####

####Script Part 3.1.29
tempDataSet = data.frame(fileReference=str_extract(cricDataSet$ScorecardLink,"[0-9]+"),PlayedinIndia=cricDataSet$PlayedinIndia)
cricDataSetOverComp = merge(cricDataSetOverComp,tempDataSet,by="fileReference")
totalRuns_Predictor = lm(data=cricDataSetOverComp,totalRuns~Over_1+Cumul_Runs_Scored+Wickets_lost+RunRate+PlayedinIndia)
summary(totalRuns_Predictor)
####

####Script Part 3.1.30
testData = data.frame(Over_1=20,Cumul_Runs_Scored=92,Wickets_lost=3,RunRate=4.6,PlayedinIndia="Yes")
testData = rbind(testData,data.frame(Over_1=30,Cumul_Runs_Scored=167,Wickets_lost=3,RunRate=7.0,PlayedinIndia="Yes"))
testData = rbind(testData,data.frame(Over_1=40,Cumul_Runs_Scored=261,Wickets_lost=3,RunRate=10.6,PlayedinIndia="Yes"))
testData = rbind(testData,data.frame(Over_1=45,Cumul_Runs_Scored=308,Wickets_lost=4,RunRate=9.4,PlayedinIndia="Yes"))
testData$PlayedinIndia = as.factor(testData$PlayedinIndia)
predict(totalRuns_Predictor,newdata=testData)
####

####Script Part 3.1.31a
loop = 1
didIndiaChase = cricDataSetFilt[loop,]$didIndiaChase
fileName = paste0(str_extract(cricDataSetFilt[loop,]$ScorecardLink,"[0-9]+"),"_overcomparison.html")
dataFile = readHTMLTable(fileName)
tempDataTable = dataFile[[1]]
tempDataTable = tempDataTable[,1:(ncol(tempDataTable)-1)]
colnames(tempDataTable) = c('Over_1','Score_1','Runs_1','RunRate_1','Rate_5ov_2','Over_2','Score_2','Runs_2','RunRate_2','Rate_5ov_2','Rate_Req','Runs_Req','Balls_Rem')
tempDataTable = tempDataTable[-1,]
tempDataTable$Runs_1 = as.character(tempDataTable$Runs_1)
tempDataTable$Runs_2 = as.character(tempDataTable$Runs_2)
tempDataTable$Runs_1 = as.integer(sapply(1:nrow(tempDataTable), function(x) sub('\\*','',tempDataTable[x,]$Runs_1)))
tempDataTable$Runs_2 = as.integer(sapply(1:nrow(tempDataTable), function(x) sub('\\*','',tempDataTable[x,]$Runs_2)))
tempDataTable$Score_1 = as.character(tempDataTable$Score_1 )
tempDataTable$Score_1_Runs = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_1,'/')[[1]][1]))
tempDataTable$Score_1_Wkts = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_1,'/')[[1]][2]))
tempDataTable$Score_2 = as.character(tempDataTable$Score_2 )
tempDataTable$Score_2_Runs = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_2,'/')[[1]][1]))
tempDataTable$Score_2_Wkts = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_2,'/')[[1]][2]))
####

####Script Part 3.1.31b
tempDataTable$Over_1 = as.integer(as.character(tempDataTable$Over_1))
if (didIndiaChase) {
  IndiaStats = tempDataTable[,c('Over_1','Runs_2','Score_2_Runs','Score_2_Wkts')]
  colnames(IndiaStats) = c('Over_1','IND_Runs_Scored','IND_Cumul_Runs_Scored','IND_Wickets_Lost')
  OtherTeamStats = tempDataTable[,c('Over_1','Runs_1','Score_1_Runs','Score_1_Wkts')]
  colnames(OtherTeamStats) = c('Over_1','OTH_Runs_Scored','OTH_Cumul_Runs_Scored','OTH_Wickets_Lost')
} else {
  IndiaStats = tempDataTable[,c('Over_1','Runs_1','Score_1_Runs','Score_1_Wkts')]
  colnames(IndiaStats) = c('Over_1','IND_Runs_Scored','IND_Cumul_Runs_Scored','IND_Wickets_Lost')
  OtherTeamStats = tempDataTable[,c('Over_1','Runs_2','Score_2_Runs','Score_2_Wkts')]
  colnames(OtherTeamStats) = c('Over_1','OTH_Runs_Scored','OTH_Cumul_Runs_Scored','OTH_Wickets_Lost')
}
####

####Script Part 3.1.31c
IndiaStats$pick = 0
IndiaStats$pick = IndiaStats$Over_1%%5
IndiaStats = IndiaStats[IndiaStats$pick==0,]
OtherTeamStats$pick = 0
OtherTeamStats$pick =OtherTeamStats$Over_1%%5
OtherTeamStats = OtherTeamStats[OtherTeamStats$pick==0,]
####

####Script Part 3.1.31d
IndiaRuns = c(IndiaStats[!is.na(IndiaStats$IND_Cumul_Runs_Scored),]$IND_Cumul_Runs_Scored,
              rep(NA,10-length(IndiaStats[!is.na(IndiaStats$IND_Cumul_Runs_Scored),]$IND_Cumul_Runs_Scored)))
OtherTeamRuns = c(OtherTeamStats[!is.na(OtherTeamStats$OTH_Cumul_Runs_Scored),]$OTH_Cumul_Runs_Scored,
              rep(NA,10-length(OtherTeamStats[!is.na(OtherTeamStats$OTH_Cumul_Runs_Scored),]$OTH_Cumul_Runs_Scored)))
####

####Script Part 3.1.31e
RunsScored = t(c(IndiaRuns,OtherTeamRuns))
if (loop == 1) {
  matchDataFrame = data.frame(cbind(str_extract(cricDataSetFilt[loop,]$ScorecardLink,"[0-9]+"),
                                    RunsScored,cricDataSetFilt[loop,]$didIndiaWin))
} else {
  matchDataFrame = rbind(matchDataFrame,data.frame(cbind(str_extract(cricDataSetFilt[loop,]$ScorecardLink,"[0-9]+"),
                                                         RunsScored,cricDataSetFilt[loop,]$didIndiaWin)))
}
####

####Script Part 3.1.32
for(loop in 1:nrow(cricDataSetFilt)) {
  ### Include 3.1.31a minus the loop initialisation
  ### Include 3.1.31b
  ### Include 3.1.31c
  ### Include 3.1.31d
  ### Include 3.1.31e
}
####

####Script Part 3.1.33
for(loop in 1:nrow(cricDataSetFilt)) {
  ####Script Part 3.1.31a
  didIndiaChase = cricDataSetFilt[loop,]$didIndiaChase
  fileName = paste0(str_extract(cricDataSetFilt[loop,]$ScorecardLink,"[0-9]+"),"_overcomparison.html")
  dataFile = readHTMLTable(fileName)
  tempDataTable = dataFile[[1]]
  tempDataTable = tempDataTable[,1:(ncol(tempDataTable)-1)]
  colnames(tempDataTable) = c('Over_1','Score_1','Runs_1','RunRate_1','Rate_5ov_2','Over_2','Score_2','Runs_2','RunRate_2','Rate_5ov_2','Rate_Req','Runs_Req','Balls_Rem')
  tempDataTable = tempDataTable[-1,]
  tempDataTable$Runs_1 = as.character(tempDataTable$Runs_1)
  tempDataTable$Runs_2 = as.character(tempDataTable$Runs_2)
  tempDataTable$Runs_1 = as.integer(sapply(1:nrow(tempDataTable), function(x) sub('\\*','',tempDataTable[x,]$Runs_1)))
  tempDataTable$Runs_2 = as.integer(sapply(1:nrow(tempDataTable), function(x) sub('\\*','',tempDataTable[x,]$Runs_2)))
  tempDataTable$Score_1 = as.character(tempDataTable$Score_1 )
  tempDataTable$Score_1_Runs = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_1,'/')[[1]][1]))
  tempDataTable$Score_1_Wkts = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_1,'/')[[1]][2]))
  tempDataTable$Score_2 = as.character(tempDataTable$Score_2 )
  tempDataTable$Score_2_Runs = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_2,'/')[[1]][1]))
  tempDataTable$Score_2_Wkts = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_2,'/')[[1]][2]))
  ####
  
  ####Script Part 3.1.31b
  tempDataTable$Over_1 = as.integer(as.character(tempDataTable$Over_1))
  if (didIndiaChase) {
    IndiaStats = tempDataTable[,c('Over_1','Runs_2','Score_2_Runs','Score_2_Wkts')]
    colnames(IndiaStats) = c('Over_1','IND_Runs_Scored','IND_Cumul_Runs_Scored','IND_Wickets_Lost')
    OtherTeamStats = tempDataTable[,c('Over_1','Runs_1','Score_1_Runs','Score_1_Wkts')]
    colnames(OtherTeamStats) = c('Over_1','OTH_Runs_Scored','OTH_Cumul_Runs_Scored','OTH_Wickets_Lost')
  } else {
    IndiaStats = tempDataTable[,c('Over_1','Runs_1','Score_1_Runs','Score_1_Wkts')]
    colnames(IndiaStats) = c('Over_1','IND_Runs_Scored','IND_Cumul_Runs_Scored','IND_Wickets_Lost')
    OtherTeamStats = tempDataTable[,c('Over_1','Runs_2','Score_2_Runs','Score_2_Wkts')]
    colnames(OtherTeamStats) = c('Over_1','OTH_Runs_Scored','OTH_Cumul_Runs_Scored','OTH_Wickets_Lost')
  }
  ####
  
  ####Script Part 3.1.31c
  IndiaStats$pick = 0
  IndiaStats$pick = IndiaStats$Over_1%%5
  IndiaStats = IndiaStats[IndiaStats$pick==0,]
  OtherTeamStats$pick = 0
  OtherTeamStats$pick =OtherTeamStats$Over_1%%5
  OtherTeamStats = OtherTeamStats[OtherTeamStats$pick==0,]
  ####
  
  ####Script Part 3.1.31d
  IndiaRuns = c(IndiaStats[!is.na(IndiaStats$IND_Cumul_Runs_Scored),]$IND_Cumul_Runs_Scored,
                rep(NA,10-length(IndiaStats[!is.na(IndiaStats$IND_Cumul_Runs_Scored),]$IND_Cumul_Runs_Scored)))
  OtherTeamRuns = c(OtherTeamStats[!is.na(OtherTeamStats$OTH_Cumul_Runs_Scored),]$OTH_Cumul_Runs_Scored,
                    rep(NA,10-length(OtherTeamStats[!is.na(OtherTeamStats$OTH_Cumul_Runs_Scored),]$OTH_Cumul_Runs_Scored)))
  ####
  
  ####Script Part 3.1.31e
  RunsScored = t(c(IndiaRuns,OtherTeamRuns))
  if (loop == 1) {
    matchDataFrame = data.frame(cbind(str_extract(cricDataSetFilt[loop,]$ScorecardLink,"[0-9]+"),
                                      RunsScored,cricDataSetFilt[loop,]$didIndiaWin))
  } else {
    matchDataFrame = rbind(matchDataFrame,data.frame(cbind(str_extract(cricDataSetFilt[loop,]$ScorecardLink,"[0-9]+"),
                                                           RunsScored,cricDataSetFilt[loop,]$didIndiaWin)))
  }
  ####
  
  
  }
####

####Script Part 3.1.34
for (loop in 2:21)
{
  matchDataFrame[,loop] = as.numeric(as.character(matchDataFrame[,loop]))
}
####

####Script Part 3.1.35
library(ggplot2)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
####

####Script Part 3.1.36
cricketTreeIndiainnings = rpart(X22 ~ ., data = matchDataFrame[,c(2:11,22)],method = "class")
fancyRpartPlot(cricketTreeIndiainnings)
####

####Script Part 3.1.37
cricketTreeOtherTeaminnings = rpart(X22 ~ ., data = matchDataFrame[,c(12:21,22)],method = "class")
fancyRpartPlot(cricketTreeOtherTeaminnings)
####


####Script Part 3.1.31d1
IndiaRunRate = c(IndiaStats[!is.na(IndiaStats$IND_Cumul_Runs_Scored),]$IND_Cumul_Runs_Scored/
                   IndiaStats[!is.na(IndiaStats$IND_Cumul_Runs_Scored),]$Over_1,
              rep(0,10-length(IndiaStats[!is.na(IndiaStats$IND_Cumul_Runs_Scored),]$IND_Cumul_Runs_Scored)))

OtherTeamRunRate = c(OtherTeamStats[!is.na(OtherTeamStats$OTH_Cumul_Runs_Scored),]$OTH_Cumul_Runs_Scored/
                    OtherTeamStats[!is.na(OtherTeamStats$OTH_Cumul_Runs_Scored),]$Over_1,
                  rep(0,10-length(OtherTeamStats[!is.na(OtherTeamStats$OTH_Cumul_Runs_Scored),]$OTH_Cumul_Runs_Scored)))
####

####Script Part 3.1.31d2
IndiaFallOfWickets = c(IndiaStats[!is.na(IndiaStats$IND_Cumul_Runs_Scored),]$IND_Wickets_Lost,
              rep(0,10-length(IndiaStats[!is.na(IndiaStats$IND_Cumul_Runs_Scored),]$IND_Cumul_Runs_Scored)))

OtherTeamFallOfWickets = c(OtherTeamStats[!is.na(OtherTeamStats$OTH_Cumul_Runs_Scored),]$OTH_Wickets_Lost,
                  rep(0,10-length(OtherTeamStats[!is.na(OtherTeamStats$OTH_Cumul_Runs_Scored),]$OTH_Cumul_Runs_Scored)))

####

####Script Part 3.1.31d3
IndiaLast10OversData = c(IndiaLast10Overs[!is.na(IndiaLast10Overs$IND_Runs_Scored),]$IND_Runs_Scored,
                         rep(NA,10-length(IndiaLast10Overs[!is.na(IndiaLast10Overs$IND_Runs_Scored),]$IND_Runs_Scored)))

OtherTeamStats10OversData = c(OtherTeamStats10Overs[!is.na(OtherTeamStats10Overs$OTH_Runs_Scored),]$OTH_Runs_Scored,
                              rep(NA,10-length(OtherTeamStats10Overs[!is.na(OtherTeamStats10Overs$OTH_Runs_Scored),]$OTH_Runs_Scored)))

####

####Script Part 3.1.31e1
RunsScored = t(c(IndiaRuns,IndiaRunRate,IndiaLast10OversData,IndiaFallOfWickets,
                 OtherTeamRuns,OtherTeamRunRate,OtherTeamStats10OversData,OtherTeamFallOfWickets))
if (loop == 1) {
  matchDataFrame = data.frame(cbind(str_extract(cricDataSetFilt[loop,]$ScorecardLink,"[0-9]+"),
                                    RunsScored,cricDataSetFilt[loop,]$didIndiaWin))
} else {
  matchDataFrame = rbind(matchDataFrame,data.frame(cbind(str_extract(cricDataSetFilt[loop,]$ScorecardLink,"[0-9]+"),
                                                         RunsScored,cricDataSetFilt[loop,]$didIndiaWin)))
}
####

####Script Part 3.1.38
for(loop in 1:nrow(cricDataSetFilt)) {
  ####Script Part 3.1.31a
  didIndiaChase = cricDataSetFilt[loop,]$didIndiaChase
  fileName = paste0(str_extract(cricDataSetFilt[loop,]$ScorecardLink,"[0-9]+"),"_overcomparison.html")
  dataFile = readHTMLTable(fileName)
  tempDataTable = dataFile[[1]]
  tempDataTable = tempDataTable[,1:(ncol(tempDataTable)-1)]
  colnames(tempDataTable) = c('Over_1','Score_1','Runs_1','RunRate_1','Rate_5ov_2','Over_2','Score_2','Runs_2','RunRate_2','Rate_5ov_2','Rate_Req','Runs_Req','Balls_Rem')
  tempDataTable = tempDataTable[-1,]
  tempDataTable$Runs_1 = as.character(tempDataTable$Runs_1)
  tempDataTable$Runs_2 = as.character(tempDataTable$Runs_2)
  tempDataTable$Runs_1 = as.integer(sapply(1:nrow(tempDataTable), function(x) sub('\\*','',tempDataTable[x,]$Runs_1)))
  tempDataTable$Runs_2 = as.integer(sapply(1:nrow(tempDataTable), function(x) sub('\\*','',tempDataTable[x,]$Runs_2)))
  tempDataTable$Score_1 = as.character(tempDataTable$Score_1 )
  tempDataTable$Score_1_Runs = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_1,'/')[[1]][1]))
  tempDataTable$Score_1_Wkts = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_1,'/')[[1]][2]))
  tempDataTable$Score_2 = as.character(tempDataTable$Score_2 )
  tempDataTable$Score_2_Runs = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_2,'/')[[1]][1]))
  tempDataTable$Score_2_Wkts = as.integer(sapply(1:nrow(tempDataTable), function(x) str_split(tempDataTable[x,]$Score_2,'/')[[1]][2]))
  ####
  
  ####Script Part 3.1.31b
  tempDataTable$Over_1 = as.integer(as.character(tempDataTable$Over_1))
  if (didIndiaChase) {
    IndiaStats = tempDataTable[,c('Over_1','Runs_2','Score_2_Runs','Score_2_Wkts')]
    colnames(IndiaStats) = c('Over_1','IND_Runs_Scored','IND_Cumul_Runs_Scored','IND_Wickets_Lost')
    OtherTeamStats = tempDataTable[,c('Over_1','Runs_1','Score_1_Runs','Score_1_Wkts')]
    colnames(OtherTeamStats) = c('Over_1','OTH_Runs_Scored','OTH_Cumul_Runs_Scored','OTH_Wickets_Lost')
  } else {
    IndiaStats = tempDataTable[,c('Over_1','Runs_1','Score_1_Runs','Score_1_Wkts')]
    colnames(IndiaStats) = c('Over_1','IND_Runs_Scored','IND_Cumul_Runs_Scored','IND_Wickets_Lost')
    OtherTeamStats = tempDataTable[,c('Over_1','Runs_2','Score_2_Runs','Score_2_Wkts')]
    colnames(OtherTeamStats) = c('Over_1','OTH_Runs_Scored','OTH_Cumul_Runs_Scored','OTH_Wickets_Lost')
  }
  ####
  
  ####Script Part 3.1.31c
  IndiaLast10Overs = IndiaStats[IndiaStats$Over_1>=41,]
  IndiaStats$pick = 0
  IndiaStats$pick = IndiaStats$Over_1%%5
  IndiaStats = IndiaStats[IndiaStats$pick==0,]
  OtherTeamStats10Overs = OtherTeamStats[OtherTeamStats$Over_1>=41,]
  OtherTeamStats$pick = 0
  OtherTeamStats$pick =OtherTeamStats$Over_1%%5
  OtherTeamStats = OtherTeamStats[OtherTeamStats$pick==0,]
  ####
  
  ####Script Part 3.1.31d
  IndiaRuns = c(IndiaStats[!is.na(IndiaStats$IND_Cumul_Runs_Scored),]$IND_Cumul_Runs_Scored,
                rep(NA,10-length(IndiaStats[!is.na(IndiaStats$IND_Cumul_Runs_Scored),]$IND_Cumul_Runs_Scored)))
  OtherTeamRuns = c(OtherTeamStats[!is.na(OtherTeamStats$OTH_Cumul_Runs_Scored),]$OTH_Cumul_Runs_Scored,
                    rep(NA,10-length(OtherTeamStats[!is.na(OtherTeamStats$OTH_Cumul_Runs_Scored),]$OTH_Cumul_Runs_Scored)))
  ####
  
  ####Script Part 3.1.31d1
  IndiaRunRate = c(IndiaStats[!is.na(IndiaStats$IND_Cumul_Runs_Scored),]$IND_Cumul_Runs_Scored/
                     IndiaStats[!is.na(IndiaStats$IND_Cumul_Runs_Scored),]$Over_1,
                   rep(NA,10-length(IndiaStats[!is.na(IndiaStats$IND_Cumul_Runs_Scored),]$IND_Cumul_Runs_Scored)))
  
  OtherTeamRunRate = c(OtherTeamStats[!is.na(OtherTeamStats$OTH_Cumul_Runs_Scored),]$OTH_Cumul_Runs_Scored/
                         OtherTeamStats[!is.na(OtherTeamStats$OTH_Cumul_Runs_Scored),]$Over_1,
                       rep(NA,10-length(OtherTeamStats[!is.na(OtherTeamStats$OTH_Cumul_Runs_Scored),]$OTH_Cumul_Runs_Scored)))
  ####
  
  ####Script Part 3.1.31d2
  IndiaFallOfWickets = c(IndiaStats[!is.na(IndiaStats$IND_Cumul_Runs_Scored),]$IND_Wickets_Lost,
                         rep(NA,10-length(IndiaStats[!is.na(IndiaStats$IND_Cumul_Runs_Scored),]$IND_Cumul_Runs_Scored)))
  
  OtherTeamFallOfWickets = c(OtherTeamStats[!is.na(OtherTeamStats$OTH_Cumul_Runs_Scored),]$OTH_Wickets_Lost,
                             rep(NA,10-length(OtherTeamStats[!is.na(OtherTeamStats$OTH_Cumul_Runs_Scored),]$OTH_Cumul_Runs_Scored)))
  
  ####
  
  ####Script Part 3.1.31d3
  IndiaLast10OversData = c(IndiaLast10Overs[!is.na(IndiaLast10Overs$IND_Runs_Scored),]$IND_Runs_Scored,
                         rep(NA,10-length(IndiaLast10Overs[!is.na(IndiaLast10Overs$IND_Runs_Scored),]$IND_Runs_Scored)))
  
  OtherTeamStats10OversData = c(OtherTeamStats10Overs[!is.na(OtherTeamStats10Overs$OTH_Runs_Scored),]$OTH_Runs_Scored,
                             rep(NA,10-length(OtherTeamStats10Overs[!is.na(OtherTeamStats10Overs$OTH_Runs_Scored),]$OTH_Runs_Scored)))
  
  ####
  
  ####Script Part 3.1.31e1
  RunsScored = t(c(IndiaRuns,IndiaRunRate,IndiaLast10OversData,IndiaFallOfWickets,
                   OtherTeamRuns,OtherTeamRunRate,OtherTeamStats10OversData,OtherTeamFallOfWickets))
  if (loop == 1) {
    matchDataFrame = data.frame(cbind(str_extract(cricDataSetFilt[loop,]$ScorecardLink,"[0-9]+"),
                                      RunsScored,cricDataSetFilt[loop,]$didIndiaWin))
  } else {
    matchDataFrame = rbind(matchDataFrame,data.frame(cbind(str_extract(cricDataSetFilt[loop,]$ScorecardLink,"[0-9]+"),
                                                           RunsScored,cricDataSetFilt[loop,]$didIndiaWin)))
  }
  ####
  
  
}
####

####Script Part 3.1.39
for (loop in 2:81)
{
  matchDataFrame[,loop] = as.numeric(as.character(matchDataFrame[,loop]))
}
####

####Script Part 3.1.40
cricketTreeOtherTeaminnings = rpart(X82 ~ ., data = matchDataFrame[,c(42:81,82)],method = "class")
fancyRpartPlot(cricketTreeOtherTeaminnings)
####





