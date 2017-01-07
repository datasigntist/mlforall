# 
#   Created : 2-Jan-2017
#
####Script Part 2.2.1
head(faithful)
#####################################################

####Script Part 2.2.2
library(ggplot2)
qplot(data=faithful,waiting,eruptions)
#####################################################

####Script Part 2.2.3
faithfulLinModel = lm (faithful$eruptions ~ faithful$waiting)
faithfulLinModel
#####################################################

####Script Part 2.2.4
print(paste('Intercept ',as.character(faithfulLinModel$coefficients[[1]]),'Constant ',as.character(faithfulLinModel$coefficients[[2]])))
#####################################################

####Script Part 2.2.5
predictedEruptions = faithfulLinModel$coefficients[[2]]*faithful$waiting+faithfulLinModel$coefficients[[1]]
#####################################################

####Script Part 2.2.6
predictedEruptions_Predictfn = predict(faithfulLinModel)
#####################################################

####Script Part 2.2.7
sum(predictedEruptions-predictedEruptions_Predictfn)
#####################################################

####Script Part 2.2.8
ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x) +
  geom_point()
#####################################################

####Script Part 2.2.9
summary(faithfulLinModel)$r.squared	
#Calculation
sum((predictedEruptions_Predictfn-mean(predictedEruptions_Predictfn))^2)/
  sum((faithful$eruptions-mean(faithful$eruptions))^2)
#####################################################

####Script Part 2.2.10
data("AirPassengers")
airPassengers = AirPassengers
plot(airPassengers, ylab="Passenger Count (1000s)", type="o", pch =20)
#####################################################

####Script Part 2.2.11
df = data.frame(as.matrix(airPassengers),date=time(airPassengers))
colnames(df) = c("PassengerCount","Time")
df$year= as.integer(df$Time)
df$TimeNumeric = round(as.numeric(as.character(df$Time)),3)
aggDF = data.frame(aggregate(x=df$PassengerCount,by=list(df$year),FUN="mean"))
colnames(aggDF) = c("Year","PassengerCount")
qplot(data=aggDF,Year,PassengerCount,geom="line")
qplot(data=df[df$Time>1958,],Time,PassengerCount,geom="line")
#####################################################

####Script Part 2.2.12
airPassDecom = decompose(airPassengers, type = "multiplicative")
plot(airPassDecom)
#####################################################

####Script Part 2.2.13
airPassDecom$trend
#####################################################

####Script Part 2.2.14
(mean(df[df$TimeNumeric>=1949 & df$TimeNumeric<1950,]$PassengerCount)
+mean(df[df$TimeNumeric>=1949.083 & df$TimeNumeric<1950.083,]$PassengerCount))/2
#####################################################

####Script Part 2.2.15
(mean(df[df$TimeNumeric>=1949.083 & df$TimeNumeric<1950.083,]$PassengerCount)
 +mean(df[df$TimeNumeric>=1949.167 & df$TimeNumeric<1950.167,]$PassengerCount))/2
#####################################################

####Script Part 2.2.16
(mean(df[df$TimeNumeric>=1949.167 & df$TimeNumeric<1950.167,]$PassengerCount)
 +mean(df[df$TimeNumeric>=1949.250 & df$TimeNumeric<1950.250,]$PassengerCount))/2
#####################################################

####Script Part 2.2.17
(mean(df[df$TimeNumeric>=1949.250 & df$TimeNumeric<1950.250,]$PassengerCount)
 +mean(df[df$TimeNumeric>=1949.333 & df$TimeNumeric<1950.333,]$PassengerCount))/2
#####################################################

####Script Part 2.2.18
df$trend = as.numeric(airPassDecom$trend)
df$SeasonalIrr = df$PassengerCount/df$trend
df$monthnumber = 1:12
#####################################################

####Script Part 2.2.19
dfSeasonalAgg = aggregate(df[!is.na(df$SeasonalIrr),]$SeasonalIrr,by=list(df[!is.na(df$SeasonalIrr),]$monthnumber),FUN="mean")
colnames(dfSeasonalAgg) = c("monthnumber","seasonal")
#####################################################

####Script Part 2.2.20
dfSeasonalAgg$seasonal_n = dfSeasonalAgg$seasonal*(nrow(dfSeasonalAgg)/sum(dfSeasonalAgg$seasonal))
#####################################################

####Script Part 2.2.21
df$timeSeq = 1:144
timeSeriesModel = lm(df$trend ~ df$timeSeq)
timeSeriesModel
#####################################################

####Script Part 2.2.22
timeSeq_1961=145:156
df_1961 = data.frame(timeSeq=timeSeq_1961,PassengerCount_Trend=timeSeq_1961*timeSeriesModel$coefficients[2]+timeSeriesModel$coefficients[1],
                     row.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
df_1961$PassengerCount_Actual = df_1961$PassengerCount_Trend*dfSeasonalAgg$seasonal_n
#####################################################

####Script Part 2.2.23
ggplot(data = df, aes(x = timeSeq, y = PassengerCount))+ 
  xlab("Time Seq")+
  ylab("Passenger Count (1000s)")+
  geom_line()+
  geom_line(data=df_1961, aes(x = timeSeq,y = PassengerCount_Actual),colour="red")
#####################################################

####Script Part 2.2.24
europeData = read.csv('https://raw.githubusercontent.com/datasigntist/mlforall/master/datasets/AirPassengerTransportMonthlyStatistics.csv')
europeData$Date = as.Date(europeData$Date,'%d/%m/%y')
head(europeData)
#####################################################

####Script Part 2.2.25
# Install this library if you do not have it
# install.packages('xts')
library(xts)
europeDatats_obj = ts(as.numeric(europeData$PassengerCount),start = c(2014,1), frequency = 12)
plot(as.xts(europeDatats_obj), major.format = "%Y-%m")
#####################################################

####Script Part 2.2.26
europeDatats_obj_ts = decompose(europeDatats_obj,type="multiplicative")
plot(europeDatats_obj_ts)
#####################################################

####Script Part 2.2.27
timeSeq = 1:24
timeSeriesModel2 = lm(as.numeric(europeDatats_obj_ts$trend) ~ timeSeq)
timeSeriesModel2
#####################################################

####Script Part 2.2.28
timeSeq_2016=25:36
df_2016 = data.frame(timeSeq=timeSeq_2016,PassengerCount_Trend=timeSeq_2016*timeSeriesModel2$coefficients[2]+timeSeriesModel2$coefficients[1],
                     row.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
df_2016$PassengerCount_Actual = df_2016$PassengerCount_Trend*as.numeric(unique(europeDatats_obj_ts$seasonal))
#####################################################

####Script Part 2.2.29
europeData$timeSeq = timeSeq
ggplot(data = europeData,aes(x = timeSeq, y = europeData$PassengerCount))+ 
  xlab("Time Seq")+
  ylab("Passenger Count (1000s)")+
  geom_line()+
  geom_line(data=df_2016, aes(x = timeSeq_2016,y = PassengerCount_Actual),colour="red")
#####################################################

####Script Part 2.2.30
summary(timeSeriesModel2)$r.squared	
#####################################################
