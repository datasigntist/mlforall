# 
#   Created : 30-Nov-2016
#
####Script Part 2.1.1
# The following code base reflects the method to download the files from an internet resource

# Set the current working directory
setwd("//Users//vishwanathanraman//Documents//mlforall//datasets//matrimonialads")

bridesWantedEndLoop = 11
bridesWantedURL = "http://www.thehindu.com/classifieds/matrimonial/?categoryName=bride&pageNo="
bridesWantedFileName = "bridesWantedPage"

groomsWantedURL= "http://www.thehindu.com/classifieds/matrimonial/?categoryName=bridegroom&pageNo="
groomsWantedFileName  = "groomsWantedPage"
groomsWantedEndLoop = 10
# However the for convenience files can be accessed from the following repository where they have been already downloaded from the website
# https://github.com/datasigntist/mlforall/tree/master/datasets/matrimonialads
#####################################################

####Script Part 2.1.2
for(loop in 1:bridesWantedEndLoop)
{
  download.file(
    paste0(bridesWantedURL,loop),
    paste0(bridesWantedFileName,loop,".html"))
}

for(loop in 1:groomsWantedEndLoop)
{
  download.file(
    paste0(groomsWantedURL,loop),
    paste0(groomsWantedFileName,loop,".html"))
}
#####################################################

####Script Part 2.1.3
# Loading the Libraries
library(XML)
library(stringr)
library(ggplot2)
library(tm)
library(NLP)
library(RColorBrewer)
library(wordcloud)
#####################################################


####Script Part 2.1.4
for(loop in 1:bridesWantedEndLoop)
{
  bridesWantedPage = 
    readLines(paste0(bridesWantedFileName,loop,".html"))
  
  htmlData = 
    htmlTreeParse(bridesWantedPage, 
                  useInternalNodes = TRUE)
  
  id_or_class_xp <- "//div[@class='data']//text()"
  parsedData = xpathSApply(htmlData,id_or_class_xp,xmlValue)
  
  if (loop == 1)
  {
    collectionParsedDataforBrides = parsedData
  } else {
    collectionParsedDataforBrides = c(collectionParsedDataforBrides,parsedData)
  }
}
#####################################################

####Script Part 2.1.5
collectionParsedDataforBrides = 
  collectionParsedDataforBrides[str_trim(collectionParsedDataforBrides)!=""]
collectionParsedDataforBrides = 
  collectionParsedDataforBrides[-grep('PUBLISHED',collectionParsedDataforBrides)] 
#####################################################


####Script Part 2.1.6
for(loop in 1:groomsWantedEndLoop)
{
  groomsWantedPage = 
    readLines(paste0(groomsWantedFileName,loop,".html"))
  
  htmlData = 
    htmlTreeParse(groomsWantedPage, 
                  useInternalNodes = TRUE)
  
  id_or_class_xp <- "//div[@class='data']//text()"
  parsedData = xpathSApply(htmlData,id_or_class_xp,xmlValue)
  
  if (loop == 1)
  {
    collectionParsedDataforBrideGrooms = parsedData
  } else {
    collectionParsedDataforBrideGrooms = c(collectionParsedDataforBrideGrooms,parsedData)
  }
}
#####################################################

####Script Part 2.1.7
collectionParsedDataforBrideGrooms = 
  collectionParsedDataforBrideGrooms[
    str_trim(collectionParsedDataforBrideGrooms)!=""]
collectionParsedDataforBrideGrooms = 
  collectionParsedDataforBrideGrooms[
    -grep('PUBLISHED',collectionParsedDataforBrideGrooms)] 
#####################################################

####Script Part 2.1.8a
# Combining the datasets
adsData = data.frame(
  adWanted=collectionParsedDataforBrideGrooms,
  adfor='Grooms')
adsData = rbind(adsData,data.frame(
  adWanted=collectionParsedDataforBrides,
  adfor='Brides'))
#####################################################

####Script Part 2.1.8b
adsData$adLength = sapply(1:nrow(adsData), function(x) str_length(adsData[x,]$adWanted))
qplot(adsData$adLength)
#####################################################

####Script Part 2.1.8c
summary(adsData$adLength)
#####################################################

####Script Part 2.1.9
adsData$adWantedOrig = adsData$adWanted
# Remove specical characters
adsData$adWanted = gsub('\\n','',adsData$adWanted)
adsData$adWanted = gsub('[1-9]+\\.[1-9]+\\.[1-9]+',' ',adsData$adWanted)
adsData$adWanted = gsub('[1-9]\\.[0-9]{2}',' ',adsData$adWanted)
adsData$adWanted = gsub('[0-9]{2}-[0-9]{2}','',adsData$adWanted)
adsData$adWanted = gsub('-','',adsData$adWanted)
adsData$adWanted = gsub('/',' ',adsData$adWanted)
adsData$adWanted = gsub('\\,',' ',adsData$adWanted)
adsData$adWanted = gsub('\\)',' ',gsub('\\(',' ',adsData$adWanted))
#####################################################

####Script Part 2.1.10
# Extract the email address
adsData$emailaddress = str_extract(
  adsData$adWanted,
  "[A-Za-z][[:alnum:]]+\\@[[:alpha:]]+\\.com|[[:alnum:]]+\\@[[:alpha:]]+\\.co\\.[a-z]+|[A-Za-z][[:alnum:]]+\\@[[:alpha:]]+\\.net")
adsData$adWanted = gsub(
  "[A-Za-z][[:alnum:]]+\\@[[:alpha:]]+\\.com|[[:alnum:]]+\\@[[:alpha:]]+\\.co\\.[a-z]+|[A-Za-z][[:alnum:]]+\\@[[:alpha:]]+\\.net",
  '',
  adsData$adWanted)
adsData$emailServiceProvider=
  str_extract(adsData$emailaddress,'\\@[[:alpha:]]+')
#####################################################

####Script Part 2.1.11
emailProviderStats = 
  data.frame(t(t(table(adsData$emailServiceProvider))))
colnames(emailProviderStats) = 
  c('emailServiceProvider','dummy','Freq')
emailProviderStats = emailProviderStats[,c(1,3)]
qplot(data=emailProviderStats[emailProviderStats$Freq>1,],
      emailServiceProvider,
      weight=Freq,geom="bar")+coord_flip()
#####################################################

####Script Part 2.1.12
adsData$phoneNumber = 
  str_extract(adsData$adWanted,'[0][0-9]{10}')
adsData$adWanted = 
  gsub('[0][0-9]{10}','',adsData$adWanted)
adsData[is.na(adsData$phoneNumber),]$phoneNumber = 
  str_extract(adsData[is.na(adsData$phoneNumber),]$adWanted,
              '[1-9][0-9]{9}')
adsData$adWanted = gsub('[1-9][0-9]{9}','',adsData$adWanted)
#####################################################

####Script Part 2.1.13a
adsData$adWanted = 
  gsub('[0-9]{4}|[0-9]{5}|[0-9]{6}|[0-9]{7}|[0-9]{8}|[0-9]{9}|[0-9]{10}',
       ' ',adsData$adWanted)
adsData$personsHeight = 
  as.numeric(str_trim
             (str_extract
               (adsData$adWanted,"[1-9][0-9]{2}")))
adsData[!is.na(adsData$personsHeight) & adsData$personsHeight>190,]$personsHeight=NA
adsData$adWanted = gsub('[1-9][0-9]{2}',' ',adsData$adWanted)
qplot(data=adsData,adfor,personsHeight,geom="boxplot")
#####################################################

####Script Part 2.1.13b
adsData[!is.na(adsData$personsHeight) & adsData$personsHeight<150,]$personsHeight=NA
qplot(data=adsData,adfor,personsHeight,geom="boxplot")
#####################################################

####Script Part 2.1.14
adsData$Age = 
  as.numeric(str_trim
             (str_extract
               (adsData$adWanted,"[2-6][0-9]")))
adsData$adWanted = gsub('[0-9]{2}',' ',adsData$adWanted)
qplot(data=adsData,adfor,Age,geom="boxplot")
#####################################################

####Script Part 2.1.15
adsData$adWanted = 
  gsub('[0-9]|[0-9]{2}',
       ' ',adsData$adWanted)
adsData$adWanted = gsub('\\bcms\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bcm\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bft\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\.',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('&',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\baged\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bage\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub(':',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bct\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bemail\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\byrs\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bson\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bboy\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bgirl\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bdaughter\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bph\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\blacs\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\blac\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bpm\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\blakh\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\blakhs\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bpa\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\blpa\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bcrs\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bper\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bmonth\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bfor\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bID\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\btheir\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bseek\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bseeks\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bsuitable\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\babove\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bl\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bk\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bbride\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bgroom\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bparents\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bparent\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\brs\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bto\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bor\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bCt\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bCH\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bCE\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bmail\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bE\\b',' ',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\'','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('~','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('"','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\ba\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bkgs\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bwkg\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bas\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bTL\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bCall\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bare\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bbelow\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('+','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\balliance\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\binvite\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bof\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\blikely\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bfrom\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bcotna\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bindia\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\babroad\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bconta\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bsend bhp\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bcontact\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bphoto biodata\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = gsub('\\bwith\\b','',adsData$adWanted,ignore.case=TRUE)
adsData$adWanted = str_trim(adsData$adWanted)
adsData$adWanted = str_to_lower(adsData$adWanted)
#####################################################

####Script Part 2.1.16
brokenText = 
  sapply(1:nrow(adsData),
         function(x) 
           str_split(adsData[x,]$adWanted,' ')[[1]][str_length(str_split(adsData[x,]$adWanted,' ')[[1]])>1])
for(loop in 1:length(brokenText)) {
  adsData[loop,]$adWanted= paste(brokenText[[loop]],collapse=' ')
}
#####################################################

####Script Part 2.1.17
for(row in 1:nrow(adsData))
{
  if (row == 1) {
    allContentData =   adsData[row,]$adWanted
  } else {
    allContentData = paste(allContentData,adsData[row,]$adWanted)
  }
}
allContentWords = strsplit(allContentData, " ", fixed = T)[[1]]
allContentWordsNGrams = vapply(ngrams(allContentWords, 2), paste, "", collapse = " ")
allContentWordsNGramsCounts = as.data.frame(xtabs(~allContentWordsNGrams))
allContentWordsNGramsCounts$allContentWordsNGrams = 
  str_trim(allContentWordsNGramsCounts$allContentWordsNGrams)
allContentWordsNGramsCounts$wordCount = 
  sapply(1:nrow(allContentWordsNGramsCounts), function(x) 
      sapply(gregexpr("\\W+", 
                      allContentWordsNGramsCounts[x,]$allContentWordsNGrams), length)+1)
allContentWordsNGramsCounts = 
  allContentWordsNGramsCounts[order(allContentWordsNGramsCounts$Freq, decreasing = T),]
allContentWordsNGramsCounts[allContentWordsNGramsCounts$Freq>1,]
#####################################################

####Script Part 2.1.18
allContent = Corpus(VectorSource(adsData$adWanted))
allContent = tm_map(allContent, removePunctuation)
allContent = tm_map(allContent, removeNumbers)
allContent = tm_map(allContent, content_transformer(tolower))
allContent = tm_map(allContent, stripWhitespace)
allContentTDM <- TermDocumentMatrix(allContent)
t(t(findAssocs(allContentTDM, 'well', 0.3)[[1]]))
#####################################################

####Script Part 2.1.19
matchPercentage = 0.3
bigramList = sapply(1:nrow(allContentWordsNGramsCounts[allContentWordsNGramsCounts$Freq>1,]),
       function(x)
       {
         if (length(rownames(t(t(findAssocs(allContentTDM, 
                    str_split(allContentWordsNGramsCounts[allContentWordsNGramsCounts$Freq>1,][x,1],' ')[[1]][1]
                    , matchPercentage)[[1]]))))>1)
           paste(str_split(allContentWordsNGramsCounts[allContentWordsNGramsCounts$Freq>1,][x,1],' ')[[1]][1],
                 rownames(t(t(findAssocs(allContentTDM, 
                    str_split(allContentWordsNGramsCounts[allContentWordsNGramsCounts$Freq>1,][x,1],' ')[[1]][1]
                    , matchPercentage)[[1]]))))
       }         
       )

refinedBigramList = unlist(sapply(1:length(unique(unlist(bigramList))), 
       function(x)
       {
         if (length(grep(unique(unlist(bigramList))[x],adsData$adWanted))>2)
           unique(unlist(bigramList))[x]
       }
    ))
wordStats = data.frame(ngram=refinedBigramList)
wordStats$stats = sapply(1:nrow(wordStats), function(x) length(grep(wordStats[x,'ngram'],adsData$adWanted)))
#####################################################

####Script Part 2.1.20
for(row in 1:nrow(adsData))
{
  if (row == 1) {
    allContentData =   adsData[row,]$adWanted
  } else {
    allContentData = paste(allContentData,adsData[row,]$adWanted)
  }
}
allContentWords = strsplit(allContentData, " ", fixed = T)[[1]]
allContentWordsNGrams = vapply(ngrams(allContentWords, 3), paste, "", collapse = " ")
allContentWordsNGramsCounts = as.data.frame(xtabs(~allContentWordsNGrams))
allContentWordsNGramsCounts$allContentWordsNGrams = str_trim(allContentWordsNGramsCounts$allContentWordsNGrams)
allContentWordsNGramsCounts$wordCount = 
  sapply(1:nrow(allContentWordsNGramsCounts), function(x) 
    sapply(gregexpr("\\W+", allContentWordsNGramsCounts[x,]$allContentWordsNGrams), length)+1)

allContentWordsNGramsCounts = allContentWordsNGramsCounts[order(allContentWordsNGramsCounts$Freq, decreasing = T),]
refinedTrigramList=unlist(sapply(1:nrow(allContentWordsNGramsCounts[allContentWordsNGramsCounts$Freq>1,]),function(x)
       {
         if (length(grep(allContentWordsNGramsCounts[allContentWordsNGramsCounts$Freq>1,][x,1],adsData$adWanted))>1)
         {
           allContentWordsNGramsCounts[allContentWordsNGramsCounts$Freq>1,][x,1]
         }
       }
))
triwordStats = data.frame(ngram=refinedTrigramList)
triwordStats$stats = sapply(1:nrow(triwordStats), function(x) length(grep(triwordStats[x,'ngram'],adsData$adWanted)))
#####################################################

####Script Part 2.1.21
wordStatsConsol = rbind(wordStats,triwordStats)
wordStatsConsol = wordStatsConsol[order(wordStatsConsol$stats,decreasing = TRUE),]
#Plot Wordcloud
wordcloud(wordStatsConsol[wordStatsConsol$stats>2,]$ngram, wordStatsConsol[wordStatsConsol$stats>2,]$stats, 
          random.order=FALSE, 
          colors=brewer.pal(6, "Dark2"),
          scale=c(3,.5),
          rot.per=.10,max.words=70)
#####################################################
