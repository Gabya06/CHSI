# cleanData
# 
# make sure data does not get loaded with factors
options(stringsAsFactors= FALSE)
getwd()
# Load demographics data and clean
demoData <- read.csv('~/dev/Rstudio/data/DEMOGRAPHICS.csv')

head(demoData)
tail(demoData)

# remove first 2 columns
demoData <- demoData[,-(1:2)]
# check
head(demoData)
# remove columns 4 and 5
demoData <- (demoData[,-(4:5)])
str(demoData)

# Load leading Causes of death data and clean
leadCauseData <- read.csv('~/dev/Rstudio/data/LEADINGCAUSESOFDEATH.csv')
str(leadCauseData)
head(leadCauseData)
summary(leadCauseData)


# remove first 2 columns and 4th
leadCauseData <- leadCauseData[][-(1:2)]
leadCauseData <-leadCauseData[][-(4)]

#leadCauseData2[leadCauseData2=='-2222']<-0

# clean up -1111 and -2222 values 
leadCauseData <- as.data.frame(lapply(leadCauseData, function(x){
  replace(x, x < 0, 0) 
}))
str(leadCauseData)
summary(leadCauseData)

# Load demographics data and clean
riskData <- read.csv('~/dev/Rstudio/data/RISKFACTORSANDACCESSTOCARE.csv')
summary(riskData)
names(riskData)
# remove first 2 columns and 4th
riskData <- riskData[][-(1:2)]
riskData <- riskData[][-(4)]

# clean up -1111 and -2222 values 
riskData <- as.data.frame(lapply(riskData, function(x){
  replace(x, x<0, 0)
}))

