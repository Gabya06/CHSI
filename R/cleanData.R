
#install_github("devtools","hadley")
library(devtools)
library(roxygen2)
#library(CHSI)
library(plyr)
library(reshape2)
library(ggmap)
library(ggplot2)
library(ggthemes)
library(scales)
require(coefplot)

# cleanData 
# make sure data does not get loaded with factors
options(stringsAsFactors= FALSE)

# -------------------------------------------------------------------------------------------------------------------------
# Load demographics data and clean
# -------------------------------------------------------------------------------------------------------------------------
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

# clean up -1111 and -2222 values in entire demographics dataset
demoData <- as.data.frame(lapply(demoData, function(x){
  replace(x, x<0, 0)
}))



# subset data to only include some columns
demographics_dat <- subset(demoData, select = 
                             c(CHSI_County_Name:CHSI_State_Abbr, Population_Size, 
                               Population_Density, Poverty, Age_19_Under, Age_19_64, 
                               Age_65_84, Age_85_and_Over, White, Black, Native_American, Asian, Hispanic))
# clean names of the subset
nms_demo_dat <- c("county.name","state.name","state.abbr","pop.size","pop.density",
                  "poverty","age.19_under","age.19_64","age.65_84","age.85_over",
                  "white","black","nat.amer","asian","hispanic")
# change col names
names(demographics_dat)<-nms_demo_dat
head(demographics_dat)


# -------------------------------------------------------------------------------------------------------------------------
# Load Leading Cause of death and clean
# -------------------------------------------------------------------------------------------------------------------------
leadCauseData <- read.csv('~/dev/Rstudio/data/LEADINGCAUSESOFDEATH.csv')
str(leadCauseData)# there are alot of missing values -1111 and -2222.2 that need to be removed
head(leadCauseData) 
summary(leadCauseData)


# remove first 2 columns and 4th
leadCauseData <- leadCauseData[][-(1:2)]
leadCauseData <-leadCauseData[][-(4)]


# subset and pick only a few columns
leadingCause_dat <- subset(leadCauseData, select = 
                             c(CHSI_County_Name:CHSI_State_Abbr, B_Wh_Cancer, B_Bl_Cancer, B_Ot_Cancer, B_Hi_Cancer, 
                               C_Wh_Cancer, C_Bl_Cancer, C_Ot_Cancer, C_Hi_Cancer, 
                               D_Wh_Cancer, D_Bl_Cancer, D_Ot_Cancer, D_Hi_Cancer, 
                               D_Wh_HeartDis, D_Bl_HeartDis, D_Ot_HeartDis, D_Hi_HeartDis, 
                               E_Wh_Cancer, E_Bl_Cancer, E_Ot_Cancer, E_Hi_Cancer, 
                               E_Wh_HeartDis, E_Bl_HeartDis, E_Ot_HeartDis, E_Hi_HeartDis, 
                               F_Wh_HeartDis, F_Bl_HeartDis,F_Ot_HeartDis,F_Hi_HeartDis, 
                               F_Wh_Cancer, F_Bl_Cancer, F_Ot_Cancer, F_Hi_Cancer))

# change the names
nms_leadingCause_dat <- c("county.name","state.name","state.abbr", 
                          "cancer.wh.1_14","cancer.bl.1_14", "cancer.ot.1_14","cancer.his.1_14",
                          "cancer.wh.15_24", "cancer.bl.15_24","cancer.ot.15_24","cancer.his.15_24",
                          "cancer.wh.25_44", "cancer.bl.25_44","cancer.ot.25_44","cancer.his.25_44",
                          "heartdis.wh.25_44", "heartdis.bl.25_44","heartdis.ot.25_44","heartdis.his.25_44",
                          "cancer.wh.45_64", "cancer.bl.45_64","cancer.ot.45_64","cancer.his.45_64",
                          "heartdis.wh.45_64", "heartdis.bl.45_64","heartdis.ot.45_64","heartdis.his.45_64",
                          "heartdis.wh.65_over", "heartdis.bl.65_over","heartdis.ot.65_over","heartdis.his.65_over",
                          "cancer.wh.65_over", "cancer.bl.65_over","cancer.ot.65_over","cancer.his.65_over")
# change col names
names(leadingCause_dat)<-nms_leadingCause_dat
head(leadingCause_dat)

summary(leadingCause_dat)

# find column names with heartdis pattern
heartdis.nms <-  grep(pattern = "heartdis", x = names(leadingCause_dat), value = TRUE)
# subset on leadingCause
heartdis_dat <- leadingCause_dat[, c(heartdis.nms)]
# add column with county info
heartdis_dat <- cbind(leadingCause_dat[,c(1:3)] , leadingCause_dat[, c(heartdis.nms)])

#heartdis_dat <- leadingCause_dat[, c(1:3,heartdis.nms)]
#heartdis_dat <- cbind(leadingCause_dat[,c(1:3)], leadingCause_dat[,c(heartdis_dat)])


# cleaned data with heart disease information --> remove nonzero rows
# not many data points!!
heartdis_dat <- with(heartdis_dat, subset(heartdis_dat, 
                                    (heartdis.wh.25_44>0) & (heartdis.bl.25_44>0) & (heartdis.ot.25_44>0) & (heartdis.his.25_44>0) & 
                                    (heartdis.wh.45_64>0) & (heartdis.bl.45_64 >0) & (heartdis.ot.45_64>0) & (heartdis.his.45_64>0) &
                                    (heartdis.wh.65_over>0) & (heartdis.bl.65_over>0) & (heartdis.ot.65_over>0) & (heartdis.his.65_over>0)  
                                    ) )




xtabs(heartdis.bl.25_44 ~ state.name, exclude = c(-1111), data = heartdis_dat)

# subset data for values >0 to exclude the -1111 and -2222 values
# leadingCause_dat <- with(leadingCause_dat, subset(leadingCause_dat, subset= c(4:5)>0, select = c(1:35))) # & 
#                                     (few.fruit>0) & (obesity>0) & (high.blood>0) & 
#                                     (diabetes>0) & (no.ins>0)))



# clean up -1111 and -2222 values 
leadCauseData <- as.data.frame(lapply(leadCauseData, function(x){
  replace(x, x < 0, 0) 
}))

str(leadCauseData) # too many columns
summary(leadCauseData)



# -------------------------------------------------------------------------------------------------------------------------
# Load risk Data and clean
# -------------------------------------------------------------------------------------------------------------------------
# Load risk data and clean
riskData <- read.csv('~/dev/Rstudio/data/RISKFACTORSANDACCESSTOCARE.csv')
summary(riskData)

# remove first 2 columns and 4th
riskData <- riskData[][-(1:2)]
riskData <- riskData[][-(4)]


# subset only certain columns
risk_dat <- subset(riskData, select = c(CHSI_County_Name:CHSI_State_Abbr, 
                                        No_Exercise, Few_Fruit_Veg, Obesity, 
                                        High_Blood_Pres, Diabetes, Uninsured))
# change names
nms_risk <- c("county.name","state.name","state.abbr","no.exercise","few.fruit","obesity","high.blood","diabetes","no.ins")
names(risk_dat)<- nms_risk

# subset data for values >0 to exclude the -1111 and -2222 values
risk_dat <- with(risk_dat, subset(risk_dat, (no.exercise>0) & 
                                    (few.fruit>0) & (obesity>0) & (high.blood>0) & 
                                    (diabetes>0) & (no.ins>0)))


# create function to lower, will use on dataframe
lower.df = function(v) 
{
  if(is.character(v)) return(tolower(v)) 
  else return(v)
}
# use lower letters across all dataframes - easier for merging data
risk_dat <- data.frame(lapply(risk_dat, lower.df))

# clean up -1111 and -2222 values --> should just remove the data instead!!
# riskData <- as.data.frame(lapply(riskData, function(x){
#   replace(x, x<0, 0)
# }))

# counting missing values
count(risk_dat2$No_Exercise[risk_dat2$No_Exercise<0])
