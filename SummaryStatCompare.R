##code to examine differences between results from summary stats and raw data
##pulled some daily data from the DMS server, compile monthly summary stats like ICIS does
##compare to stats generated from full raw dataset

library(readxl)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(lubridate)

#attempt to turn off scientific notation
options(scipen=999)

#get data from Forest Grove, Canby, Seaside, and Troutdale from 1/1/2012-1/1/2017

data<-read_excel("C:/COVID-19 WORK/ICIS_Work/RPA_Data_Test/All_DMS_2012-2017_AmmoniaRPA.xlsx")

#convert deg F to deg C
data$ObservQty<-case_when(data$UnitAbbr=="°F" & data$ParameterDesc=='Temperature'~((data$ObservQty-32)*0.5556),
                       !(data$UnitAbbr=="°F" & data$ParameterDesc=='Temperature')~data$ObservQty)
data$UnitAbbr<-case_when(data$UnitAbbr=="°F" & data$ParameterDesc=='Temperature'~"°C",
                     !(data$UnitAbbr=="°F" & data$ParameterDesc=='Temperature')~data$UnitAbbr)

#remove lbs/day data- not needed
data<-subset(data, !(UnitAbbr=='lbs/day'))

#remove data that is already a summary stat
data<-subset(data, is.na(LPM_ParameterModAbbr) & is.na(LPM_1_ParameterModAbbr) & is.na(SummaryTimePrdCd))

#########################Data from Raw Statistics##########################

#create maximum values dataset
maxim<-aggregate(ObservQty~ParameterDesc + PermitNo + MonPtCat + MonPtCatQual + UnitAbbr,data,max)
names(maxim)<-c("ParameterDesc", "PermitNo","MonPtCat", "MonPtCatQual","UnitAbbr","RawData_Maximum")
distinct<-unique(subset(data,select=c(PermitNo, ParameterDesc, MonPtCat, MonPtCatQual, UnitAbbr)))
main<-merge(distinct,maxim,all=TRUE)

#calculate 10th and 90th percentiles
ninperc<-aggregate(ObservQty~ParameterDesc + PermitNo + MonPtCat + MonPtCatQual + UnitAbbr,data,FUN='quantile', probs =.90)

#change column name to Ninety_Perc
names(ninperc)<-c("ParameterDesc", "PermitNo","MonPtCat", "MonPtCatQual","UnitAbbr","RawData_Ninety_Percentile")

tenp<-aggregate(ObservQty~ParameterDesc + PermitNo + MonPtCat + MonPtCatQual + UnitAbbr,data,FUN='quantile', probs =.10)

#change column name to Ten_Perc
names(tenp)<-c("ParameterDesc", "PermitNo","MonPtCat", "MonPtCatQual","UnitAbbr","RawData_TenPercentile")

#merge into main table
main<-merge(ninperc,main,all=TRUE)
main<-merge(tenp,main,all=TRUE)

#number of samples
obs<-data %>% count(ParameterDesc, PermitNo, MonPtCat, MonPtCatQual, UnitAbbr)
names(obs)<-c("ParameterDesc", "PermitNo","MonPtCat", "MonPtCatQual","UnitAbbr","RawData_Count")
#merge in with our max data table
main<-merge(obs,main,all=TRUE)

#need to add coefficient of variation
CV<-data %>%
  group_by(ParameterDesc, PermitNo, MonPtCat, MonPtCatQual, UnitAbbr) %>%
  summarise(RawData_Average = mean(ObservQty), RawData_StandardDev = sd(ObservQty)) %>%
  mutate (RawData_CV = round(RawData_StandardDev/RawData_Average,2))

main<-merge(CV,main)

#############################Calculate monthly statistics to mimic an ICIS pull###########################
#add months
data$datamonth<-month(data$ObservDt)
#add weeks
data$dataweek<-week(data$ObservDt)
data$datayear<-year(data$ObservDt)

#calculate summary stats by month
#Monthly Maximum
summax<-aggregate(ObservQty~ParameterDesc + PermitNo + MonPtCat + MonPtCatQual + UnitAbbr + datamonth+datayear,data,max)
names(summax)<-c("ParameterDesc", "PermitNo","MonPtCat", "MonPtCatQual","UnitAbbr","datamonth","datayear","Maximum")
sumdistinct<-unique(subset(data,select=c(PermitNo, ParameterDesc, MonPtCat, MonPtCatQual, UnitAbbr, datamonth, datayear)))
summarydata<-merge(sumdistinct,summax,all=TRUE)

#Monthly Minimum
summin<-aggregate(ObservQty~ParameterDesc + PermitNo + MonPtCat + MonPtCatQual + UnitAbbr + datamonth + datayear,data,min)
names(summin)<-c("ParameterDesc", "PermitNo","MonPtCat", "MonPtCatQual","UnitAbbr","datamonth","datayear","Minimum")
summarydata<-merge(summarydata,summin,all=TRUE)

#Monthly Average
sumavg<-aggregate(ObservQty~ParameterDesc + PermitNo + MonPtCat + MonPtCatQual + UnitAbbr + datamonth + datayear,data,mean)
names(sumavg)<-c("ParameterDesc", "PermitNo","MonPtCat", "MonPtCatQual","UnitAbbr","datamonth","datayear","Average")
summarydata<-merge(summarydata,sumavg,all=TRUE)

#Maximum Weekly Average
sumwkavg<-aggregate(ObservQty~ParameterDesc + PermitNo + MonPtCat + MonPtCatQual + UnitAbbr + datamonth+datayear+dataweek,data,mean)
maxwkavg<-aggregate(ObservQty~ParameterDesc + PermitNo + MonPtCat + MonPtCatQual + UnitAbbr + datamonth+datayear,sumwkavg,max)
names(maxwkavg)<-c("ParameterDesc", "PermitNo","MonPtCat", "MonPtCatQual","UnitAbbr","datamonth","datayear","Max_Wkly_Avg")
summarydata<-merge(summarydata,maxwkavg,all=TRUE)

#need count of data points
sumcount<-data %>% count(ParameterDesc, PermitNo, MonPtCat, MonPtCatQual, UnitAbbr, datamonth, datayear)
#merge in with our max data table
summarydata<-merge(sumcount,summarydata,all=TRUE)

#####Get Summary stats I would use in RPA- to simulate what my code would do to an ICIS pull

#highest maximum
SumStatMax<-aggregate(Maximum~ParameterDesc + PermitNo + MonPtCat + MonPtCatQual + UnitAbbr,summarydata,max)
names(SumStatMax)<-c("ParameterDesc", "PermitNo","MonPtCat", "MonPtCatQual","UnitAbbr","SumData_Maximum_of_Maximum")
main<-merge(main,SumStatMax,all=TRUE)

#average minimum
SumStatAvgMin<-aggregate(Minimum~ParameterDesc + PermitNo + MonPtCat + MonPtCatQual + UnitAbbr,summarydata,mean)
names(SumStatAvgMin)<-c("ParameterDesc", "PermitNo","MonPtCat", "MonPtCatQual","UnitAbbr","SumData_AvgMinimum")
main<-merge(main,SumStatAvgMin,all=TRUE)

#average maximum
SumStatAvgMax<-aggregate(Maximum~ParameterDesc + PermitNo + MonPtCat + MonPtCatQual + UnitAbbr,summarydata,mean)
names(SumStatAvgMax)<-c("ParameterDesc", "PermitNo","MonPtCat", "MonPtCatQual","UnitAbbr","SumData_AvgMaximum")
main<-merge(main,SumStatAvgMax,all=TRUE)

#highest average
SumStatMaxAvg<-aggregate(Average~ParameterDesc + PermitNo + MonPtCat + MonPtCatQual + UnitAbbr,summarydata,max)
names(SumStatMaxAvg)<-c("ParameterDesc", "PermitNo","MonPtCat", "MonPtCatQual","UnitAbbr","SumData_Maximum_Average")
main<-merge(main,SumStatMaxAvg,all=TRUE)

#lowest average
SumStatMinAvg<-aggregate(Average~ParameterDesc + PermitNo + MonPtCat + MonPtCatQual + UnitAbbr,summarydata,min)
names(SumStatMinAvg)<-c("ParameterDesc", "PermitNo","MonPtCat", "MonPtCatQual","UnitAbbr","SumData_Minimum_Average")
main<-merge(main,SumStatMinAvg,all=TRUE)

#highest maximum average
SumStatMaxMaxAvg<-aggregate(Max_Wkly_Avg~ParameterDesc + PermitNo + MonPtCat + MonPtCatQual + UnitAbbr,summarydata,max)
names(SumStatMaxMaxAvg)<-c("ParameterDesc", "PermitNo","MonPtCat", "MonPtCatQual","UnitAbbr","SumData_Maximum_Weekly_Average")
main<-merge(main,SumStatMaxMaxAvg,all=TRUE)

#average of average
SumStatAvgAvg<-aggregate(Average~ParameterDesc + PermitNo + MonPtCat + MonPtCatQual + UnitAbbr,summarydata,mean)
names(SumStatAvgAvg)<-c("ParameterDesc", "PermitNo","MonPtCat", "MonPtCatQual","UnitAbbr","SumData_Average_of_Average")
main<-merge(main,SumStatAvgAvg,all=TRUE)

#total count
SumStatCount<-aggregate(n~ParameterDesc + PermitNo + MonPtCat + MonPtCatQual + UnitAbbr,summarydata,sum)
names(SumStatCount)<-c("ParameterDesc", "PermitNo","MonPtCat", "MonPtCatQual","UnitAbbr","SumData_Count")
main<-merge(main,SumStatCount,all=TRUE)

# if counts <60, then calculate CV, otherwise assume 0.6
Sum_CV<-summarydata %>%
  group_by(ParameterDesc, PermitNo, MonPtCat, MonPtCatQual, UnitAbbr) %>%
  summarise(SumData_Average = mean(Maximum), SumData_StandardDev = sd(Maximum)) %>%
  mutate (SumData_CV = round(SumData_StandardDev/SumData_Average,2))

main<-merge(main,Sum_CV)
#remove avg and stdev col
main<-subset(main,select=-c(SumData_Average,SumData_StandardDev))

main$SumData_CV<-case_when(main$SumData_Count<=60~main$SumData_CV,
                           main$SumData_Count>60~0.6)


  