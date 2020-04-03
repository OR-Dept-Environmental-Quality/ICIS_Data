##code to examine differences between results from summary stats and raw data
##pulled some daily data from the DMS server, compile monthly summary stats like ICIS does
##compare to stats generated from full raw dataset

library(readxl)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(lubridate)
library(ggplot2)

#attempt to turn off scientific notation
options(scipen=999)

#get data from Forest Grove, Canby, Seaside, and Troutdale from 1/1/2012-1/1/2017

data<-read_excel("C:/COVID-19 WORK/ICIS_Work/RPA_Data_Test/All_DMS_2012-2017_AmmoniaRPA.xlsx")

#convert deg F to deg C
data$ObservQty<-case_when(data$UnitAbbr=="°F" & data$ParameterDesc=='Temperature'~((data$ObservQty-32)*0.5556),
                       !(data$UnitAbbr=="°F" & data$ParameterDesc=='Temperature')~data$ObservQty)
data$UnitAbbr<-case_when(data$UnitAbbr=="°F" & data$ParameterDesc=='Temperature'~"°C",
                     !(data$UnitAbbr=="°F" & data$ParameterDesc=='Temperature')~data$UnitAbbr)

#some temp data looks really wrong- remove anything higher than 100 or less than 0 for deg C
data<-subset(data,!(ParameterDesc=="Temperature" & (ObservQty>100|ObservQty<0)))

#some pH also looks really wrong-remove anything higher than 14 or less than 4
data<-subset(data,!(ParameterDesc=="pH" & (ObservQty>14|ObservQty<4)))

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

#minimum of minimum (in case the other 10th percentile stats don't work)
SumStatminmin<-aggregate(Minimum~ParameterDesc + PermitNo + MonPtCat + MonPtCatQual + UnitAbbr,summarydata,min)
names(SumStatminmin)<-c("ParameterDesc", "PermitNo","MonPtCat", "MonPtCatQual","UnitAbbr","SumData_Minimum_of_Minimum")
main<-merge(main,SumStatminmin,all=TRUE)

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

#there are some NaN cases because all the data was only reported in one month
main$SumData_CV<-case_when(main$SumData_Count<=60~main$SumData_CV,
                           main$SumData_Count>60~0.6)

######################Have both datasets compiled in main##############

#figure out percent differences
###if average is the RPA statistics the corresponding summary stat is average of the average

RPAAvg<-subset(main,
               select=c(ParameterDesc,PermitNo,MonPtCat,MonPtCatQual,UnitAbbr,RawData_Count,RawData_CV, RawData_Average,
                        SumData_Average_of_Average))

RPAAvg$avg_percdiff<-(abs(RPAAvg$RawData_Average-RPAAvg$SumData_Average_of_Average)/((RPAAvg$RawData_Average+RPAAvg$SumData_Average_of_Average)/2))*100


#plot data 
ggplot(data=RPAAvg,mapping= aes(y=avg_percdiff, x=RawData_Count,shape=ParameterDesc))+
  geom_point()

###if maximum is the RPA statistic we are interested in
RPAMax<-subset(main,
               select=c(ParameterDesc,PermitNo,MonPtCat,MonPtCatQual,UnitAbbr,RawData_Count,RawData_CV, 
                        RawData_Maximum,SumData_Maximum_of_Maximum, SumData_Maximum_Average,
                        SumData_Maximum_Weekly_Average))

#compare with maximum of maximum
RPAMax$max_percdiffmaxmax<-(abs(RPAMax$RawData_Maximum-RPAMax$SumData_Maximum_of_Maximum)/((RPAMax$RawData_Maximum+RPAMax$SumData_Maximum_of_Maximum)/2))*100

ggplot(data=RPAMax,mapping= aes(y=max_percdiffmaxmax, x=RawData_Count,shape=ParameterDesc))+
  geom_point()

#compare with maximum of maximum weekly average
RPAMax$max_percdiffmaxwkavg<-(abs(RPAMax$RawData_Maximum-RPAMax$SumData_Maximum_Weekly_Average)/((RPAMax$RawData_Maximum+RPAMax$SumData_Maximum_Weekly_Average)/2))*100

ggplot(data=RPAMax,mapping= aes(y=max_percdiffmaxwkavg, x=RawData_Count,shape=ParameterDesc))+
  geom_point()

#compare with maximum of the average
RPAMax$max_percdiffmaxavg<-(abs(RPAMax$RawData_Maximum-RPAMax$SumData_Maximum_Average)/((RPAMax$RawData_Maximum+RPAMax$SumData_Maximum_Average)/2))*100  
  
ggplot(data=RPAMax,mapping= aes(y=max_percdiffmaxavg, x=RawData_Count,shape=ParameterDesc))+
  geom_point()

#####10th percentile RPA statistic
RPATen<-subset(main,
              select=c(ParameterDesc,PermitNo,MonPtCat,MonPtCatQual,UnitAbbr,RawData_Count,RawData_CV, 
                       RawData_TenPercentile, SumData_AvgMinimum,SumData_Minimum_Average,SumData_Minimum_of_Minimum))

#compare with the average of the minimum
RPATen$ten_percdiffavgmin<-(abs(RPATen$RawData_TenPercentile-RPATen$SumData_AvgMinimum)/((RPATen$RawData_TenPercentile+RPATen$SumData_AvgMinimum)/2))*100  

ggplot(data=RPATen,mapping= aes(y=ten_percdiffavgmin, x=RawData_Count,shape=ParameterDesc,color=ParameterDesc))+
  geom_point()

#compare with the minimum of the average
RPATen$ten_percdiffminavg<-(abs(RPATen$RawData_TenPercentile-RPATen$SumData_Minimum_Average)/((RPATen$RawData_TenPercentile+RPATen$SumData_Minimum_Average)/2))*100  

#one NaN due to 0/0- there was no difference so change the NaN to 0
RPATen$ten_percdiffminavg<-ifelse(is.na(RPATen$ten_percdiffminavg),0,RPATen$ten_percdiffminavg)

ggplot(data=RPATen,mapping= aes(y=ten_percdiffminavg, x=RawData_Count,shape=ParameterDesc,color=ParameterDesc))+
  geom_point()

#compare with minimum of minimum
RPATen$ten_percdiffminmin<-(abs(RPATen$RawData_TenPercentile-RPATen$SumData_Minimum_of_Minimum)/((RPATen$RawData_TenPercentile+RPATen$SumData_Minimum_of_Minimum)/2))*100  

#two NaN due to 0/0- there was no difference so change the NaN to 0
RPATen$ten_percdiffminmin<-ifelse(is.na(RPATen$ten_percdiffminmin),0,RPATen$ten_percdiffminmin)

ggplot(data=RPATen,mapping= aes(y=ten_percdiffminmin, x=RawData_Count,shape=ParameterDesc,color=ParameterDesc))+
  geom_point()

###90th percentile summary statistic
RPA90<-subset(main, 
              select=c(ParameterDesc,PermitNo,MonPtCat,MonPtCatQual,UnitAbbr,RawData_Count,RawData_CV, 
                       RawData_Ninety_Percentile,SumData_AvgMaximum,SumData_Maximum_Average,SumData_Maximum_Weekly_Average))

#compare with average of the maximum
RPA90$percdiff90avgmax<-(abs(RPA90$RawData_Ninety_Percentile-RPA90$SumData_AvgMaximum)/((RPA90$RawData_Ninety_Percentile+RPA90$SumData_AvgMaximum)/2))*100  

ggplot(data=RPA90,mapping= aes(y=percdiff90avgmax, x=RawData_Count,shape=ParameterDesc,color=ParameterDesc))+
  geom_point()

#compare with maximum of the average
RPA90$percdiff90maxavg<-(abs(RPA90$RawData_Ninety_Percentile-RPA90$SumData_Maximum_Average)/((RPA90$RawData_Ninety_Percentile+RPA90$SumData_Maximum_Average)/2))*100  

ggplot(data=RPA90,mapping= aes(y=percdiff90maxavg, x=RawData_Count,shape=ParameterDesc,color=ParameterDesc))+
  geom_point()

#compare with maximum of the weekly average
RPA90$percdiff90maxwkavg<-(abs(RPA90$RawData_Ninety_Percentile-RPA90$SumData_Maximum_Weekly_Average)/((RPA90$RawData_Ninety_Percentile+RPA90$SumData_Maximum_Weekly_Average)/2))*100  

ggplot(data=RPA90,mapping= aes(y=percdiff90maxwkavg, x=RawData_Count,shape=ParameterDesc,color=ParameterDesc))+
  geom_point()

######have graphs, but want to see what how much is within 10%
#merge all the RPAstat tables together
RPA<-merge(RPAAvg,RPAMax,all=TRUE)
RPA<-merge(RPA,RPA90,all=TRUE)
RPA<-merge(RPA,RPATen,all=TRUE)

#just get the important columns
RPA<-subset(RPA,select=c("ParameterDesc","avg_percdiff","max_percdiffmaxmax","max_percdiffmaxwkavg",
                         "max_percdiffmaxavg","percdiff90avgmax","percdiff90maxavg","percdiff90maxwkavg",
                         "ten_percdiffavgmin","ten_percdiffminavg","ten_percdiffminmin"))

#convert to longtable format to make it easier to use
RPA1<-gather(RPA,Statistic,percent_diff,avg_percdiff:ten_percdiffminmin)


counts<-RPA1 %>%
  group_by(ParameterDesc,Statistic)%>%
  summarise(Below10perc=sum(ifelse(percent_diff<=10,1,0)),
            Above10perc=sum(ifelse(percent_diff>10,1,0)), 
            total=Above10perc+Below10perc) %>%
  mutate (percent_below10perc=(Below10perc/total)*100)

############################Let's see how this affects actual ammonia RPA#################

#get permittees with full data set
fullset<-main %>%
  group_by(PermitNo) %>%
  mutate(complete = ifelse(sum(ifelse(!is.na(PermitNo),1,0))==4, "Yes", "No"))

fullset<-subset(fullset,complete=="Yes")
#only 5 permittees have full data set. Just plug numbers into RPA sheet instead of trying to recreate RPA in R
  
#time to export to excel
wb<-createWorkbook()
addWorksheet(wb,"Percent_Differences")
writeData(wb,sheet="Percent_Differences",x=counts)
addWorksheet(wb,"Raw_vs_Summary")
writeData(wb,sheet="Raw_vs_Summary",x=main)
addWorksheet(wb,"CompleteDataset")
writeData(wb,sheet="CompleteDataset",x=fullset)

saveWorkbook(wb,"C:/COVID-19 WORK/ICIS_Work/SummaryStatCompare.xlsx",overwrite=TRUE)