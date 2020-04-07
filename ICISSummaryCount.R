#code to examine the types and amounts of summary stats in ICIS 

library(readxl)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(lubridate)

#attempt to turn off scientific notation
options(scipen=999)

#load Summary stat pull (data from all permittees from last 5 years)

data<-read_excel("C:/COVID-19 WORK/ICIS_Work/ICIS_CountofSummaryStats.xlsx",skip=4)

#convert names so that they are usable
names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))

#remove data where DMR Value count is 0
data<-subset(data,DMR.Value>0)

#number of unique permits - total of 244
unique(data$NPDES.ID)

#get number of permittees that are sampling each analyte by summary stat and frequency
sumgrp<-data%>%
group_by(Parameter.Desc,Statistic.Description,Sampling.Frequency)%>%
summarise(Permit_Count= n()) %>%
mutate(Total_Reported=sum(Permit_Count))

#get same summary, but without the sampling frequency
statsum<-data%>%
group_by(Parameter.Desc,Statistic.Description)%>%
summarise(Permit_Count= n()) %>%
mutate(Total_Reported=sum(Permit_Count))

#get summary with sampling frequency, but not statistic description
freqsum<-data%>%
group_by(Parameter.Desc,Sampling.Frequency)%>%
summarise(Permit_Count= n()) %>%
mutate(Total_Reported=sum(Permit_Count))

#transform sampling frequency into either greater than once a month, or less than once a month
data$frequency<-case_when(data$Sampling.Frequency %in% c("Monthly","Quarterly","Once per 2 Months","Annual")
~"Monthly or Less",
data$Sampling.Frequency %in% c("Twice per Week","Once per 2 Weeks","Three per Week",
"Daily","Weekly","Twice per Month","Three Days per Week","Weekdays","Five per Week",
"Once per Hour","Every 1/2 Hour","Seven per Week","Continuous", 
"11 per Month","Four per Month","Nine per Month","Seven per Month",
"Six per Month","Three per Month","24 per Hour","Weekly When Discharging",
"14 per Month","17 per Month","18 per Month","22 per Month",
"23 per Month","24 per Month","26 per Month","Eight per Month",
"19 per Month","21 per Month","28 per Month","29 per Month", 
"10 per Month","12 per Month","20 per Month","25 per Month", 
"27 per Month")~"Greater Than Monthly")

#summary
freqsumgrp<-data%>%
  group_by(Parameter.Desc,Statistic.Description,frequency)%>%
  summarise(Permit_Count= n()) %>%
  mutate(Total_Reported=sum(Permit_Count))

freqgrp<-data%>%
  group_by(Parameter.Desc,frequency)%>%
  summarise(Permit_Count= n()) %>%
  mutate(Total_Reported=sum(Permit_Count))

#transform statistic description into a type
data$stat_type<-case_when(data$Statistic.Description %in% c("Daily Maximum","Maximum","Monthly Maximum",
                                                            "Weekly Maximum","Quarterly Maximum","2 Hour Peak", 
                                                            "Annual Maximum") ~"Maximum",
                          data$Statistic.Description %in% c("Daily Minimum","Minimum","Monthly Minimum",
                                                            "Instantaneous Minimum","Weekly Minimum") ~"Minimum",
                          data$Statistic.Description %in% c("Monthly Average","Weekly Average","1 Hour Average",
                                                            "Average","30 Day Average") ~"Average",
                          data$Statistic.Description %in% c("High 7 Day Average", "Weekly Maximum",
                                                            "Maximum 7 Day Average","Maximum Weekly Average") ~"Maximum Average")

statsumgrp<-data%>%
  group_by(Parameter.Desc,stat_type,frequency)%>%
  summarise(Permit_Count= n()) %>%
  mutate(Total_Reported=sum(Permit_Count))

statgrp<-data%>%
  group_by(Parameter.Desc,stat_type)%>%
  summarise(Permit_Count= n()) %>%
  mutate(Total_Reported=sum(Permit_Count))
