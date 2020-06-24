# a code to help transform an ICIS data pull into something useful for RPA analyses

library(readxl)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(lubridate)

#attempt to turn off scientific notation
options(scipen=999)

#load ICIS pull

data<-read_excel("//deqhq1/WQ-Share/WQPPD/NPDES Permit Issuance/101081 IP Springfield/2- Permit Development/Data+RPA/101081-DATA-ICISrawpull-20200624.xlsx",skip=4,
                 col_types=c("text","text","text","date","date",
                             "text","text","text","numeric","text","text","text"))

#convert names so that they are usable
names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))

#remove data where Result is blank
data<-subset(data,!(is.na(Result)))

#change data where Result is "." to "0", for some reason the export removes all preceeding and trailing 0s (so "0.3" becomes ".3", etc)
data$Result<-ifelse(data$Result==".",0,data$Result)


#examine NODI issues
prob<-subset(data,!(is.na(NODI.Descriptor)))

#For results where Sampling.Frequency is NA, change to "Unknown"
data$Sampling.Frequency<-ifelse(is.na(data$Sampling.Frequency),"Unknown",data$Sampling.Frequency)

#save a copy of the data sheet for the excel output before we make too many more data transformations
data1<-data

##need to convert farenheit tempt to celcius
data$Result<-case_when(data$Unit=="deg F" & data$Parameter.Desc=='Temperature, water deg. fahrenheit'~((data$Result-32)*0.5556),
                       !(data$Unit=="deg F" & data$Parameter.Desc=='Temperature, water deg. fahrenheit')~data$Result)
data$Unit<-case_when(data$Unit=="deg F" & data$Parameter.Desc=='Temperature, water deg. fahrenheit'~"deg C",
                     !(data$Unit=="deg F" & data$Parameter.Desc=='Temperature, water deg. fahrenheit')~data$Unit)
data$Parameter.Desc<-case_when(data$Parameter.Desc=='Temperature, water deg. fahrenheit'~"Temperature, water deg. centigrade",
                     !(data$Parameter.Desc=='Temperature, water deg. fahrenheit')~data$Parameter.Desc)

##################Summarizing Data#################################
#create maximum values dataset
maxs<-aggregate(Result~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit,data,max)
#change column name to maximum
names(maxs)<-c("Parameter.Desc","Location.Description","Outfall","Statistic.Description","Sampling.Frequency","Unit","Maximum")
dist<-unique(subset(data,select=c( "NPDES.ID", "Location.Description","Outfall","Parameter.Desc", "Statistic.Description","Sampling.Frequency","Unit")))
newmax<-merge(dist,maxs,all=TRUE)

#let's get minimums too
mins<-aggregate(Result~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit,data,min)
#change column name to maximum
names(mins)<-c("Parameter.Desc","Location.Description","Outfall","Statistic.Description","Sampling.Frequency","Unit","Minimum")
newmax<-merge(newmax,mins,all=TRUE)

#calculate 10th and 90th percentiles
ninperc<-aggregate(Result~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit,data,FUN='quantile', probs =.90)

#change column name to Ninety_Perc
names(ninperc)<-c("Parameter.Desc","Location.Description","Outfall","Statistic.Description","Sampling.Frequency","Unit","Ninety_Perc")

tenp<-aggregate(Result~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit,data,FUN='quantile', probs =.10)

#change column name to Ten_Perc
names(tenp)<-c("Parameter.Desc","Location.Description","Outfall","Statistic.Description","Sampling.Frequency","Unit","Ten_Perc")

#merge into main table
newmax<-merge(ninperc,newmax,all=TRUE)
newmax<-merge(tenp,newmax,all=TRUE)

#estimate # of samples taken
#calculate # of observations per type
obs<-data %>% count(Parameter.Desc,Location.Description,Outfall,Statistic.Description,Sampling.Frequency,Unit)
#merge in with our max data table
newmax<-merge(obs,newmax,all=TRUE)


#calculate estimated # of samples taken (if they were consistent)
#since not every month has the exact same number of days, use the average number of days in a month (30.42). 
#This won't account for leap years, but if they are sampling daily then they will have so many samples this shouldn't be much of an issue
#average of 4.35 weeks in a month over the course of a year
newmax$est.samp<-case_when(newmax$Sampling.Frequency=="Twice per Week"~round(2*4.35*newmax$n,0),
                           newmax$Sampling.Frequency=="Daily" ~round(30.42*newmax$n,0),
                           newmax$Sampling.Frequency=="Weekly" ~round(4.35*newmax$n,0),
                           newmax$Sampling.Frequency=="Monthly"~ as.numeric(newmax$n),
                           newmax$Sampling.Frequency=="Quarterly"~as.numeric(newmax$n),
                           newmax$Sampling.Frequency=="Twice per Year"~as.numeric(newmax$n),
                           newmax$Sampling.Frequency=="Three per Week"~round(3*4.35*newmax$n,0),
                           newmax$Sampling.Frequency=="Five per Week"~round(5*4.35*newmax$n,0),
                           newmax$Sampling.Frequency=="Twice per Month"~round(2*newmax$n,0)
)


#need to add coefficient of variation
#for monthly sampling frequency, we can calculate it, but for anything else we can assume 0.6 (TSD Appendix E-3)
summary<-data %>%
  group_by(Parameter.Desc, Location.Description, Outfall, Statistic.Description, Sampling.Frequency, Unit) %>%
  summarise(avg = mean(Result), stdev = sd(Result)) %>%
  mutate (CV = round(stdev/avg,2))

newmax<-merge(summary,newmax)

newmax$CV<-case_when(newmax$Sampling.Frequency=="Monthly"~newmax$CV,
                     !(newmax$Sampling.Frequency=="Monthly")~0.6)




#table of summary of all parameters
sumdat<-subset(newmax,
               select=c("Location.Description","Outfall","NPDES.ID","Parameter.Desc","Statistic.Description",
                        "Sampling.Frequency","n","est.samp",
                        "Maximum","Minimum","avg","Ninety_Perc","Ten_Perc","Unit","CV")
               )

#create table for export to be used with RPA
#chlorine, pH, 
#for any toxics- will need to examine the summary of all parameters table
rpabasics<-subset(newmax,Parameter.Desc %in% c('Chlorine, total residual',"pH",
                                            'Temperature, water deg. centigrade',
                                            'Alkalinity, total [as CaCO3]') & !(Location.Description %in% "Raw Sewage Influent"),
               select=c("Location.Description","Outfall","NPDES.ID","Parameter.Desc","Statistic.Description",
                        "Sampling.Frequency","n","est.samp",
                        "Maximum","Minimum","avg","Ninety_Perc","Ten_Perc","Unit","CV")
               )


#########################AMMONIA RPA WORK #######################################
#for ammonia RPA, want seasonal information
#add seasonal column, define summer as May-October and winter as Nov-April
#use monitoring period end date to define the month
data$month<-month(data$Monitoring.Period.End.Date)

data$season<-ifelse(data$month %in% c(5,6,7,8,9,10),"summer","winter")

#get data for ammonia RPA
amm<-subset(data,Parameter.Desc %in% c("pH","Nitrogen, ammonia total [as N]",
                                       'Temperature, water deg. centigrade', 'Alkalinity, total [as CaCO3]')
            &  !(Unit %in% "lb/d"))

#create maximum values dataset
ammmaxs<-aggregate(Result~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit+season,amm,max)
#change column name to maximum
names(ammmaxs)<-c("Parameter.Desc","Location.Description","Outfall","Statistic.Description","Sampling.Frequency","Unit","season","Maximum")
dist<-unique(subset(amm,select=c( "NPDES.ID", "Location.Description","Parameter.Desc", "Statistic.Description","Sampling.Frequency","Unit","season")))
amstat<-merge(dist,ammmaxs,all=TRUE)

#let's get minimums too
ammins<-aggregate(Result~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit+season,amm,min)
#change column name to maximum
names(ammins)<-c("Parameter.Desc","Location.Description","Outfall","Statistic.Description","Sampling.Frequency","Unit","season","Minimum")
amstat<-merge(amstat,ammins,all=TRUE)


#estimate # of samples taken
#calculate # of observations per type
ammobs<-amm %>% count(Parameter.Desc,Location.Description,Outfall,Statistic.Description,Sampling.Frequency,Unit,season)
#merge in with our max data table
amstat<-merge(ammobs,amstat,all=TRUE)

#calculate 10th and 90th percentiles
ninetyperc<-aggregate(Result~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit+season,amm,FUN='quantile', probs =.90)

#change column name to Ninety_Perc
names(ninetyperc)<-c("Parameter.Desc","Location.Description","Outfall","Statistic.Description","Sampling.Frequency","Unit","season","Ninety_Perc")

tenperc<-aggregate(Result~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit+season,amm,FUN='quantile', probs =.10)

#change column name to Ten_Perc
names(tenperc)<-c("Parameter.Desc","Location.Description","Outfall","Statistic.Description","Sampling.Frequency","Unit","season","Ten_Perc")

#merge into main ammonia table
amstat<-merge(ninetyperc,amstat,all=TRUE)
amstat<-merge(tenperc,amstat,all=TRUE)


#calculate estimated # of samples taken (if they were consistent)
#since not every month has the exact same number of days, use the average number of days in a month (30.42). 
#This won't account for leap years, but if they are sampling daily then they will have so many samples this shouldn't be much of an issue
#average of 4.35 weeks in a month over the course of a year
amstat$est.samp<-case_when(amstat$Sampling.Frequency=="Twice per Week"~round(2*4.35*amstat$n,0),
                           amstat$Sampling.Frequency=="Daily" ~round(30.42*amstat$n,0),
                           amstat$Sampling.Frequency=="Weekly" ~round(4.35*amstat$n,0),
                           amstat$Sampling.Frequency=="Monthly"~ as.numeric(amstat$n),
                           amstat$Sampling.Frequency=="Quarterly"~as.numeric(amstat$n),
                           amstat$Sampling.Frequency=="Twice per Year"~as.numeric(amstat$n),
                           amstat$Sampling.Frequency=="Three per Week"~round(3*4.35*amstat$n,0),
                           amstat$Sampling.Frequency=="Five per Week"~round(5*4.35*amstat$n,0),
                           amstat$Sampling.Frequency=="Twice per Month"~round(2*amstat$n,0)
)

#need to add coefficient of variation
#for monthly sampling frequency, we can calculate it, but for anything else we can assume 0.6 (TSD Appendix E-3)
amsum<-amm %>%
  group_by(Parameter.Desc, Location.Description, Outfall, Statistic.Description, Sampling.Frequency, Unit,season) %>%
  summarise(avg = mean(Result), stdev = sd(Result)) %>%
  mutate (CV = round(stdev/avg,2))

amstat<-merge(amsum,amstat)

amstat$CV<-case_when(amstat$Sampling.Frequency %in% c("Monthly","Quarterly","Twice per Year")~amstat$CV,
                     !(amstat$Sampling.Frequency %in% c("Monthly","Quarterly","Twice per Year"))~0.6)

#create table for ammonia RPA
ammrp<-subset(amstat,Statistic.Description %in% c("Monthly Average","Daily Maximum","Maximum","Minimum","Daily Minimum","Maximum Value"),
                  select=c("Location.Description","Outfall","NPDES.ID","Parameter.Desc","Statistic.Description",
                           "season","Sampling.Frequency","n","est.samp",
                           "Maximum","Minimum","avg","Ninety_Perc","Ten_Perc","Unit","CV"))

#sort
ammrp<-ammrp[order(ammrp$Parameter.Desc,ammrp$season,ammrp$Statistic.Description),]

#create another table for ammonia, this one not seasonal
ammtot<-subset(sumdat,Parameter.Desc %in% c("pH","Nitrogen, ammonia total [as N]",
                                            'Temperature, water deg. centigrade', 'Alkalinity, total [as CaCO3]')
               & Statistic.Description %in% c("Monthly Average","Daily Maximum","Maximum","Minimum","Daily Minimum","Maximum Value")
               & !(Unit %in% 'lb/d'),
               select=c("Location.Description","Outfall","NPDES.ID","Parameter.Desc","Statistic.Description",
                               "Sampling.Frequency","n","est.samp",
                               "Maximum","avg","Ninety_Perc","Ten_Perc","Unit","CV"))


##########################################EXCEL EXPORT###########################
#create workbook to be exported
wb<-createWorkbook()

#create styles

#sheet of Chlorine and pH RPA-relevant info
addWorksheet(wb,"pH and Chlorine RPA")
writeData(wb,sheet="pH and Chlorine RPA",startRow=1, x="Summary statistics for pH and Chlorine RPA analysis")
writeData(wb,sheet="pH and Chlorine RPA",startRow=2, x="CV only calculated for data with a monthly or less monitoring frequency, otherwise a CV of 0.6 is assumed")
writeData(wb,sheet="pH and Chlorine RPA",startRow=3, x="average, 10th percentile, and 90th percentile are calculated using n, not est.samp")
writeData(wb,sheet="pH and Chlorine RPA",x=rpabasics,startCol=1, startRow=5)

#sheet of Ammonia RPA relevant info
addWorksheet(wb,"Ammonia RPA")
writeData(wb,sheet="Ammonia RPA",startRow=1,x="Summary statistics for Ammonia RPA")
writeData(wb,sheet="Ammonia RPA",startRow=2,x="winter=November - April, summer= May - October")
writeData(wb,sheet="Ammonia RPA",startRow=3, x="average, 10th percentile, and 90th percentile are calculated using n, not est.samp")
writeData(wb,sheet="Ammonia RPA",x="Seasonal",startCol=1, startRow=5)
writeData(wb,sheet="Ammonia RPA",x="Year Round",startCol=17,startRow=5)
writeData(wb,sheet="Ammonia RPA",x=ammrp,startCol=1, startRow=6)
writeData(wb,sheet="Ammonia RPA",x=ammtot,startCol=17,startRow=6)

#sheet of all summary 
addWorksheet(wb,"Parameter Summary")
writeData(wb,sheet="Parameter Summary",startRow=1,x="Summary of all reported DMR parameters")
writeData(wb,sheet="Parameter Summary",startRow=2, x="average, 10th percentile, and 90th percentile are calculated using n, not est.samp")
writeData(wb,sheet="Parameter Summary",startRow=4,x=sumdat)

#sheet of original data
addWorksheet(wb,"ICIS Data")
writeData(wb,sheet="ICIS Data",startRow=1,x="Data from ICIS")
writeData(wb,sheet="ICIS Data",startRow=2,x="Note that any data where the result was not reported has been removed")
writeData(wb,sheet="ICIS Data",startRow=3,x="This data has not had any unit transformations done")
writeData(wb,sheet="ICIS Data",startRow=5,x=data1)

saveWorkbook(wb,"C:/COVID-19 WORK/ICIS_Work/ICIS_RPAPrep.xlsx",overwrite=TRUE)









