# a code to help transform an ICIS data pull into something useful for RPA analyses

library(readxl)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(lubridate)

#attempt to turn off scientific notation
options(scipen=999)

#load ICIS pull

data<-read_excel("//Deqhq1/abrits/R/DMRAlianaDataPull_ForestGrove.xlsx",skip=4,
                 col_types=c("text","text","text","date","date",
                             "text","text","text","numeric","text","text","text"))

#convert names so that they are usable
names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))

#remove data where Result is blank
data<-subset(data,!(is.na(Result)))

#remove data where Result is "."
data<-subset(data,!(Result=="."))

#remove data where Result is "0.00", this is a transcription error and should be blank
data<-subset(data,!(Result==0.000))

#examine NODI issues
prob<-subset(data,!(is.na(NODI.Descriptor)))

#For results where Sampling.Frequency is NA, change to "Unknown"
data$Sampling.Frequency<-ifelse(is.na(data$Sampling.Frequency),"Unknown",data$Sampling.Frequency)

#create maximum values dataset
maxs<-aggregate(Result~Parameter.Desc + Location.Description+Statistic.Description+Sampling.Frequency+Unit,data,max)
#change column name to maximum
names(maxs)<-c("Parameter.Desc","Location.Description","Statistic.Description","Sampling.Frequency","Unit","Maximum")
dist<-unique(subset(data,select=c( "NPDES.ID", "Location.Description","Parameter.Desc", "Statistic.Description","Sampling.Frequency","Unit")))
newmax<-merge(dist,maxs,all=TRUE)

#let's get minimums too
mins<-aggregate(Result~Parameter.Desc + Location.Description+Statistic.Description+Sampling.Frequency+Unit,data,min)
#change column name to maximum
names(mins)<-c("Parameter.Desc","Location.Description","Statistic.Description","Sampling.Frequency","Unit","Minimum")
newmax<-merge(newmax,mins,all=TRUE)

#estimate # of samples taken
#calculate # of observations per type
obs<-data %>% count(Parameter.Desc,Location.Description,Statistic.Description,Sampling.Frequency,Unit)
#merge in with our max data table
newmax<-merge(obs,newmax,all=TRUE)


#calculate estimated # of samples taken (if they were consistent)
#since not every month has the exact same number of days, use the average number of days in a month (30.42). 
#This won't account for leap years, but if they are sampling daily then they will have so many samples this shouldn't be much of an issue
newmax$est.samp<-case_when(newmax$Sampling.Frequency=="Twice per Week"~8*newmax$n,
                           newmax$Sampling.Frequency=="Daily" ~round(30.42*newmax$n,0),
                           newmax$Sampling.Frequency=="Weekly" ~4*newmax$n,
                           newmax$Sampling.Frequency=="Monthly"~ as.numeric(newmax$n),
                           newmax$Sampling.Frequency=="Quarterly"~as.numeric(newmax$n),
                           newmax$Sampling.Frequency=="Twice per Year"~as.numeric(newmax$n),
                           newmax$Sampling.Frequency=="Three per Week"~12*newmax$n)

#need to add coefficient of variation
#for monthly sampling frequency, we can calculate it, but for anything else we can assume 0.6 (TSD Appendix E-3)
summary<-data %>%
  group_by(Parameter.Desc, Location.Description, Statistic.Description, Sampling.Frequency, Unit) %>%
  summarise(avg = mean(Result), stdev = sd(Result)) %>%
  mutate (CV = round(stdev/avg,2))

newmax<-merge(summary,newmax)

newmax$CV<-case_when(newmax$Sampling.Frequency=="Monthly"~newmax$CV,
                     !(newmax$Sampling.Frequency=="Monthly")~0.6)


#convert mg/L to ug/L
newmax$Maximum<-ifelse(newmax$Unit=="mg/L" & newmax$Parameter.Desc=='Chlorine, total residual',newmax$Maximum*1000,newmax$Maximum)
newmax$Minimum<-ifelse(newmax$Unit=="mg/L" & newmax$Parameter.Desc=='Chlorine, total residual',newmax$Minimum*1000,newmax$Minimum)
newmax$Unit<-ifelse(newmax$Unit=="mg/L" & newmax$Parameter.Desc=='Chlorine, total residual',"ug/L",newmax$Unit)
newmax$avg<-ifelse(newmax$Unit=="mg/L" & newmax$Parameter.Desc=='Chlorine, total residual',"ug/L",newmax$avg)


#table of summary of all parameters
sumdat<-subset(newmax,
               select=c("Location.Description","NPDES.ID","Parameter.Desc","Statistic.Description",
                        "Sampling.Frequency","n","est.samp",
                        "Maximum","Minimum","avg","Unit","CV")
               )

#create table for export to be used with RPA
#chlorine, pH, 
#for any toxics- will need to examine the summary of all parameters table
rpabasics<-subset(newmax,Parameter.Desc %in% c('Chlorine, total residual',"pH",
                                            'Temperature, water deg. centigrade',
                                            'Alkalinity, total [as CaCO3]') & !(Location.Description %in% "Raw Sewage Influent"),
               select=c("Location.Description","NPDES.ID","Parameter.Desc","Statistic.Description",
                        "Sampling.Frequency","n","est.samp",
                        "Maximum","Minimum","avg","Unit","CV")
               )

#for ammonia RPA, want seasonal information
#add seasonal column, define summer as May-October and winter as Nov-April
#use monitoring period end date to define the month
data$month<-month(data$Monitoring.Period.End.Date)

data$season<-ifelse(data$month %in% c(5,6,7,8,9,10),"summer","winter")

######
#get data for ammonia RPA
amm<-subset(data,Parameter.Desc %in% c("pH","Nitrogen, ammonia total [as N]",
                                       'Temperature, water deg. centigrade', 'Alkalinity, total [as CaCO3]')
            & !(Location.Description %in% "Raw Sewage Influent"))

#create maximum values dataset
ammmaxs<-aggregate(Result~Parameter.Desc + Location.Description+Statistic.Description+Sampling.Frequency+Unit+season,amm,max)
#change column name to maximum
names(ammmaxs)<-c("Parameter.Desc","Location.Description","Statistic.Description","Sampling.Frequency","Unit","season","Maximum")
dist<-unique(subset(amm,select=c( "NPDES.ID", "Location.Description","Parameter.Desc", "Statistic.Description","Sampling.Frequency","Unit","season")))
ammmax<-merge(dist,ammmaxs,all=TRUE)

#let's get minimums too
ammins<-aggregate(Result~Parameter.Desc + Location.Description+Statistic.Description+Sampling.Frequency+Unit+season,amm,min)
#change column name to maximum
names(ammins)<-c("Parameter.Desc","Location.Description","Statistic.Description","Sampling.Frequency","Unit","season","Minimum")
ammmax<-merge(ammmax,ammins,all=TRUE)



#estimate # of samples taken
#calculate # of observations per type
ammobs<-amm %>% count(Parameter.Desc,Location.Description,Statistic.Description,Sampling.Frequency,Unit,season)
#merge in with our max data table
ammmax<-merge(ammobs,ammmax,all=TRUE)

#for monthly, quarterly, and twice per year data calculate 10th and 90th percentiles

ninetyperc<-aggregate(Result~Parameter.Desc + Location.Description+Statistic.Description+Sampling.Frequency+Unit+season,amm,FUN='quantile', probs =.90)
  
tenperc<-aggregate(Result~Parameter.Desc + Location.Description+Statistic.Description+Sampling.Frequency+Unit+season,amm,FUN='quantile', probs =.10)

#calculate estimated # of samples taken (if they were consistent)
#since not every month has the exact same number of days, use the average number of days in a month (30.42). 
#This won't account for leap years, but if they are sampling daily then they will have so many samples this shouldn't be much of an issue
ammmax$est.samp<-case_when(ammmax$Sampling.Frequency=="Twice per Week"~8*ammmax$n,
                           ammmax$Sampling.Frequency=="Daily" ~round(30.42*ammmax$n,0),
                           ammmax$Sampling.Frequency=="Weekly" ~4*ammmax$n,
                           ammmax$Sampling.Frequency=="Monthly"~ as.numeric(ammmax$n),
                           ammmax$Sampling.Frequency=="Quarterly"~as.numeric(ammmax$n),
                           ammmax$Sampling.Frequency=="Twice per Year"~as.numeric(ammmax$n),
                           ammmax$Sampling.Frequency=="Three per Week"~12*ammmax$n)

#need to add coefficient of variation
#for monthly sampling frequency, we can calculate it, but for anything else we can assume 0.6 (TSD Appendix E-3)
amsum<-amm %>%
  group_by(Parameter.Desc, Location.Description, Statistic.Description, Sampling.Frequency, Unit,season) %>%
  summarise(avg = mean(Result), stdev = sd(Result)) %>%
  mutate (CV = round(stdev/avg,2))

ammmax<-merge(amsum,ammmax)

ammmax$CV<-case_when(ammmax$Sampling.Frequency %in% c("Monthly","Quarterly","Twice per Year")~ammmax$CV,
                     !(ammmax$Sampling.Frequency %in% c("Monthly","Quarterly","Twice per Year"))~0.6)

#create table for ammonia RPA
ammrp<-subset(ammmax,
                  select=c("Location.Description","NPDES.ID","Parameter.Desc","Statistic.Description",
                           "season","Sampling.Frequency","n","est.samp",
                           "Maximum","Minimum","avg","Unit","CV")
)

#sort
ammrp<-ammrp[order(ammrp$Parameter.Desc,ammrp$season),]

#create another table for ammonia, this one not seasonal
ammtot<-subset(sumdat,Parameter.Desc %in% c("pH","Nitrogen, ammonia total [as N]",
                                            'Temperature, water deg. centigrade', 'Alkalinity, total [as CaCO3]')
               & !(Location.Description %in% "Raw Sewage Influent"),
               select=c("Location.Description","NPDES.ID","Parameter.Desc","Statistic.Description",
                               "Sampling.Frequency","n","est.samp",
                               "Maximum","avg","Unit","CV")
)


##########################################EXCEL EXPORT###########################
#create workbook to be exported
wb<-createWorkbook()

#create styles

#sheet of Chlorine and pH RPA-relevant info
addWorksheet(wb,"pH and Chlorine RPA")
writeData(wb,sheet="pH and Chlorine RPA",startRow=1, x="Summary statistics for pH and Chlorine RPA analysis")
writeData(wb,sheet="pH and Chlorine RPA",startRow=2, x="CV only calculated for monthly data, otherwise a CV of 0.6 is assumed")
writeData(wb,sheet="pH and Chlorine RPA",startRow=3, x="avg is calculated using n, not est.samp")
writeData(wb,sheet="pH and Chlorine RPA",x=rpabasics,startCol=1, startRow=5)

#sheet of Ammonia RPA relevant info
addWorksheet(wb,"Ammonia RPA")
writeData(wb,sheet="Ammonia RPA",startRow=1,x="Summary statistics for Ammonia RPA")
writeData(wb,sheet="Ammonia RPA",startRow=2,x="winter=November - April, summer= May - October")
writeData(wb,sheet="Ammonia RPA",startRow=3, x="avg is calculated using n, not est.samp")
writeData(wb,sheet="Ammonia RPA",x="Seasonal",startCol=1, startRow=5)
writeData(wb,sheet="Ammonia RPA",x="Year Round",startCol=15,startRow=5)
writeData(wb,sheet="Ammonia RPA",x=ammrp,startCol=1, startRow=6)
writeData(wb,sheet="Ammonia RPA",x=ammtot,startCol=15,startRow=6)

#sheet of all summary 
addWorksheet(wb,"Parameter Summary")
writeData(wb,sheet="Parameter Summary",startRow=1,x="Summary of all DMR parameters")
writeData(wb,sheet="Parameter Summary",startRow=3,x=sumdat)

#sheet of original data
addWorksheet(wb,"ICIS Data")
writeData(wb,sheet="ICIS Data",startRow=1,x="Data from ICIS")
writeData(wb,sheet="ICIS Data",startRow=1,x="Note that any data where the result was not reported has been removed")
writeData(wb,sheet="ICIS Data",startRow=5,x=data)

saveWorkbook(wb,"//Deqhq1/abrits/R/ICIS_RPAPrep.xlsx",overwrite=TRUE)









