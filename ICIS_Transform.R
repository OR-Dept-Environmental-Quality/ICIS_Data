# a code to help transform an ICIS data pull into something useful for RPA analyses

library(readxl)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(lubridate)
library(ggplot2)

#attempt to turn off scientific notation
options(scipen=999)

#load ICIS pull, note that if you get a bunch of warning messages where it says it is "expecting *x* (date, numeric, etc) in *y*..." it is working, 
# there are just some strange gaps in the raw data pull that R ends up ignoring but don't affect the data

data<-read_excel("//deqhq1/WQ-Share/WQPPD/NPDES Permit Issuance/101773 Brookings STP/2- Permit Development/Data+RPA/101773-DATA-ICISrawpull-20210922.xlsx",skip=4,
                 col_types=c("text","text","text","date","date",
                             "text","text","text","numeric","text","text","text"))

#save pathway so we can save result in same folder
path<-"//deqhq1/WQ-Share/WQPPD/NPDES Permit Issuance/101773 Brookings STP/2- Permit Development/Data+RPA/"

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
#get unique values
dist<-unique(subset(data,select=c( "NPDES.ID", "Location.Description","Outfall","Parameter.Desc", "Statistic.Description","Sampling.Frequency","Unit")))

#create maximum values dataset
maxs<-aggregate(Result~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit,data,max) %>%
  rename(Maximum=Result)

#let's get minimums too
mins<-aggregate(Result~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit,data,min) %>%
  rename(Minimum=Result)

#calculate 10th and 90th percentiles
ninperc<-aggregate(Result~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit,data,FUN='quantile', probs =.90) %>%
  rename(Ninety_Perc=Result)

tenp<-aggregate(Result~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit,data,FUN='quantile', probs =.10) %>%
  rename(Ten_Perc=Result)

#estimate # of samples taken
#calculate # of observations per type
obs<-data %>% count(Parameter.Desc,Location.Description,Outfall,Statistic.Description,Sampling.Frequency,Unit)

#get first observation date and last observation date
start<-aggregate(Monitoring.Period.Start.Date~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit,data,min)
end<-aggregate(Monitoring.Period.End.Date~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit,data,max)

#merge into main table
newmax<-merge(dist,maxs,all=TRUE)
newmax<-merge(newmax,mins,all=TRUE)
newmax<-merge(ninperc,newmax,all=TRUE)
newmax<-merge(tenp,newmax,all=TRUE)
newmax<-merge(obs,newmax,all=TRUE)
newmax<-merge(start,newmax,all=TRUE)
newmax<-merge(end,newmax,all=TRUE)

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
                           newmax$Sampling.Frequency=="Twice per Month"~round(2*newmax$n,0),
                           newmax$Sampling.Frequency=="Once per 2 Weeks"~round(2*newmax$n,0)
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
                        "Maximum","Minimum","avg","Ninety_Perc","Ten_Perc","Unit","CV","Monitoring.Period.Start.Date","Monitoring.Period.End.Date")
               )

#create table for export to be used with RPA
#chlorine, pH, 
#for any toxics- will need to examine the summary of all parameters table
rpabasics<-subset(newmax,Parameter.Desc %in% c('Chlorine, total residual',"Chlorine, free available","pH","pH, maximum",
                                            'Temperature, water deg. centigrade',
                                            'Alkalinity, total [as CaCO3]') & !(Location.Description %in% "Raw Sewage Influent"),
               select=c("Location.Description","Outfall","NPDES.ID","Parameter.Desc","Statistic.Description",
                        "Sampling.Frequency","n","est.samp",
                        "Maximum","Minimum","avg","Ninety_Perc","Ten_Perc","Unit","CV","Monitoring.Period.Start.Date","Monitoring.Period.End.Date")
               )


#########################AMMONIA RPA WORK #######################################
#for ammonia RPA, want seasonal information
#add seasonal column, define summer as May-October and winter as Nov-April
#use monitoring period end date to define the month
data$month<-month(data$Monitoring.Period.End.Date)

data$season<-ifelse(data$month %in% c(5,6,7,8,9,10),"summer","winter")

#get data for ammonia RPA
amm<-subset(data,Parameter.Desc %in% c("pH","Nitrogen, ammonia total [as N]","pH, maximum",
                                       'Temperature, water deg. centigrade', 'Alkalinity, total [as CaCO3]')
            &  !(Unit %in% "lb/d"))

#get unique values
dist<-unique(subset(amm,select=c( "NPDES.ID", "Location.Description","Parameter.Desc", "Statistic.Description","Sampling.Frequency","Unit","season")))

#create maximum values dataset
ammmaxs<-aggregate(Result~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit+season,amm,max) %>%
  rename(Maximum=Result)

#let's get minimums too
ammins<-aggregate(Result~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit+season,amm,min) %>%
  rename(Minimum=Result)

#get first observation date and last observation date
startam<-aggregate(Monitoring.Period.Start.Date~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit+season,amm,min)
endam<-aggregate(Monitoring.Period.End.Date~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit+season,amm,max)

#estimate # of samples taken
#calculate # of observations per type
ammobs<-amm %>% count(Parameter.Desc,Location.Description,Outfall,Statistic.Description,Sampling.Frequency,Unit,season)

#calculate 10th and 90th percentiles
ninetyperc<-aggregate(Result~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit+season,amm,FUN='quantile', probs =.90) %>%
  rename(Ninety_Perc=Result)

tenperc<-aggregate(Result~Parameter.Desc + Location.Description+Outfall+Statistic.Description+Sampling.Frequency+Unit+season,amm,FUN='quantile', probs =.10) %>%
  rename(Ten_Perc=Result)

#merge into main ammonia table
amstat<-merge(dist,ammmaxs,all=TRUE)
amstat<-merge(amstat,ammins,all=TRUE)
amstat<-merge(startam,amstat,all=TRUE)
amstat<-merge(endam,amstat,all=TRUE)
amstat<-merge(ammobs,amstat,all=TRUE)
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
                           amstat$Sampling.Frequency=="Twice per Month"~round(2*amstat$n,0),
                           amstat$Sampling.Frequency=="Once per 2 Weeks"~round(2*amstat$n,0)
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
ammrp<-subset(amstat,Parameter.Desc %in% c("pH","Nitrogen, ammonia total [as N]","pH, maximum",
                                           'Temperature, water deg. centigrade', 'Alkalinity, total [as CaCO3]')
              & !(Unit %in% 'lb/d')
              & !(Location.Description %in% "Raw Sewage Influent"),
                  select=c("Location.Description","Outfall","NPDES.ID","Parameter.Desc","Statistic.Description",
                           "season","Sampling.Frequency","n","est.samp",
                           "Maximum","Minimum","avg","Ninety_Perc","Ten_Perc","Unit","CV","Monitoring.Period.Start.Date","Monitoring.Period.End.Date"))

#sort
ammrp<-ammrp[order(ammrp$Parameter.Desc,ammrp$season,ammrp$Statistic.Description),]

#create another table for ammonia, this one not seasonal
ammtot<-subset(sumdat,Parameter.Desc %in% c("pH","Nitrogen, ammonia total [as N]","pH, maximum",
                                            'Temperature, water deg. centigrade', 'Alkalinity, total [as CaCO3]')
               & !(Unit %in% 'lb/d')
               & !(Location.Description %in% "Raw Sewage Influent"),
               select=c("Location.Description","Outfall","NPDES.ID","Parameter.Desc","Statistic.Description",
                               "Sampling.Frequency","n","est.samp",
                               "Maximum","avg","Ninety_Perc","Ten_Perc","Unit","CV","Monitoring.Period.Start.Date","Monitoring.Period.End.Date"))



##convert dates to make them easier to work with (at request of Erich Brandstetter) for each sheet
rpabasics$Monitoring.Period.Start.Date<-format(rpabasics$Monitoring.Period.Start.Date,format = "%m/%d/%Y")
rpabasics$Monitoring.Period.End.Date<-format(rpabasics$Monitoring.Period.End.Date,format= '%m/%d/%Y')

ammrp$Monitoring.Period.Start.Date<-format(ammrp$Monitoring.Period.Start.Date,format = "%m/%d/%Y")
ammrp$Monitoring.Period.End.Date<-format(ammrp$Monitoring.Period.End.Date,format= '%m/%d/%Y')

ammtot$Monitoring.Period.Start.Date<-format(ammtot$Monitoring.Period.Start.Date,format = "%m/%d/%Y")
ammtot$Monitoring.Period.End.Date<-format(ammtot$Monitoring.Period.End.Date,format= '%m/%d/%Y')

sumdat$Monitoring.Period.Start.Date<-format(sumdat$Monitoring.Period.Start.Date,format = "%m/%d/%Y")
sumdat$Monitoring.Period.End.Date<-format(sumdat$Monitoring.Period.End.Date,format= '%m/%d/%Y')

data1$Monitoring.Period.Start.Date<-format(data1$Monitoring.Period.Start.Date,format = "%m/%d/%Y")
data1$Monitoring.Period.End.Date<-format(data1$Monitoring.Period.End.Date,format= '%m/%d/%Y')


#############Temperature and Flow data#######################

#get temperature and flow data to help Erich with TORCH analysis
tf<-subset(data,Parameter.Desc %in% c('Flow, in conduit or thru treatment plant','Temperature, water deg. centigrade'))

#shorten parameters to just temperature and flow
tf$Parameter.Desc<-ifelse(tf$Parameter.Desc=='Flow, in conduit or thru treatment plant',"Flow",
                          (ifelse(tf$Parameter.Desc=='Temperature, water deg. centigrade',"Temperature",tf$Parameter.Desc)))

#desirable to have parameter, statistic description, and units all in one cell
tf$header<-paste(tf$Location.Description,tf$Parameter.Desc,tf$Statistic.Description,tf$Unit,sep="-")

#just get variables we need then convert from long to wide
tfsub<-subset(tf,select=c('Monitoring.Period.Start.Date',"Result","header"))

tfspr<-spread(tfsub,header,Result)

#reformat date to make it easier to read
tfspr$Monitoring.Period.Start.Date<-format(tfspr$Monitoring.Period.Start.Date,format = "%m/%d/%Y")
           
#plots for flow and temperature
tf$Monitoring.Period.Start.Date<-as.Date(tf$Monitoring.Period.Start.Date)
tfplot<-ggplot(data=tf, aes(x=Monitoring.Period.Start.Date,y=Result,color=Parameter.Desc))+
  geom_point()+
  geom_line()+
  facet_wrap(~Location.Description+Parameter.Desc+Statistic.Description+Unit,scales="free")+
  scale_color_manual(values=c("#003366","#990000"))+
  theme_bw()+
  scale_x_date(date_labels="%b-%Y")


##########################################EXCEL EXPORT###########################
#create workbook to be exported
wb<-createWorkbook()

#create styles

#sheet of Chlorine and pH RPA-relevant info
addWorksheet(wb,"pH and Chlorine RPA")
writeData(wb,sheet="pH and Chlorine RPA",startRow=1, x="Summary statistics for pH and Chlorine RPA analysis")
writeData(wb,sheet="pH and Chlorine RPA",startRow=2, x="CV only calculated for data with a monthly or less monitoring frequency, otherwise a CV of 0.6 is assumed")
writeData(wb,sheet="pH and Chlorine RPA",startRow=3, x="average, 10th percentile, and 90th percentile are calculated using n, not est.samp")
writeData(wb,sheet="pH and Chlorine RPA",startRow=4, x="Dates are in Month/Day/Year format")
writeData(wb,sheet="pH and Chlorine RPA",x=rpabasics,startCol=1, startRow=6)

#sheet of Ammonia RPA relevant info
addWorksheet(wb,"Ammonia RPA")
writeData(wb,sheet="Ammonia RPA",startRow=1,x="Summary statistics for Ammonia RPA")
writeData(wb,sheet="Ammonia RPA",startRow=2,x="winter=November - April, summer= May - October")
writeData(wb,sheet="Ammonia RPA",startRow=3, x="average, 10th percentile, and 90th percentile are calculated using n, not est.samp")
writeData(wb,sheet="Ammonia RPA",startRow=4, x="Dates are in Month/Day/Year format")
writeData(wb,sheet="Ammonia RPA",x="Seasonal",startCol=1, startRow=6)
writeData(wb,sheet="Ammonia RPA",x="Year Round",startCol=21,startRow=6)
writeData(wb,sheet="Ammonia RPA",x=ammrp,startCol=1, startRow=7)
writeData(wb,sheet="Ammonia RPA",x=ammtot,startCol=21,startRow=7)

#sheet of all summary 
addWorksheet(wb,"Parameter Summary")
writeData(wb,sheet="Parameter Summary",startRow=1,x="Summary of all reported DMR parameters")
writeData(wb,sheet="Parameter Summary",startRow=2, x="average, 10th percentile, and 90th percentile are calculated using n, not est.samp")
writeData(wb,sheet="Parameter Summary",startRow=4, x="Dates are in Month/Day/Year format")
writeData(wb,sheet="Parameter Summary",startRow=5,x=sumdat)

#sheet of original data
addWorksheet(wb,"ICIS Data")
writeData(wb,sheet="ICIS Data",startRow=1,x="Data from ICIS")
writeData(wb,sheet="ICIS Data",startRow=2,x="Note that any data where the result was not reported has been removed")
writeData(wb,sheet="ICIS Data",startRow=3,x="This data has not had any unit transformations done")
writeData(wb,sheet="ICIS Data",startRow=4, x="Dates are in Month/Day/Year format")
writeData(wb,sheet="ICIS Data",startRow=6,x=data1)

#sheet for temperature and flow
addWorksheet(wb,"Flow+Temp")
writeData(wb,sheet="Flow+Temp",startRow=1,x="Flow and Temperature data from ICIS")
writeData(wb,sheet="Flow+Temp",startRow=2,x="Reported flow and temperature for each month based on start date")
writeData(wb,sheet="Flow+Temp",startRow=2,x="Scales on plots may vary from plot to plot")
writeData(wb,sheet="Flow+Temp",startRow=5,x=tfspr)
print(tfplot)
insertPlot(wb,sheet="Flow+Temp",width=15,height=15,startCol=15,fileType='png',units="in",dpi=300)

saveWorkbook(wb,paste(path,"X-DATA-ICISRPAReady-",format(Sys.Date(),"%Y%m%d"),".xlsx",sep=""),overwrite=TRUE)


