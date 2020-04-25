#Determine the sufficiency of data found in ICIS for the 2021 permittee issuance plan

library(readxl)
library(tidyverse)
library(stringr)
library(openxlsx)


#import data
icis<-read_excel("C:/COVID-19 WORK/Gap_Analysis_Work/ICIS_SummaryStats_2010-2020.xlsx",skip=4)

#convert names so that they are usable
#note that DMR.Value is actually a count of the number of observations
names(icis)<-str_replace_all(names(icis), c(" " = "." , "," = "" ))

#remove data where DMR Value count is 0
icis<-subset(icis,DMR.Value>0)

#temperature unit doesn't matter, change name to temperature
icis$Parameter.Desc<-case_when(icis$Parameter.Desc=="Temperature, water deg. centigrade"~"Temperature",
                               icis$Parameter.Desc=="Temperature, water deg. fahrenheit"~"Temperature",
                               !(icis$Parameter.Desc=="Temperature")~icis$Parameter.Desc)

#only need information for permittees that are on 2021 list

permlist<-c('OR0041653', 'OR0041599', 'OR0041602', 'OR0041611', 'OR0041629', 'OR0041637', 'OR0041645', 'OR0041688', 
            'OR0034215', 'OR0034223', 'OR0029777', 'OR0031224', 'OR0028118', 'OR0020168', 'OR0023345', 'OR0023361', 
            'OR0020834', 'OR0020761', 'OR0020559', 'OR0020427', 'OR0020516', 'OR0020320', 'OR0026972', 'OR0020435', 
            'OR0032468', 'OR0030546', 'OR0023167', 'OR0027791', 'OR0030864', 'OR0026280', 'OR0030945', 'OR0020770', 
            'OR0034037', 'OR0030210', 'OR0022608', 'OR0026573', 'OR0022560', 'OR0020443', 'OR0033049', 'OR0022381', 
            'OR0022454', 'OR0023388', 'OR0043991', 'OR0022721', 'OR0041505', 'OR0031470', 'OR0000515', 'OR0000345', 
            'OR0000337', 'OR0000329', 'OR0000451', 'OR0030589', 'OR0044695', 'OR0001589', 'OR0044661', 'OR0044679', 
            'OR0000680', 'OR0001601', 'OR0034371', 'OR0027847', 'OR0000698', 'OR0034517')

icis<-subset(icis, icis$NPDES.ID %in% permlist)

#remove influent and percent removal data
icis<-subset(icis, !(icis$Location.Description %in% c('Raw Sewage Influent','Percent Removal','Intake','During Manufacturing')))

#number of parameters we really don't need, remove
icis<-subset(icis, !(icis$Parameter.Desc %in% c("BOD, 5-day, 20 deg. C","Dilution factor",
                                                "Solids, total suspended", "E. coli","Depth of pond or reservoir in feet", "BOD, carbonaceous [5 day, 20 C]",
                                                "Color [PT-CO units]", "Excess thermal load", "Turbidity",
                                                "Floating solids, waste or visible foam-visual", "Oil and grease",                               
                                                "BOD, 5-day, percent removal","BOD, carb-5 day, 20 deg C, percent removal","Solids, suspended percent removal",            
                                                "pH range excursions, > 60 minutes", "pH range excursions, monthly total accum",     
                                                "Solids, total dissolved", "Oil & Grease","Duration",                    
                                                "Report due [yrmoda]", "Total production - mass", "Coliform, fecal general","Biomass, fish",                                
                                                "Solids, settleable", "Enterococci" , "Coliform, fecal - % samples exceeding limit",     
                                                "Streamflow", "Number of Events","Volume, total","Chlorine usage", 
                                                "Flow, in conduit or thru treatment plant", "Flow, total", "Flow rate","Oil and grease visual"
)))


# get estimated count per parameter
#calculate estimated # of samples taken (if they were consistent)
#since not every month has the exact same number of days, use the average number of days in a month (30.42). 
#This won't account for leap years, but if they are sampling daily then they will have so many samples this shouldn't be much of an issue
#assume continuous data is once per day for now (don't know frequency)-vast underestimate, but it's something
#20.8 is average workdays in a month for '5 per week'
icis$est.samp<-case_when(icis$Sampling.Frequency=="Twice per Week"~8*icis$DMR.Value,
                           icis$Sampling.Frequency %in% c("Daily",'Continuous') ~round(30.42*icis$DMR.Value,0),
                           icis$Sampling.Frequency=="Weekly" ~4*icis$DMR.Value,
                           icis$Sampling.Frequency %in% c("Twice per Month", 'Once per 2 Weeks')~2*icis$DMR.Value,
                           icis$Sampling.Frequency=="Monthly"~ as.numeric(icis$DMR.Value),
                           icis$Sampling.Frequency %in% c("Quarterly",'When Discharging','Annual',
                                                          'Once per Event','Once per Occurance','Weekly When Discharging')~as.numeric(icis$DMR.Value),
                           icis$Sampling.Frequency=="Twice per Year"~as.numeric(icis$DMR.Value),
                           icis$Sampling.Frequency=="Three per Week"~12*icis$DMR.Value,
                           is.na(icis$Sampling.Frequency)~icis$DMR.Value,
                           icis$Sampling.Frequency=='Five per Week'~round(20.8*icis$DMR.Value,0)
                         )

#get estimated count by parameter
sumicis<-aggregate(est.samp~NPDES.ID+Location.Description+Parameter.Desc+Statistic.Description,icis,sum)


#go through by RPA type to see if data is sufficient

#to do that we need to bring in permittee information
permittee<-read_excel("C:/COVID-19 WORK/Gap_Analysis_Work/5yrNPDESIssuancePlan.xlsx",sheet="PermitteeInfo_R",
                      col_types = c("text","text","text","text","text","text","date","text","text","text",
                                    "text","text","text","text","text","text","text","text","text"))

#convert names so that they are usable
names(permittee)<-str_replace_all(names(permittee), c(" " = "." , "," = "" ))


#get just a list of permittees to merge sufficiency info into
suffice<-subset(permittee,select=c("EPA.Number","Common.Name","Permit.Type","Expiration.Date","Type","Major/Minor","Flow.Criteria"))

#determine presence of any data in ICIS
suffice$ICIS_Data<-ifelse(suffice$EPA.Number %in% sumicis$NPDES.ID,"Yes","No Data in ICIS")

#start with chlorine RPA 
#chlorine RPA needed when if permittee uses chlorine to disinfect

chlor<-subset(sumicis,sumicis$Parameter.Desc %in% c("Chlorine, total residual","Chlorine, free available",
                                                    "Ultraviolet light dosage","Ultraviolet light intensity",
                                                    "Ultraviolet light transmittance") 
              & sumicis$Statistic.Description=="Daily Maximum")

#do we meet minimum sample needs?
chlor$enough<-ifelse(chlor$Parameter.Desc %in% c("Ultraviolet light dosage","Ultraviolet light intensity",
                                                 "Ultraviolet light transmittance"),"UV System, No RPA needed", 
                     ifelse(chlor$est.samp>=52,"Enough Data","Data present, but may be insufficient"))


#populate sufficiency table with results
suffice$ChlorRPA<-chlor$enough[match(suffice$EPA.Number,chlor$NPDES.ID)]

#fill in NAs as appropriate

suffice$ChlorRPA<-ifelse(is.na(suffice$ChlorRPA) & suffice$Type=="Industrial",
                         "Industrial, Data not present and Chlorine RPA likely unnecessary", 
                         suffice$ChlorRPA)

suffice$ChlorRPA<-ifelse(is.na(suffice$ChlorRPA) & suffice$Type=="Irrigation",
                         "Irrigation, Data not present and Chlorine RPA likely unnecessary", 
                         suffice$ChlorRPA)

suffice$ChlorRPA<-ifelse(is.na(suffice$ChlorRPA) & suffice$Type=="Domestic",
                         "Domestic, No Chlorine Data and Chlorine RPA likely needed", 
                         suffice$ChlorRPA)
##brava!!!#####

##now on to pH and ammonia RPA....
#more difficult because we need pH, Temperature, ammonia, and alkalinity

amm<-subset(sumicis,sumicis$Parameter.Desc %in% c("Temperature","Nitrogen, ammonia dissolved","pH","pH, maximum",
                                                  "Nitrogen, ammonia total [as N]","Alkalinity, total [as CaCO3]",
                                                  "pH, minimum") & 
              sumicis$Location.Description %in% c("Effluent Gross","Internal Monitoring Point"))

#clean up a bit- 
amm<-subset(amm,!(amm$Statistic.Description %in% c("7 Day Median","Daily Minimum","Minimum")))

amm<-subset(amm, !(amm$Parameter.Desc=="Nitrogen, ammonia total [as N]" & amm$Statistic.Description=="Monthly Average"))

amm<-subset(amm, !(amm$Parameter.Desc=="pH" & amm$Statistic.Description=="Weekly Average"))

  
#meet minimum sample needs?
amm$pHenough<-ifelse(amm$Parameter.Desc=="pH",ifelse(amm$est.samp>=12,"pH Sufficient"," pH Insufficient"),NA)
amm$tempenough<-ifelse(amm$Parameter.Desc=="Temperature",ifelse(amm$est.samp>=12,"Temperature Sufficient","Temperature Insufficient"),NA)
amm$alkenough<-ifelse(amm$Parameter.Desc=="Alkalinity, total [as CaCO3]",ifelse(amm$est.samp>=4,"Alkalinity Sufficient","Alkalinity Insufficient"),NA)
amm$ammenough<-ifelse(amm$Parameter.Desc=="Nitrogen, ammonia total [as N]",ifelse(amm$est.samp>=4,"Ammonia Sufficient","Ammonia Insufficient"),NA)


#have whether each parameter is enough, now need to determine if all are there and sufficient
#make a column for each in suffice
#since match takes the first matching value, need to make sure the dataset 
#is in proper order for each parameter so that NAs are last

amm<-amm[order(amm$pHenough),]
suffice$pH<-amm$pHenough[match(suffice$EPA.Number,amm$NPDES.ID)]

amm<-amm[order(amm$tempenough),]
suffice$temperature<-amm$tempenough[match(suffice$EPA.Number,amm$NPDES.ID)]

amm<-amm[order(amm$alkenough),]
suffice$alkalinity<-amm$alkenough[match(suffice$EPA.Number,amm$NPDES.ID)]

amm<-amm[order(amm$ammenough),]
suffice$ammonia<-amm$ammenough[match(suffice$EPA.Number,amm$NPDES.ID)]

#let's fill in the gaps
suffice$AmmRPA<-case_when(suffice$Flow.Criteria=="<0.1 mgd flow"~"Flow <0.1 MGD, no Ammonia RPA needed",
                          suffice$Type=="Irrigation"~"Irrigation, Ammonia RPA not necessary",
                         is.na(suffice$pH) & is.na(suffice$temperature) & is.na(suffice$alkalinity) & is.na(suffice$ammonia)~"No Data, Ammonia RPA likely needed",
                         is.na(suffice$pH)|is.na(suffice$temperature)|is.na(suffice$alkalinity)|is.na(suffice$ammonia)~"Some parameters missing, Ammonia RPA likely needed",
                         (!is.na(suffice$pH) & !is.na(suffice$temperature) & !is.na(suffice$alkalinity) & !is.na(suffice$ammonia)) &
                           (str_detect(suffice$pH,"Ins")|str_detect(suffice$temperature,"Ins")|str_detect(suffice$alkalinity,"Ins")|str_detect(suffice$ammonia,"Ins"))~"All parameters present, but some insufficient",
                         (!is.na(suffice$pH) & !is.na(suffice$temperature) & !is.na(suffice$alkalinity) & !is.na(suffice$ammonia)) &
                           !(str_detect(suffice$pH,"Ins")|str_detect(suffice$temperature,"Ins")|str_detect(suffice$alkalinity,"Ins")|str_detect(suffice$ammonia,"Ins"))~"All parameters present, data sufficient"
                         )

suffice$pHRPA<-case_when(suffice$Type=="Irrigation"~"Irrigation, pH RPA not necessary",
                         is.na(suffice$alkalinity)& is.na(suffice$temperature)~"No Data, pH RPA likely needed",
                         is.na(suffice$alkalinity)|is.na(suffice$temperature)~"Some parameters missing, pH RPA likely needed",
                         (!is.na(suffice$alkalinity)& !is.na(suffice$temperature)) & (str_detect(suffice$temperature,"Ins")|str_detect(suffice$alkalinity,"Ins"))~"All parameters present, but some insufficient",
                         (!is.na(suffice$alkalinity)& !is.na(suffice$temperature)) & !(str_detect(suffice$temperature,"Ins")|str_detect(suffice$alkalinity,"Ins"))~"All parameters present, data sufficient"
                         )

#let's get a table of the random one-off parameters that might be useful for a toxics pull
grabbag<-subset(sumicis,(Parameter.Desc %in% c("Copper, total [as Cu]", "Lead, total [as Pb]", "Zinc, total [as Zn]",                   
                                               "Nitrite + Nitrate total [as N]", "Phosphate, ortho, dissolved [as P]",    
                                               "Phosphate, total [as PO4]", "Phosphorus, total [as P]",           
                                               "Mercury, dissolved [as Hg]","Mercury, total [as Hg]","Methylmercury",                        
                                               "Sulfur dioxide, total", "Formaldehyde", "Chromium, total [as Cr]", "Fluoride, total [as F]",               
                                               "Total toxic organics [TTO] [40 CFR469]", "BTEX",                                  
                                               "Bromine, reported as the element","2-Chlorophenol","4,4'-DDD","4,4'-DDE","4,4'-DDT",                              
                                               "Cadmium, total recoverable","Chloride [as Cl]","Chlorobenzene","Chromium, hexavalent [as Cr]",         
                                               "Iron, total recoverable", "Perchlorate [ClO4]","Tetrachloroethylene")))

#get list of permittees to merge potential toxics into
pottox<-subset(permittee,select=c("EPA.Number","Common.Name","Permit.Type","Expiration.Date","Type","Major/Minor","Flow.Criteria"))

pottox<-merge(pottox,grabbag,by.y="NPDES.ID",by.x="EPA.Number",all=FALSE)


#get table of potential ambient data
recwat<-subset(sumicis,!(Location.Description %in% c("Effluent Gross","Internal Monitoring Point")))

#merge into list of permittees
permamb<-subset(permittee,select=c("EPA.Number","Common.Name","Permit.Type","Expiration.Date","Type","Major/Minor","Flow.Criteria"))

permamb<-merge(permamb,recwat,by.y="NPDES.ID",by.x="EPA.Number",all=FALSE)

##########################################lets get an excel exported
wb<-createWorkbook()

#create some styles
header<-createStyle(fontSize = 12,textDecoration = "bold")
wrap<-createStyle(wrapText=TRUE)


#order sufficiency data by permit type, then by expiration date
suffice<-suffice[order(suffice$Permit.Type,suffice$Expiration.Date),]
suffice<-subset(suffice, select=c("EPA.Number", "Common.Name", "Permit.Type" ,"Expiration.Date","Type" , "Major/Minor",    
                                  "Flow.Criteria", "ICIS_Data","ChlorRPA","AmmRPA","pHRPA","pH","temperature","alkalinity",     
                                  "ammonia"))
names(suffice)<-c("EPA Number","Common Name","Permit Type","Expiration Date","Type","Major/Minor","Design Flow",
                  "Data Present in ICIS","Chlorine RPA Data Status","Ammonia RPA Data Status","pH RPA Data status",
                  "pH Data","Temperature Data","Alkalinity Data","Ammonia Data")

#want sheets for sufficiency data, potential toxics, and potential ambient data
addWorksheet(wb,"Gap_Analysis")
writeData(wb,"Gap_Analysis",startCol=1,startRow=1,x="RPA information in ICIS")
writeData(wb,"Gap_Analysis",startCol=1,startRow=2,x="Note that code likely underestimated the amount of data when the parameter was present due the lack of sampling frequency reported for some analytes")
writeData(wb,"Gap_Analysis",startCol=1,startRow=3,x="Therefore, any claim of 'insuffient data' will need to be verified with an actual data pull")
writeData(wb,"Gap_Analysis",startCol=1,startRow=4,x="Ammonia RPA Status examined sufficiency of temperature, ammonia, pH, and alkalinity data")
writeData(wb,"Gap_Analysis",startCol=1,startRow=5,x="pH RPA Status examined sufficiency of temperature and alkalinity data")
writeData(wb,"Gap_Analysis",startCol=1,startRow=6,x="Irrigation permits added just to see if any data could be found in ICIS, not part of 2021 permit issuance plan")
writeData(wb,"Gap_Analysis",startCol=1,startRow=8,x=suffice,keepNA=TRUE,na.string="No Data")
addStyle(wb,"Gap_Analysis",rows=1:8, cols = 1:20, style = header, gridExpand= T)
addStyle(wb,"Gap_Analysis",rows=9:100,cols = 1:20,style= wrap, gridExpand = T)
setColWidths(wb,"Gap_Analysis",widths=15, cols=1:20)

pottox<-pottox[order(pottox$Permit.Type),]
names(pottox)<-c("EPA Number","Common Name","Permit Type","Expiration Date","Type","Major/Minor","Design Flow","Location Description",
                 "Parameter","Statistic","Estimated Number of Samples")

addWorksheet(wb,"Toxics_Data")
writeData(wb,"Toxics_Data",startCol=1,startRow=1,x="Data that could be used for Toxics RPA")
writeData(wb,"Toxics_Data",startCol=1,startRow=4,x=pottox)
addStyle(wb,"Toxics_Data",rows=1:4, cols = 1:20, style = header, gridExpand= T)
addStyle(wb,"Toxics_Data",rows=5:100,cols = 1:20, gridExpand=T,style= wrap)
setColWidths(wb,"Toxics_Data",widths=15, cols=1:20)


permamb<-permamb[order(permamb$Permit.Type),]
names(permamb)<-c("EPA Number","Common Name","Permit Type","Expiration Date","Type","Major/Minor","Design Flow","Location Description",
                  "Parameter","Statistic","Estimated Number of Samples")

addWorksheet(wb,"Potential_Ambient")
writeData(wb,"Potential_Ambient",startCol=1,startRow=1,x="Potential ambient data reported in ICIS, note that the locations and usability will need to be further verified before use")
writeData(wb,"Potential_Ambient",startCol=1,startRow = 4, x=permamb)
addStyle(wb,"Potential_Ambient",rows=1:4, cols = 1:20, style = header, gridExpand=T)
addStyle(wb,"Potential_Ambient",rows=5:100,cols = 1:20,gridExpand=T, style= wrap)
setColWidths(wb,"Potential_Ambient",widths=15, cols=1:20)

saveWorkbook(wb,"C:/COVID-19 WORK/Gap_Analysis_Work/Gap_Analysis_Results.xlsx",overwrite=TRUE)


