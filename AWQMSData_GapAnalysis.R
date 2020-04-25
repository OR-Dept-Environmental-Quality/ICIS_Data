#Pulling AWQMS Ambient data into one file and combining it with permittee data

library(tidyverse)
library(readxl)
library(openxlsx)

source("E:/GitHub/ShinyNPDES_AWQMS/NPDES_AWQMSQuery.R")
source("E:/GitHub/ShinyNPDES_AWQMS/NameandFraction.R")


#get list of parameters fron NPDES Shiny app 
param<-c("Alkalinity, total","pH","Temperature, water","Salinity","Conductivity","Ammonia ","Ammonia and ammonium","Ammonia-nitrogen",
         "Calcium","Chloride","Copper","Magnesium","pH","Potassium","Sodium","Sulfate","Organic carbon",
         "Total Sulfate","Sulfide","Specific conductance","Dissolved oxygen (DO)","Dissolved oxygen saturation",
          "p,p'-DDT","Parathion","Chlordane","Lindane","Dieldrin","Endrin","Methoxychlor","p,p'-DDD","p,p'-DDE","Heptachlor",
           "Azinphos-methyl","Malathion","Aldrin",".alpha.-Hexachlorocyclohexane",".beta.-Hexachlorocyclohexane",
           "Benzene Hexachloride, Beta (BHC)","1,2,3,4,5,6-Hexachlorocyclohexane",".alpha.-Endosulfan","Heptachlor epoxide",
           "Endosulfan sulfate","Mirex","Chlorpyrifos","Endrin aldehyde","Toxaphene","Demeton","Aroclor 1260","Aroclor 1254",
           "Aroclor 1221","Aroclor 1232","Aroclor 1248","Aroclor 1016",".beta.-Endosulfan","Aroclor 1242","Total PCBs",
         "Benzo[a]pyrene","Dibenz[a,h]anthracene","Benz[a]anthracene","N-Nitrosodimethylamine","Hexachloroethane",
         "Hexachlorocyclopentadiene","Isophorone","Acenaphthene","Diethyl phthalate","Dibutyl phthalate","Phenanthrene",
         "Butyl benzyl phthalate","N-Nitrosodiphenylamine","Fluorene","Hexachlorobutadiene","Naphthalene","2-Chloronaphthalene",
         "3,3'-Dichlorobenzidine","Benzidine","1,2,4,5-Tetrachlorobenzene","Nitrobenzene","BDE-003",
         "Bis(2-chloro-1-methylethyl) ether","Bis(2-chloroethyl) ether","Bis(2-chloroethoxy)methane","Di(2-ethylhexyl) phthalate",
         "Di-n-octyl phthalate","Hexachlorobenzene","Anthracene","1,2,4-Trichlorobenzene","2,4-Dinitrotoluene","1,2-Diphenylhydrazine",
         "Pyrene","Dimethyl phthalate","Benzo[ghi]perylene","Indeno[1,2,3-cd]pyrene","Benzo(b)fluoranthene","Fluoranthene",
         "Benzo[k]fluoranthene","Acenaphthylene","Chrysene","2,6-Dinitrotoluene","Pentachlorobenzene","N-Nitrosodi-n-propylamine",
         "p-Chlorophenyl phenyl ether","Azobenzene","2,4-Dinitrophenol","p-Chloro-m-cresol","Pentachlorophenol","2,4,6-Trichlorophenol",
         "o-Nitrophenol","o-Chlorophenol", "2,4,5-Trichlorophenol","p-Nitrophenol","2,4-Dimethylphenol","Phenol","Phenols","2,4-Dichlorophenol",
         "4,6-Dinitro-o-cresol","Carbon tetrachloride","Chloroform","Benzene","1,1,1-Trichloroethane","Methyl bromide","Chloromethane",
         "Chloroethane","Vinyl chloride","Methylene chloride","Tribromomethane","Dichlorobromomethane","1,1-Dichloroethane","1,1-Dichloroethylene",
          "1,2-Dichloropropane","1,1,2-Trichloroethane","Trichloroethene (TCE)","Trichloroethylene","1,1,2,2-Tetrachloroethane","o-Dichlorobenzene",
          "Ethylbenzene","p-Dichlorobenzene","Acrolein","Allyl chloride","1,2-Dichloroethane","Toluene","Chlorobenzene",
          "2-Chloroethyl vinyl ether","Chlorodibromomethane","Tetrachloroethene","Tetrachloroethylene","trans-1,2-Dichloroethylene",
          "m-Dichlorobenzene","1,3-Dichloropropene","Acrylonitrile","Cyanide","Cyanides amenable to chlorination (HCN & CN)",
         "Aluminum","Iron","Lead","Mercury","Nickel","Silver","Thallium","Antimony","Arsenic","Arsenic, Inorganic",
             "Beryllium","Cadmium","Chromium","Zinc","Selenium","Nitrate","Inorganic nitrogen (nitrate and nitrite)",
             "Nitrate + Nitrite","Chromium(III)","Chromium(VI)","Arsenic ion (3+)","Total hardness","Hardness, Ca, Mg",
            "Hardness, carbonate","Hardness, non-carbonate", "Orthophosphate","Phosphate-phosphorus","Manganese","Flow","Total dissolved solids",
          "Nitrite","Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)","Organic Nitrogen")

#list of AUs needed from 2021 list
aus<-c('OR_EB_1710020607_01_107223', 'OR_EB_1710030403_01_100285', 'OR_EB_1710030403_01_107198', 'OR_LK_1709000203_02_100706', 
       'OR_OC_01_1710030605_107239', 'OR_OC_1710031206_01_107241', 'OR_SR_1707010305_02_101480', 'OR_SR_1707010307_02_102616', 
       'OR_SR_1707010313_02_101492', 'OR_SR_1707010401_02_102605', 'OR_SR_1707010402_02_101494', 'OR_SR_1707020109_05_101547', 
       'OR_SR_1707020110_05_101552', 'OR_SR_1707030603_05_102625', 'OR_SR_1707030611_05_101828', 'OR_SR_1708000302_88_100669', 
       'OR_SR_1708000302_88_100670', 'OR_SR_1709000110_02_104584', 'OR_SR_1709000203_02_104585', 'OR_SR_1709000204_02_103787', 
       'OR_SR_1709000306_05_103854', 'OR_SR_1709000405_02_103866', 'OR_SR_1709000407_02_103884', 'OR_SR_1709000503_02_103906', 
       'OR_SR_1709000506_02_103930', 'OR_SR_1709000701_05_104005', 'OR_SR_1709000703_04_104013', 'OR_SR_1709000703_05_104014', 
       'OR_SR_1709000703_88_104015', 'OR_SR_1709000704_02_104018', 'OR_SR_1709000704_88_104019', 'OR_SR_1709000704_88_104020', 
       'OR_SR_1709000902_02_104073', 'OR_SR_1709000905_02_104088', 'OR_SR_1709001002_02_104104', 'OR_SR_1709001002_02_104105', 
       'OR_SR_1709001004_02_104139', 'OR_SR_1709001104_02_104154', 'OR_SR_1709001104_02_104155', 'OR_SR_1709001202_88_104175', 
       'OR_SR_1710020201_05_106441', 'OR_SR_1710020202_05_106442', 'OR_SR_1710020608_02_105080', 'OR_SR_1710030302_05_105126', 
       'OR_SR_1710030303_02_106421', 'OR_SR_1710030304_05_105153', 'OR_SR_1710030502_02_104970', 'OR_SR_1710030502_02_104973', 
       'OR_SR_1710030802_04_105816', 'OR_SR_1710030804_04_106341', 'OR_SR_1710030904_02_105618', 'OR_SR_1710030906_02_106343', 
       'OR_SR_1801020104_05_107179', 'OR_SR_1801020106_05_106967', 'OR_WS_170701050506_02_102001', 'OR_WS_170701050701_02_102005', 
       'OR_WS_170701050702_02_102006', 'OR_WS_170703010705_05_102300', 'OR_WS_170800010202_02_103638', 'OR_WS_170900030601_02_104287', 
       'OR_WS_170900040202_02_104306', 'OR_WS_170900040204_02_104308', 'OR_WS_170900050203_02_104345', 'OR_WS_170900090303_02_104470', 
       'OR_WS_170900100401_02_104506', 'OR_WS_170900110604_02_104546', 'OR_WS_171002020101_05_106443' 
)

#perform query
#want last ten years, and want to make sure to have correct site types
data<-NPDES_AWQMS_Qry(startdate="2010-04-22",enddate = "2020-04-22", char=param, AU_ID = aus, 
                      montype = c('BEACH Program Site-Ocean','BEACH Program Site-River/Stream', 'BEACH Program Site-Estuary',
                                  'Canal Drainage','Canal Irrigation','Canal Transport','Estuary',
                                   'Lake','Ocean','Reservoir','River/Stream',
                                   'River/Stream Perennial','Facility Public Water Supply (PWS)'))

#combine name and fraction of parameters so metals are counted ok
data<-namefrac(data)

#get count of data points by parameter by AU
counts<-data %>%
  group_by(AU_ID,Char_Name) %>%
  summarise(samples= n())

#do we have data for pH and ammonia RPAs?
ammcounts<-subset(counts,counts$Char_Name %in% c("Alkalinity, total","pH","Temperature, water","Salinity",
                                              "Ammonia ","Ammonia and ammonium","Ammonia-nitrogen"))
  

#read in permittee list
permittee<-read_excel("C:/COVID-19 WORK/Gap_Analysis_Work/5yrNPDESIssuancePlan.xlsx",sheet="PermitteeInfo_R",
                      col_types = c("text","text","text","text","text","text","date","text","text","text",
                                    "text","text","text","text","text","text","text","text","text"))
#fix names
names(permittee)<-str_replace_all(names(permittee), c(" " = "." , "," = "" ))

#make shorter version to match au data, get rid of irrigation permits- don't need to look for receiving water for them
perm<-subset(permittee,permittee$Type !="Irrigation",select=c("EPA.Number","Common.Name","Permit.Type","Expiration.Date","Type","Major/Minor",
                                "Flow.Criteria","Receiving.AU","Upstream.AU"))


#meet minimum sample needs?
ammcounts$pHenough<-ifelse(ammcounts$Char_Name=="pH",ifelse(ammcounts$samples>=12,"pH Sufficient"," pH Insufficient"),NA)
ammcounts$tempenough<-ifelse(ammcounts$Char_Name=="Temperature, water",ifelse(ammcounts$samples>=12,"Temperature Sufficient","Temperature Insufficient"),NA)
ammcounts$alkenough<-ifelse(ammcounts$Char_Name=="Alkalinity, total",ifelse(ammcounts$samples>=4,"Alkalinity Sufficient","Alkalinity Insufficient"),NA)
ammcounts$ammenough<-ifelse(ammcounts$Char_Name %in% c("Ammonia ","Ammonia and ammonium","Ammonia-nitrogen"),ifelse(ammcounts$samples>=4,"Ammonia Sufficient","Ammonia Insufficient"),NA)

#populate perm table
ammcounts<-ammcounts[order(ammcounts$pHenough),]
perm$pH_Rec<-ammcounts$pHenough[match(perm$Receiving.AU,ammcounts$AU_ID)]
perm$pH_Ups<-ammcounts$pHenough[match(perm$Upstream.AU,ammcounts$AU_ID)]

ammcounts<-ammcounts[order(ammcounts$tempenough),]
perm$temperature_Rec<-ammcounts$tempenough[match(perm$Receiving.AU,ammcounts$AU_ID)]
perm$temperature_Ups<-ammcounts$tempenough[match(perm$Upstream.AU,ammcounts$AU_ID)]

ammcounts<-ammcounts[order(ammcounts$alkenough),]
perm$alkalinity_Rec<-ammcounts$alkenough[match(perm$Receiving.AU,ammcounts$AU_ID)]
perm$alkalinity_Ups<-ammcounts$alkenough[match(perm$Upstream.AU,ammcounts$AU_ID)]

ammcounts<-ammcounts[order(ammcounts$ammenough),]
perm$ammonia_Rec<-ammcounts$ammenough[match(perm$Receiving.AU,ammcounts$AU_ID)]
perm$ammonia_Ups<-ammcounts$ammenough[match(perm$Upstream.AU,ammcounts$AU_ID)]

#let's fill in the gaps
perm$AmmRPA_Rec<-case_when(perm$Receiving.AU=="NA"|is.na(perm$Receiving.AU)|perm$Receiving.AU=="Not Assessed"~"No Receiving AU Identified",
                           perm$Flow.Criteria=="<0.1 mgd flow"~"Flow <0.1 MGD, no Ammonia RPA needed",
                          perm$Type=="Irrigation"~"Irrigation, Ammonia RPA not necessary",
                          is.na(perm$pH_Rec) & is.na(perm$temperature_Rec) & is.na(perm$alkalinity_Rec) & is.na(perm$ammonia_Rec)~"No Data",
                          is.na(perm$pH_Rec)|is.na(perm$temperature_Rec)|is.na(perm$alkalinity_Rec)|is.na(perm$ammonia_Rec)~"Some parameters missing",
                          (!is.na(perm$pH_Rec) & !is.na(perm$temperature_Rec) & !is.na(perm$alkalinity_Rec) & !is.na(perm$ammonia_Rec)) &
                            (str_detect(perm$pH_Rec,"Ins")|str_detect(perm$temperature_Rec,"Ins")|str_detect(perm$alkalinity_Rec,"Ins")|str_detect(perm$ammonia_Rec,"Ins"))~"All parameters present, but some insufficient",
                          (!is.na(perm$pH_Rec) & !is.na(perm$temperature_Rec) & !is.na(perm$alkalinity_Rec) & !is.na(perm$ammonia_Rec)) &
                            !(str_detect(perm$pH_Rec,"Ins")|str_detect(perm$temperature_Rec,"Ins")|str_detect(perm$alkalinity_Rec,"Ins")|str_detect(perm$ammonia_Rec,"Ins"))~"All parameters present, data sufficient"
)

perm$AmmRPA_Ups<-case_when(perm$Upstream.AU=="NA"|is.na(perm$Upstream.AU)~"No Upstream AU Identified",
                           perm$Flow.Criteria=="<0.1 mgd flow"~"Flow <0.1 MGD, no Ammonia RPA needed",
                           perm$Type=="Irrigation"~"Irrigation, Ammonia RPA not necessary",
                           is.na(perm$pH_Ups) & is.na(perm$temperature_Ups) & is.na(perm$alkalinity_Ups) & is.na(perm$ammonia_Ups)~"No Data",
                           is.na(perm$pH_Ups)|is.na(perm$temperature_Ups)|is.na(perm$alkalinity_Ups)|is.na(perm$ammonia_Ups)~"Some parameters missing",
                           (!is.na(perm$pH_Ups) & !is.na(perm$temperature_Ups) & !is.na(perm$alkalinity_Ups) & !is.na(perm$ammonia_Ups)) &
                             (str_detect(perm$pH_Ups,"Ins")|str_detect(perm$temperature_Ups,"Ins")|str_detect(perm$alkalinity_Ups,"Ins")|str_detect(perm$ammonia_Ups,"Ins"))~"All parameters present, but some insufficient",
                           (!is.na(perm$pH_Ups) & !is.na(perm$temperature_Ups) & !is.na(perm$alkalinity_Ups) & !is.na(perm$ammonia_Ups)) &
                             !(str_detect(perm$pH_Ups,"Ins")|str_detect(perm$temperature_Ups,"Ins")|str_detect(perm$alkalinity_Ups,"Ins")|str_detect(perm$ammonia_Ups,"Ins"))~"All parameters present, data sufficient"
)

perm$pHRPA_Rec<-case_when(perm$Receiving.AU=="NA"|is.na(perm$Receiving.AU)|perm$Receiving.AU=="Not Assessed"~"No Receiving AU Identified",
                          perm$Type=="Irrigation"~"Irrigation, pH RPA not necessary",
                         is.na(perm$alkalinity_Rec)& is.na(perm$temperature_Rec)& is.na(perm$pH_Rec)~"No Data",
                         is.na(perm$alkalinity_Rec)|is.na(perm$temperature_Rec)|is.na(perm$pH_Rec)~"Some parameters missing",
                         (!is.na(perm$alkalinity_Rec)& !is.na(perm$temperature_Rec) &!is.na(perm$pH_Rec)) & 
                           (str_detect(perm$temperature_Rec,"Ins")|str_detect(perm$alkalinity_Rec,"Ins")|str_detect(perm$pH_Rec,"Ins"))~"All parameters present, but some insufficient",
                         (!is.na(perm$alkalinity_Rec)& !is.na(perm$temperature_Rec)&!is.na(perm$pH_Rec)) & 
                           !(str_detect(perm$temperature_Rec,"Ins")|str_detect(perm$alkalinity_Rec,"Ins")|str_detect(perm$pH_Rec,"Ins"))~"All parameters present, data sufficient"
)

perm$pHRPA_Ups<-case_when(perm$Upstream.AU=="NA"|is.na(perm$Upstream.AU)~"No Upstream AU Identified",
                          perm$Type=="Irrigation"~"Irrigation, pH RPA not necessary",
                          is.na(perm$alkalinity_Ups)& is.na(perm$temperature_Ups)& is.na(perm$pH_Ups)~"No Data",
                          is.na(perm$alkalinity_Ups)|is.na(perm$temperature_Ups)|is.na(perm$pH_Ups)~"Some parameters missing",
                          (!is.na(perm$alkalinity_Ups)& !is.na(perm$temperature_Ups) & !is.na(perm$pH_Ups)) & 
                            (str_detect(perm$temperature_Ups,"Ins")|str_detect(perm$alkalinity_Ups,"Ins")|str_detect(perm$pH_Ups, "Ins"))~"All parameters present, but some insufficient",
                          (!is.na(perm$alkalinity_Ups)& !is.na(perm$temperature_Ups)) & 
                            !(str_detect(perm$temperature_Ups,"Ins")|str_detect(perm$alkalinity_Ups,"Ins")|str_detect(perm$pH_Ups, "Ins"))~"All parameters present, data sufficient"
)

#########let's export results
wb<-createWorkbook()

#create some styles
header<-createStyle(fontSize = 12,textDecoration = "bold")
wrap<-createStyle(wrapText=TRUE)

#table just for ammonia RPA
ammperm<-subset(perm,select=c("EPA.Number", "Common.Name","Permit.Type","Expiration.Date","Type","Major/Minor","Flow.Criteria", 
                              "Receiving.AU","Upstream.AU","AmmRPA_Rec","AmmRPA_Ups","pH_Rec","temperature_Rec","ammonia_Rec",
                              "alkalinity_Rec","pH_Ups","temperature_Ups","ammonia_Ups","alkalinity_Ups"))

names(ammperm)<-c("EPA Number","Common Name","Permit Type","Expiration Date","Type","Major/Minor","Design Flow","Receiving Water AU",
                  "Upstream AU","Ammonia RPA Data Status-Receiving AU","Ammonia RPA Data Status- Upstream AU","pH status-Receving AU",
                  "temperature status- Receiving AU","ammonia status- Receiving AU","alkalinity status-Receiving AU","pH status-Upstream AU",
                  "temperature status- Upstream AU","ammonia status- Upstream-AU","alkalinity status-Upstream AU")

addWorksheet(wb,"Ambient_AmmoniaRPA")
writeData(wb,"Ambient_AmmoniaRPA",startCol=1,startRow=1,x="Ambient Ammonia RPA information in AWQMS")
writeData(wb,"Ambient_AmmoniaRPA",startCol=1,startRow=4,x=ammperm,keepNA=TRUE,na.string="No Data")
addStyle(wb,"Ambient_AmmoniaRPA",rows=1:4, cols = 1:20, style = header, gridExpand= T)
addStyle(wb,"Ambient_AmmoniaRPA",rows=5:100,cols = 1:20,style= wrap, gridExpand = T)
setColWidths(wb,"Ambient_AmmoniaRPA",widths=15, cols=1:20)

#table just for pH RPA
phperm<-subset(perm, select=c("EPA.Number", "Common.Name","Permit.Type","Expiration.Date","Type","Major/Minor","Flow.Criteria", 
                              "Receiving.AU","Upstream.AU", "pHRPA_Rec" ,"pHRPA_Ups","pH_Rec","temperature_Rec","alkalinity_Rec",
                              "pH_Ups", "temperature_Ups","alkalinity_Ups"))
names(phperm)<-c("EPA Number","Common Name","Permit Type","Expiration Date","Type","Major/Minor","Design Flow","Receiving Water AU",
                 "Upstream AU","pH RPA Data Status-Receiving AU","pH RPA Data status-Upstream AU","pH status-Receving AU",
                 "temperature status- Receiving AU","alkalinity status-Receiving AU","pH status-Upstream AU",
                 "temperature status- Upstream AU","alkalinity status-Upstream AU")

addWorksheet(wb,"Ambient_pHRPA")
writeData(wb,"Ambient_pHRPA",startCol=1,startRow=1,x="Ambient pH RPA information in AWQMS")
writeData(wb,"Ambient_pHRPA",startCol=1,startRow=4,x=phperm,keepNA=TRUE,na.string="No Data")
addStyle(wb,"Ambient_pHRPA",rows=1:4, cols = 1:20, style = header, gridExpand= T)
addStyle(wb,"Ambient_pHRPA",rows=5:100,cols = 1:20,style= wrap, gridExpand = T)
setColWidths(wb,"Ambient_pHRPA",widths=15, cols=1:20)

saveWorkbook(wb,"C:/COVID-19 WORK/Gap_Analysis_Work/AMBIENT_Gap_Analysis_Results.xlsx",overwrite=TRUE)
  
