
#############################################################################
# SOCIAL CONTACT DATA ANALYSIS
#
# Copyright 2019, SIMID
#############################################################################
#
# MISSING DATA EXPLORATION
#
#############################################################################

rm(list=ls(all=TRUE))
setwd("C:\\Users\\lucp9032\\Desktop\\TIME USE FOLDER\\CODE_MI_BOOSTRAP\\THANG")

devtools::install_github("lwillem/simid_rtools",force=F,quiet=T)
#devtools::uninstall(simid.rtools)
library('simid.rtools')

packages_list <- c("car",
                    'mice',  
                   'VIM',   
                   'devtools')   

# load packages (and install if not present)
smd_load_packages(packages_list)  

# input/output folder
data_folder    <- smd_file_path('data','survey_flanders2010')

# load data
part <- read.table(file.path(data_folder,"individualsurvey_participants_full.txt"), sep=",",header = TRUE)
# Exclude people living in an elderly/nursing home + 6 people aged 90+: to be consistent with the sample size used in 
# the Flemish social contact paper
part$residential_status<-as.character(part$residential_status)
part$residential_status[is.na(part$residential_status)]<-"missing"
part<-subset(part,residential_status!="elderly_home" & residential_status!="nursing_home")
part<-subset(part,participant_age<90)
dim(part) #1707


###Create TUD###
names(part)
var<-c("local_id","participant_age","participant_gender","dayofweek","holiday", paste0("time_use_location_",1:17),"hh_size")
TUD<-part[var]

time_use_location<-paste0("time_use_location_",1:17)
TUD[,time_use_location] <- lapply(TUD[,time_use_location], function(x) 
  recode(x,"9=NA"))


names(TUD)[names(TUD)=="time_use_location_1"]<-"05-08h"
names(TUD)[names(TUD)=="time_use_location_2"]<-"08-09h"
names(TUD)[names(TUD)=="time_use_location_3"]<-"09-10h"
names(TUD)[names(TUD)=="time_use_location_4"]<-"10-11h"
names(TUD)[names(TUD)=="time_use_location_5"]<-"11-12h"
names(TUD)[names(TUD)=="time_use_location_6"]<-"12-13h"
names(TUD)[names(TUD)=="time_use_location_7"]<-"13-14h"
names(TUD)[names(TUD)=="time_use_location_8"]<-"14-15h"
names(TUD)[names(TUD)=="time_use_location_9"]<-"15-16h"
names(TUD)[names(TUD)=="time_use_location_10"]<-"16-17h"
names(TUD)[names(TUD)=="time_use_location_11"]<-"17-18h"
names(TUD)[names(TUD)=="time_use_location_12"]<-"18-19h"
names(TUD)[names(TUD)=="time_use_location_13"]<-"19-20h"
names(TUD)[names(TUD)=="time_use_location_14"]<-"20-22h"
names(TUD)[names(TUD)=="time_use_location_15"]<-"22-24h"
names(TUD)[names(TUD)=="time_use_location_16"]<-"24-02h"
names(TUD)[names(TUD)=="time_use_location_17"]<-"02-05h"

TUD$week<-NULL
TUD$week[TUD$dayofweek==0|TUD$dayofweek==6]<- "Weekend"
TUD$week[TUD$dayofweek==1|TUD$dayofweek==2|TUD$dayofweek==3|TUD$dayofweek==4|TUD$dayofweek==5]<- "Weekday"


TUD$version<-NULL
TUD$version[TUD$participant_age<12]<-"Version 1"
TUD$version[TUD$participant_age>=12&TUD$participant_age<=60]<-"Version 2"
TUD$version[TUD$participant_age>60]<-"Version 3"


TUD1<-TUD[,6:22]

####Table showing missing pattern#####
pattern<-md.pattern(TUD1)

pattern<-data.frame(md.pattern(TUD1))
pattern$number<-row.names(md.pattern(TUD1))
View(pattern)

pattern1<-pattern[,c(1,2,5,6,3,4,7,9,8,10,11,13,12,14,15,16,17)]
missing<-as.matrix(pattern1[,1:17]*100/1759)

colnames(missing)<-NULL
rownames(missing)<-NULL
barplot(t(missing),ylim=c(0,8),col = "yellow",ylab="% nonfilled cases",xlab="",
        names.arg=c("05-08h","08-09h","09-10h","10-11h","11-12h","12-13h","13-14h","14-15h","15-16h",
                    "16-17h","17-18h","18-19h","19-20h","20-22h","22-24h","24-02h","02-05h"),las=2)

plot <- aggr(TUD1, col=c('navyblue','yellow'),
             numbers=TRUE, sortVars=TRUE,
             labels=names(TUD1), cex.axis=.7,
             gap=3, ylab=c("Missing data","Pattern"),digits=1)


######Missing frequency in time use data####
missing<-TUD
var<-c("05-08h","08-09h","09-10h","10-11h","11-12h","12-13h","13-14h","14-15h","15-16h",
       "16-17h","17-18h","18-19h","19-20h","20-22h","22-24h","24-02h","02-05h")

for(ex in var){
  missing[,ex][!is.na(missing[,ex])]<-0
  missing[,ex][is.na(missing[,ex])]<-1
}

missing$total<-rowSums(missing[,var])

#####Missing frequency by versions

missing$total[missing$total>0]<-1

table(missing$total,missing$version)


#######Missing data by time slot####

c<-NULL
for(ex in var){
  c[ex]<-table(is.na(TUD[,ex]))[2]*100/1759
}


barplot(t(c),ylim=c(0,7),col = "yellow",ylab="% nonfilled cases",xlab="",las=2)







