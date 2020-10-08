rm(list=ls(all=TRUE))
setwd("C:\\Users\\lucp9032\\Desktop\\TIME USE FOLDER\\CODE_MI_BOOSTRAP\\THANG")

devtools::install_github("lwillem/simid_rtools",force=F,quiet=T)
#devtools::uninstall(simid.rtools)
library('simid.rtools')

packages_list <- c('car',  
                   'VIM',   
                   'devtools')   

# input/output folder
data_folder    <- smd_file_path('data','survey_flanders2010')
output_folder <- smd_file_path('output','Figures')


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
var<-c("local_id","participant_age","participant_gender","dayofweek","holiday", "time_use_location_1", "time_use_location_2"
       ,"time_use_location_3", "time_use_location_4", "time_use_location_5"
       ,"time_use_location_6", "time_use_location_7", "time_use_location_8", "time_use_location_9", "time_use_location_10"
       ,"time_use_location_11", "time_use_location_12", "time_use_location_13", "time_use_location_14", "time_use_location_15"
       ,"time_use_location_16", "time_use_location_17","hh_size")

TUD<-part[var]

###Recode multiple variables in R
#install.packages("car")
library(car)

TUD[,c(6:22)] <- lapply(TUD[,c(6:22)], function(x) 
  recode(x,"1='home';2='school';3='school';
         4='work';5='transport';6='other';7='leisure';8='other';9='nonfilled'"))

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


#####Calculate total duration spent on each location####
names(TUD)

location <- function (x,y){
  a<-rep(0,length(TUD[,1]))
  a1<-a;a2<-a;a3<-a;a4<-a;a5<-a;a6<-a;a7<-a;a8<-a;a9<-a;a10<-a;a11<-a;a12<-a;a13<-a;a14<-a;a15<-a;a16<-a;a17<-a
  for (i in 1:length(TUD[,1])) {
    if (x[i,6]==y){a1[i] <- a1[i]+ 3}
    if (x[i,7]==y){a2[i] <- a2[i]+ 1}
    if (x[i,8]==y){a3[i] <- a3[i]+ 1}
    if (x[i,9]==y){a4[i] <- a4[i]+ 1}
    if (x[i,10]==y){a5[i] <- a5[i]+ 1}
    if (x[i,11]==y){a6[i] <- a6[i]+ 1}
    if (x[i,12]==y){a7[i] <- a7[i]+ 1}
    if (x[i,13]==y){a8[i] <- a8[i]+ 1}
    if (x[i,14]==y){a9[i] <- a9[i]+ 1}
    if (x[i,15]==y){a10[i] <- a10[i]+ 1}
    if (x[i,16]==y){a11[i] <- a11[i]+ 1}
    if (x[i,17]==y){a12[i] <- a12[i]+ 1}
    if (x[i,18]==y){a13[i] <- a13[i]+ 1}
    if (x[i,19]==y){a14[i] <- a14[i]+ 2}
    if (x[i,20]==y){a15[i] <- a15[i]+ 2}
    if (x[i,21]==y){a16[i] <- a16[i]+ 2}
    if (x[i,22]==y){a17[i] <- a17[i]+ 3}
    a[i]<-a1[i] +a2[i]+a3[i]+a4[i]+a5[i]+a6[i]+a7[i]+a8[i]+a9[i]+a10[i]+a11[i]+a12[i]+a13[i]+a14[i]+a15[i]+a16[i]+a17[i]}
  return (a)}

TUD$home<-location(TUD,"home")
TUD$school<- location(TUD,"school")
TUD$work <- location(TUD,"work")
TUD$transport <- location(TUD,"transport")
TUD$leisure <- location(TUD,"leisure")
TUD$other <- location(TUD,"other")
TUD$nonfilled <- location(TUD,"nonfilled")

###merge other locations
TUD$missing<-TUD$nonfilled
TUD$nonfilled<-NULL
TUD$other<-TUD$other+TUD$leisure
TUD$leisure<-NULL

TUD$age_cat <- cut(TUD$participant_age, breaks=c(0, 3, 6, 12,18,25,30,35,40,45,50,55,60,65,70,75,80,90),right = FALSE)

TUD1<-TUD[,-c(6:22)]

########################################################
########################################################
########################################################

library(tidyr)
library(plyr)

TUD1 <- gather(TUD1, locations, value, home:missing, factor_key=TRUE)
TUD1<-TUD1[order(TUD1$local_id),]
TUD1<-aggregate(value~age_cat+locations+participant_gender,data=TUD1,sum)
TUD1<-TUD1[order(TUD1$age_cat),]

TUD2<-ddply(TUD1,.(age_cat,participant_gender),transform,percent= value/sum(value) * 100)

TUD2$participant_gender<-as.character(TUD2$participant_gender)
TUD2$participant_gender[TUD2$participant_gender=="M"]<-"Male"
TUD2$participant_gender[TUD2$participant_gender=="F"]<-"Female"

#Get the levels for type in the required order
#TUD2$locations = factor(TUD2$locations, levels = c("home",  "school", "work","transport","leisure","other","nonfilled"))
TUD2$locations = factor(TUD2$locations, levels = c("home",  "school", "work","transport","other","missing"))
TUD2 = arrange(TUD2, participant_gender,age_cat, desc(locations))

# Format the labels and calculate their positions
TUD2$label = paste0(sprintf("%.0f", TUD2$percent), "%")

# Get the levels for type in the required order
TUD2$age_cat = factor(TUD2$age_cat, levels = c(levels(TUD2$age_cat)))

TUD2$percent<-round(TUD2$percent, digits = 2)

TUD2<-TUD2[TUD2$percent>0.5,]


windows(width=18, height=10)
library(ggplot2)
ggplot(TUD2, aes(x = factor(TUD2$age_cat), y = percent, fill = locations)) +
  geom_bar(stat = "identity", width = 0.95) + theme_bw()  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_grid(.~participant_gender) +
  geom_text(aes(label = label), size = 3.2,position = position_stack(vjust = 0.5), size = 2)+
  xlab("age categories")+
  ylab("Percentage (%)")+
  theme(axis.title = element_text(size=15),axis.text.x = element_text(size=10, angle = 90, hjust = 1,vjust = 0.5),legend.position = "bottom",legend.text = element_text(size = 15))+
  theme(legend.title=element_blank())+
  theme(strip.text.x = element_text(size=15),strip.text.y = element_text(size = 13,angle=0),strip.background = element_rect(fill = "white",colour = "NA")) +
  theme(panel.border = element_blank())+
  scale_fill_manual("",values = c("#9ecae1","orchid1", "#de2d26","seagreen3","#8856a7","#fec44f"))


######################################################
######################################################
######################################################
# rm(list = ls(all=TRUE))
# setwd("C:\\Users\\lucp9032\\Desktop\\All datasets\\contact data\\SoCRates_simid-survey_flanders2010-5f1d63042d36\\data\\clean_data")
# part = read.table("individualsurvey_participants_full_clean.txt", sep=",",header = TRUE)

###Create TUD###
names(part)
var<-c("local_id","participant_age","participant_gender","dayofweek","holiday", "time_use_location_1", "time_use_location_2"
       ,"time_use_location_3", "time_use_location_4", "time_use_location_5"
       ,"time_use_location_6", "time_use_location_7", "time_use_location_8", "time_use_location_9", "time_use_location_10"
       ,"time_use_location_11", "time_use_location_12", "time_use_location_13", "time_use_location_14", "time_use_location_15"
       ,"time_use_location_16", "time_use_location_17","hh_size","hh_member_age_1","hh_member_age_2","hh_member_age_3","hh_member_age_4",                  
       "hh_member_age_5",                   "hh_member_age_6",                   "hh_member_age_7",                   "hh_member_age_8",                  
       "hh_member_age_9",                   "hh_member_age_10",                  "hh_member_age_11",                  "hh_member_age_12",                 
       "hh_member_gender_1",                "hh_member_gender_2",                "hh_member_gender_3",                "hh_member_gender_4",               
       "hh_member_gender_5",                "hh_member_gender_6",                "hh_member_gender_7",                "hh_member_gender_8",               
       "hh_member_gender_9",                "hh_member_gender_10",               "hh_member_gender_11",               "hh_member_gender_12",              
       "hh_member_present_1",               "hh_member_present_2",               "hh_member_present_3",               "hh_member_present_4",             
       "hh_member_present_5",               "hh_member_present_6",               "hh_member_present_7",               "hh_member_present_8",              
       "hh_member_present_9",               "hh_member_present_10",              "hh_member_present_11",              "hh_member_present_12")

TUD<-part[var]

library(car)

TUD[,c(6:22)] <- lapply(TUD[,c(6:22)], function(x) 
  recode(x,"1='home';2='school';3='school';
         4='work';5='transport';6='other';7='leisure';8='other';9='nonfilled'"))

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

#####Calculate total duration spent on each location####
names(TUD)

location <- function (x,y){
  a<-rep(0,length(TUD[,1]))
  a1<-a;a2<-a;a3<-a;a4<-a;a5<-a;a6<-a;a7<-a;a8<-a;a9<-a;a10<-a;a11<-a;a12<-a;a13<-a;a14<-a;a15<-a;a16<-a;a17<-a
  for (i in 1:length(TUD[,1])) {
    if (x[i,6]==y){a1[i] <- a1[i]+ 3}
    if (x[i,7]==y){a2[i] <- a2[i]+ 1}
    if (x[i,8]==y){a3[i] <- a3[i]+ 1}
    if (x[i,9]==y){a4[i] <- a4[i]+ 1}
    if (x[i,10]==y){a5[i] <- a5[i]+ 1}
    if (x[i,11]==y){a6[i] <- a6[i]+ 1}
    if (x[i,12]==y){a7[i] <- a7[i]+ 1}
    if (x[i,13]==y){a8[i] <- a8[i]+ 1}
    if (x[i,14]==y){a9[i] <- a9[i]+ 1}
    if (x[i,15]==y){a10[i] <- a10[i]+ 1}
    if (x[i,16]==y){a11[i] <- a11[i]+ 1}
    if (x[i,17]==y){a12[i] <- a12[i]+ 1}
    if (x[i,18]==y){a13[i] <- a13[i]+ 1}
    if (x[i,19]==y){a14[i] <- a14[i]+ 2}
    if (x[i,20]==y){a15[i] <- a15[i]+ 2}
    if (x[i,21]==y){a16[i] <- a16[i]+ 2}
    if (x[i,22]==y){a17[i] <- a17[i]+ 3}
    a[i]<-a1[i] +a2[i]+a3[i]+a4[i]+a5[i]+a6[i]+a7[i]+a8[i]+a9[i]+a10[i]+a11[i]+a12[i]+a13[i]+a14[i]+a15[i]+a16[i]+a17[i]}
  return (a)}

TUD$home<-location(TUD,"home")
TUD$school<- location(TUD,"school")
TUD$work <- location(TUD,"work")
TUD$transport <- location(TUD,"transport")
TUD$leisure <- location(TUD,"leisure")
TUD$other <- location(TUD,"other")
TUD$nonfilled <- location(TUD,"nonfilled")

TUD$age_cat <- cut(TUD$participant_age, breaks=c(0, 3, 6, 12,18,25,30,35,40,45,50,55,60,65,70,75,80,90),right = FALSE)


TUD1<-TUD[TUD$participant_age>=25 & TUD$participant_age<=65,]
TUD1$with_child<-NA
TUD1$with_child[TUD1$hh_member_age_1<13|TUD1$hh_member_age_2<13|TUD1$hh_member_age_3<13|TUD1$hh_member_age_4<13|TUD1$hh_member_age_5<13
                |TUD1$hh_member_age_6<13|TUD1$hh_member_age_7<13|TUD1$hh_member_age_8<13|TUD1$hh_member_age_9<13
                |TUD1$hh_member_age_10<13|TUD1$hh_member_age_11<13|TUD1$hh_member_age_12<13]<- 3

TUD1$with_child[is.na(TUD1$with_child)]<-2
TUD1$with_child[TUD1$hh_size==1]<-1


#weekday##
#TUD1<-TUD1[TUD1$week=="Weekday",]
TUD1<-TUD1[TUD1$week=="Weekend",]
#TUD1<-TUD1[TUD1$holiday=="Y",]
TUD1<-TUD1[TUD1$holiday=="N",]

mean_with_child<-aggregate(TUD1[,c(62:68)],by=list(TUD1$with_child,TUD1$participant_gender),mean)

mean_with_child<-mean_with_child[,-c(1,2)]

mean_with_child_female<-data.matrix(mean_with_child[1:3,])
mean_with_child_male<-data.matrix(mean_with_child[4:6,])

windows(width=18, height=10)
par(mfrow=c(1,2))
barplot(mean_with_child_female, main="Female",names.arg=c("home","school","work","transport","leisure","other","nonfilled"),
        ylab="Average number of hours", xlab="", ylim=c(0,20),col=c("blue","red","pink"),beside=TRUE)
legend("topright", legend=c("Living alone","Living without children less than 13","Living with children less than 13"),fill=c("blue","red","pink"))

barplot(mean_with_child_male, main="Male",names.arg=c("home","school","work","transport","leisure","other","nonfilled"),
        ylab="", xlab="", ylim=c(0,20),col=c("blue","red","pink"),beside=TRUE)


#################Couple with or without children less than 13/18 years old#######

TUD1<-TUD[TUD$participant_age>=25 & TUD$participant_age<=65,]
TUD1$with_child<-NULL

##less than 13
TUD1$with_child[TUD1$hh_member_age_1<13|TUD1$hh_member_age_2<13|TUD1$hh_member_age_3<13|TUD1$hh_member_age_4<13|TUD1$hh_member_age_5<13
                |TUD1$hh_member_age_6<13|TUD1$hh_member_age_7<13|TUD1$hh_member_age_8<13|TUD1$hh_member_age_9<13
                |TUD1$hh_member_age_10<13|TUD1$hh_member_age_11<13|TUD1$hh_member_age_12<13]<- 1

###less than 18
TUD1$with_child[TUD1$hh_member_age_1<18|TUD1$hh_member_age_2<18|TUD1$hh_member_age_3<18|TUD1$hh_member_age_4<18|TUD1$hh_member_age_5<18
              |TUD1$hh_member_age_6<18|TUD1$hh_member_age_7<18|TUD1$hh_member_age_8<18|TUD1$hh_member_age_9<18
              |TUD1$hh_member_age_10<18|TUD1$hh_member_age_11<18|TUD1$hh_member_age_12<18]<- 1

TUD1$with_child[is.na(TUD1$with_child)]<-2
#TUD1$with_child[TUD1$hh_size==1]<-NA
#Excude people living alone

TUD1<-TUD1[TUD1$hh_size>1,]


TUD1$time<- factor(TUD1$week):factor(TUD1$holiday)
TUD1$time<-as.character(TUD1$time)
TUD1<-TUD1[!is.na(TUD1$time),]

TUD_1<-TUD1[TUD1$time=="Weekday:N",]
TUD_2<-TUD1[TUD1$time=="Weekend:N",]
TUD_3<-TUD1[TUD1$time=="Weekday:Y",]
TUD_4<-TUD1[TUD1$time=="Weekend:Y",]

mean_with_child_1<-aggregate(TUD_1[,c(62:68)],by=list(TUD_1$with_child),mean)
mean_with_child_1<-mean_with_child_1[,-c(1)]
mean_with_child_1<-data.matrix((mean_with_child_1))

mean_with_child_2<-aggregate(TUD_2[,c(62:68)],by=list(TUD_2$with_child),mean)
mean_with_child_2<-mean_with_child_2[,-c(1)]
mean_with_child_2<-data.matrix((mean_with_child_2))

mean_with_child_3<-aggregate(TUD_3[,c(62:68)],by=list(TUD_3$with_child),mean)
mean_with_child_3<-mean_with_child_3[,-c(1)]
mean_with_child_3<-data.matrix((mean_with_child_3))

mean_with_child_4<-aggregate(TUD_4[,c(62:68)],by=list(TUD_4$with_child),mean)
mean_with_child_4<-mean_with_child_4[,-c(1)]
mean_with_child_4<-data.matrix((mean_with_child_4))

##children less than 13####
par(mfrow=c(2,2))
barplot(mean_with_child_1, main="Weekday:Regular",names.arg=c("home","school","work","transport","leisure","other","nonfilled"),
        ylab="Average number of hours", xlab="", ylim=c(0,20),col=c("#9ebcda","#8856a7"),beside=TRUE,cex.axis = 1.5,cex.names = 1.5,cex.lab=1.5,cex.main=1.5)
legend("topright", legend=c("With children less than 13","Without children less than 13"),fill=c("#9ebcda","#8856a7"),cex = 1.5)

barplot(mean_with_child_2, main="Weekend:Regular",names.arg=c("home","school","work","transport","leisure","other","nonfilled"),
        ylab="", xlab="", ylim=c(0,20),col=c("#9ebcda","#8856a7"),beside=TRUE,cex.axis = 1.5,cex.names = 1.5,cex.lab=1.5,cex.main=1.5)

barplot(mean_with_child_3, main="Weekday:Holiday",names.arg=c("home","school","work","transport","leisure","other","nonfilled"),
        ylab="", xlab="", ylim=c(0,20),col=c("#9ebcda","#8856a7"),beside=TRUE,cex.axis = 1.5,cex.names = 1.5,cex.lab=1.5,cex.main=1.5)

barplot(mean_with_child_4, main="Weekend:Holiday",names.arg=c("home","school","work","transport","leisure","other","nonfilled"),
        ylab="", xlab="", ylim=c(0,20),col=c("#9ebcda","#8856a7"),beside=TRUE,cex.axis = 1.5,cex.names = 1.5,cex.lab=1.5,cex.main=1.5)


###Children less than 18
par(mfrow=c(2,2))
barplot(mean_with_child_1, main="Weekday:Regular",names.arg=c("home","school","work","transport","leisure","other","nonfilled"),
        ylab="Average number of hours", xlab="", ylim=c(0,20),col=c("#9ebcda","#8856a7"),beside=TRUE,cex.axis = 1.5,cex.names = 1.5,cex.lab=1.5,cex.main=1.5)
legend("topright", legend=c("With children less than 18","Without children less than 18"),fill=c("#9ebcda","#8856a7"))

barplot(mean_with_child_2, main="Weekend:Regular",names.arg=c("home","school","work","transport","leisure","other","nonfilled"),
        ylab="", xlab="", ylim=c(0,20),col=c("#9ebcda","#8856a7"),beside=TRUE,cex.axis = 1.5,cex.names = 1.5,cex.lab=1.5,cex.main=1.5)

barplot(mean_with_child_3, main="Weekday:Holiday",names.arg=c("home","school","work","transport","leisure","other","nonfilled"),
        ylab="", xlab="", ylim=c(0,20),col=c("#9ebcda","#8856a7"),beside=TRUE,cex.axis = 1.5,cex.names = 1.5,cex.lab=1.5,cex.main=1.5)

barplot(mean_with_child_4, main="Weekend:Holiday",names.arg=c("home","school","work","transport","leisure","other","nonfilled"),
        ylab="", xlab="", ylim=c(0,20),col=c("#9ebcda","#8856a7"),beside=TRUE,cex.axis = 1.5,cex.names = 1.5,cex.lab=1.5,cex.main=1.5)



#############line charts#################
library(plyr)
library(ggplot2)
TUD1<-TUD[!is.na(TUD$week),]
TUD1 <- data.frame(TUD[c(1,2,3,4,5,60)], stack(TUD[6:22]))
TUD1<-TUD1[order(TUD1$local_id),]

#View(TUD1)

#Change order of values
names(TUD1)[names(TUD1)=="ind"]<- "Time"
names(TUD1)[names(TUD1)=="values"]<- "Location"

TUD1$Time <- factor(TUD1$Time, levels = c("24-02h","02-05h","05-08h", "08-09h", "09-10h", "10-11h", "11-12h", "12-13h", "13-14h", "14-15h",
                                          "15-16h", "16-17h", "17-18h", "18-19h", "19-20h", "20-22h", "22-24h"))



gender = count(TUD1, c('Time', 'Location',"participant_gender","week"))

gender<-ddply(gender,.(Time,participant_gender,week),transform,percent = freq/sum(freq) * 100)


###At work
gender<-gender[gender$Location=="work",]

gender$week<-as.character(gender$week)
gender$week[gender$week=="Weekday"]<-"At work in weekdays"
gender$week[gender$week=="Weekend"]<-"At work in weekends"

gender$participant_gender<-as.character(gender$participant_gender)
gender$participant_gender[gender$participant_gender=="F"]<-"Female"
gender$participant_gender[gender$participant_gender=="M"]<-"Male"

gender<-gender[order(gender$participant_gender),]

         ggplot(gender, aes(x=Time, y=percent, group=participant_gender)) +
         geom_line(aes(linetype=participant_gender))+
         geom_point(aes(shape=participant_gender))+ theme_bw()+
  theme(legend.title=element_blank())+
           geom_line(aes(color=participant_gender))+
         xlab("")+
           ylab("Percentage (%)")+
           theme(panel.border = element_blank())+
           facet_grid(.~week) +
           theme(axis.title = element_text(size=15),axis.text.x = element_text(size=13, angle = 90,vjust=0.5, hjust = 1),legend.position = "right",legend.text = element_text(size = 15))+
           theme(legend.title=element_blank())+
           theme(strip.text.x = element_text(size=15),strip.text.y = element_text(size = 13,angle=0),strip.background = element_rect(fill = "white",colour = "NA"))
         
         
###At home
         gender<-gender[gender$Location=="home",]
         gender$week<-as.character(gender$week)
         gender$week[gender$week=="Weekday"]<-"At home in weekdays"
         gender$week[gender$week=="Weekend"]<-"At home in weekends"
         
         gender$participant_gender<-as.character(gender$participant_gender)
         gender$participant_gender[gender$participant_gender=="F"]<-"Female"
         gender$participant_gender[gender$participant_gender=="M"]<-"Male"
         
         gender<-gender[order(gender$participant_gender),]
         
         ggplot(gender, aes(x=Time, y=percent, group=participant_gender)) +
           geom_line(aes(linetype=participant_gender))+
           geom_point(aes(shape=participant_gender))+ theme_bw()+
           theme(legend.title=element_blank())+
           geom_line(aes(color=participant_gender))+
           xlab("")+
           ylab("Percentage (%)")+
           theme(panel.border = element_blank())+
           facet_grid(.~week) +
           theme(axis.title = element_text(size=15),axis.text.x = element_text(size=13, angle = 90, hjust = 1),legend.position = "right",legend.text = element_text(size = 15))+
           theme(legend.title=element_blank())+
           theme(strip.text.x = element_text(size=15),strip.text.y = element_text(size = 13,angle=0),strip.background = element_rect(fill = "white",colour = "NA"))
         
         
####Leisure
         gender<-gender[gender$Location=="leisure",]
         gender$week<-as.character(gender$week)
         gender$week[gender$week=="Weekday"]<-"At leisure places in weekdays"
         gender$week[gender$week=="Weekend"]<-"At leisure places in weekends"
         
         gender$participant_gender<-as.character(gender$participant_gender)
         gender$participant_gender[gender$participant_gender=="F"]<-"Female"
         gender$participant_gender[gender$participant_gender=="M"]<-"Male"
         
         gender<-gender[order(gender$participant_gender),]
         
         ggplot(gender, aes(x=Time, y=percent, group=participant_gender)) +
           geom_line(aes(linetype=participant_gender))+
           geom_point(aes(shape=participant_gender))+ theme_bw()+
           theme(legend.title=element_blank())+
           geom_line(aes(color=participant_gender))+
           xlab("")+
           ylab("Percentage (%)")+
           theme(panel.border = element_blank())+
           facet_grid(.~week) +
           theme(axis.title = element_text(size=15),axis.text.x = element_text(size=13, angle = 90, hjust = 1),legend.position = "right",legend.text = element_text(size = 15))+
           theme(legend.title=element_blank())+
           theme(strip.text.x = element_text(size=15),strip.text.y = element_text(size = 13,angle=0),strip.background = element_rect(fill = "white",colour = "NA"))
         
         
         
         
         
       