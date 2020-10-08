
#############################################################################
# SOCIAL CONTACT DATA ANALYSIS
#
# Copyright 2019, SIMID
#############################################################################
#
# DATA EXPLORATION ANALYSIS
#
#############################################################################


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
var<-c("local_id","participant_age","participant_gender","dayofweek","holiday", paste0("time_use_location_",1:17),"hh_size","hh_parental_type")
TUD<-part[var]

###Recode multiple variables in R
#install.packages("car")
library(car)

TUD[,c(6:22)] <- lapply(TUD[,c(6:22)], function(x) 
recode(x,"1='home';2='kinder-garten';3='school';
4='workplace';5='transport';6='family';7='leisure';8='other';9='nonfilled'"))


#names(TUD)[names(TUD)=="time_use_location_1"]<-"05-08h"
#names(TUD)[names(TUD)=="time_use_location_2"]<-"08-09h"
#names(TUD)[names(TUD)=="time_use_location_3"]<-"09-10h"
#names(TUD)[names(TUD)=="time_use_location_4"]<-"10-11h"
#names(TUD)[names(TUD)=="time_use_location_5"]<-"11-12h"
#names(TUD)[names(TUD)=="time_use_location_6"]<-"12-13h"
#names(TUD)[names(TUD)=="time_use_location_7"]<-"13-14h"
#names(TUD)[names(TUD)=="time_use_location_8"]<-"14-15h"
#names(TUD)[names(TUD)=="time_use_location_9"]<-"15-16h"
#names(TUD)[names(TUD)=="time_use_location_10"]<-"16-17h"
#names(TUD)[names(TUD)=="time_use_location_11"]<-"17-18h"
#names(TUD)[names(TUD)=="time_use_location_12"]<-"18-19h"
#names(TUD)[names(TUD)=="time_use_location_13"]<-"19-20h"
#names(TUD)[names(TUD)=="time_use_location_14"]<-"20-22h"
#names(TUD)[names(TUD)=="time_use_location_15"]<-"22-24h"
#names(TUD)[names(TUD)=="time_use_location_16"]<-"24-02h"
#names(TUD)[names(TUD)=="time_use_location_17"]<-"02-05h"

TUD$week<-NULL
TUD$week[TUD$dayofweek==0|TUD$dayofweek==6]<- "Weekend"
TUD$week[TUD$dayofweek==1|TUD$dayofweek==2|TUD$dayofweek==3|TUD$dayofweek==4|TUD$dayofweek==5]<- "Weekday"

TUD$version<-NULL
TUD$version[TUD$participant_age<12]<-"Version 1"
TUD$version[TUD$participant_age>=12&TUD$participant_age<=60]<-"Version 2"
TUD$version[TUD$participant_age>60]<-"Version 3"

TUD$holiday[is.na(TUD$week)]<-NA

##select columns: time slot
num_time_slots <-17
colnames_time_use_location <- paste0("time_use_location_",1:num_time_slots)


#set time slot sizes
t1 <- colnames_time_use_location[2:13]  ### time slots of 1 hour
t2 <- colnames_time_use_location[14:16] ### time slots of 2 hours
t3 <- colnames_time_use_location[c(1,17)]  ### time slots of 3 hours

##### Function to calculate TUD at a given location
TUD_location <- TUD
get_time_at_location <- function(TUD_location,location_type){
  
  #TUD_location[,colnames_time_use_location][TUD_location[,colnames_time_use_location]=="nonfilled"] <- NA
  TUD_location[,colnames_time_use_location][TUD_location[,colnames_time_use_location] != location_type] <- 0
  TUD_location[,t1][TUD_location[,t1]==location_type] <- 1
  TUD_location[,t2][TUD_location[,t2]==location_type] <- 2
  TUD_location[,t3][TUD_location[,t3]==location_type] <- 3
  
  for (i in colnames_time_use_location){
    TUD_location[,i]<-as.numeric(TUD_location[,i])
  }
  
  total_time<-rowSums(TUD_location[,colnames_time_use_location])
  return(total_time)
}



TUD$home<-get_time_at_location(TUD,'home')
TUD$kinder_garten <- get_time_at_location(TUD,"kinder-garten")
TUD$school<- get_time_at_location(TUD,"school")
TUD$workplace <- get_time_at_location(TUD,"workplace")
TUD$transport <- get_time_at_location(TUD,"transport")
TUD$family <- get_time_at_location(TUD,"family")
TUD$leisure <- get_time_at_location(TUD,"leisure")
TUD$other <- get_time_at_location(TUD,"other")
TUD$nonfilled <- get_time_at_location(TUD,"nonfilled")


####Table 1###
#location <- function (x,y){
#  a<-rep(0,length(TUD[,1]))
#  a1<-a;a2<-a;a3<-a;a4<-a;a5<-a;a6<-a;a7<-a;a8<-a;a9<-a;a10<-a;a11<-a;a12<-a;a13<-a;a14<-a;a15<-a;a16<-a;a17<-a
#  for (i in 1:length(TUD[,1])) {
#    if (x[i,6]==y){a1[i] <- a1[i]+ 3}
#    if (x[i,7]==y){a2[i] <- a2[i]+ 1}
#    if (x[i,8]==y){a3[i] <- a3[i]+ 1}
#    if (x[i,9]==y){a4[i] <- a4[i]+ 1}
#    if (x[i,10]==y){a5[i] <- a5[i]+ 1}
#    if (x[i,11]==y){a6[i] <- a6[i]+ 1}
#    if (x[i,12]==y){a7[i] <- a7[i]+ 1}
#    if (x[i,13]==y){a8[i] <- a8[i]+ 1}
#    if (x[i,14]==y){a9[i] <- a9[i]+ 1}
#    if (x[i,15]==y){a10[i] <- a10[i]+ 1}
#    if (x[i,16]==y){a11[i] <- a11[i]+ 1}
#    if (x[i,17]==y){a12[i] <- a12[i]+ 1}
#    if (x[i,18]==y){a13[i] <- a13[i]+ 1}
#    if (x[i,19]==y){a14[i] <- a14[i]+ 2}
#    if (x[i,20]==y){a15[i] <- a15[i]+ 2} #remove 3 time slots
#    if (x[i,21]==y){a16[i] <- a16[i]+ 2}
#    if (x[i,22]==y){a17[i] <- a17[i]+ 3}
#    a[i]<-a1[i] +a2[i]+a3[i]+a4[i]+a5[i]+a6[i]+a7[i]+a8[i]+a9[i]+a10[i]+a11[i]+a12[i]+a13[i]+a14[i]+a15[i]+a16[i]+a17[i]}
#  return (a)}

#TUD$home<-location(TUD,"home")
#TUD$kinder_garten <- location(TUD,"kinder-garten")
#TUD$school<- location(TUD,"school")
#TUD$workplace <- location(TUD,"workplace")
#TUD$transport <- location(TUD,"transport")
#TUD$family <- location(TUD,"family")
#TUD$leisure <- location(TUD,"leisure")
#TUD$other <- location(TUD,"other")
#TUD$nonfilled <- location(TUD,"nonfilled")

##check:OK
TUD$total<-TUD$home + TUD$kinder_garten+ TUD$school + TUD$workplace + TUD$transport + TUD$family + TUD$leisure + TUD$other + TUD$nonfilled 

TUD$home_perc<-round((TUD$home/TUD$total)*100,digits=1)
TUD$kinder_garten_perc<-round((TUD$kinder_garten/TUD$total)*100,digits=1)
TUD$school_perc<-round((TUD$school/TUD$total)*100,digits=1)
TUD$workplace_perc<-round((TUD$workplace/TUD$total)*100,digits=1)
TUD$transport_perc<-round((TUD$transport/TUD$total)*100,digits=1)
TUD$family_perc<-round((TUD$family/TUD$total)*100,digits=1)
TUD$leisure_perc<-round((TUD$leisure/TUD$total)*100,digits=1)
TUD$other_perc<-round((TUD$other/TUD$total)*100,digits=1)
TUD$nonfilled_perc<-round((TUD$nonfilled/TUD$total)*100,digits=1)

TUD$total_perc <- TUD$home_perc + TUD$kinder_garten_perc + TUD$school_perc + TUD$workplace_perc + TUD$transport_perc +
  TUD$family_perc + TUD$leisure_perc + TUD$other_perc + TUD$nonfilled_perc

#check
#table(TUD$total_perc)

#####Table time time with other variables####
TUD_general<- data.frame(c(mean(TUD$home_perc),mean(TUD$kinder_garten_perc),mean(TUD$school_perc),mean(TUD$workplace_perc),mean(TUD$transport_perc),mean(TUD$family_perc),mean(TUD$leisure_perc),mean(TUD$other_perc),mean(TUD$nonfilled_perc)))


###Age###
attach(TUD)

TUD$age_cat[participant_age<3]<- 1
TUD$age_cat[participant_age>=3 & participant_age<6]<- 2
TUD$age_cat[participant_age>=6 & participant_age<12]<- 3
TUD$age_cat[participant_age>=12 & participant_age<18]<- 4
TUD$age_cat[participant_age>=18 & participant_age<25]<- 5
TUD$age_cat[participant_age>=25 & participant_age<45]<- 6
TUD$age_cat[participant_age>=45 & participant_age<=64]<- 7
TUD$age_cat[participant_age>=65]<- 8


TUD_age<-data.frame(cbind(aggregate(home_perc,by=list(TUD$age_cat),mean),aggregate(kinder_garten_perc,by=list(TUD$age_cat),mean),aggregate(school_perc,by=list(TUD$age_cat),mean),
                          aggregate(workplace_perc,by=list(TUD$age_cat),mean),aggregate(transport_perc,by=list(TUD$age_cat),mean),aggregate(family_perc,by=list(TUD$age_cat),mean),
                          aggregate(leisure_perc,by=list(TUD$age_cat),mean),aggregate(other_perc,by=list(TUD$age_cat),mean),aggregate(nonfilled_perc,by=list(TUD$age_cat),mean)))

TUD_age<-TUD_age[,-c(1,3,5,7,9,11,13,15,17)]
TUD_age<-round(TUD_age,digits = 2)
write.table(TUD_age,"clipboard",sep="\t", row.names=FALSE)

##merge school+kindergarten, leisure + family + other
TUD$school_merge<-TUD$school_perc + TUD$kinder_garten_perc
TUD$other_merge<-TUD$leisure_perc + TUD$family_perc + TUD$other_perc

attach(TUD)
TUD_age<-data.frame(cbind(aggregate(home_perc,by=list(TUD$age_cat),mean),aggregate(school_merge,by=list(TUD$age_cat),mean),
                          aggregate(workplace_perc,by=list(TUD$age_cat),mean),aggregate(transport_perc,by=list(TUD$age_cat),mean),
                          aggregate(other_merge,by=list(TUD$age_cat),mean),aggregate(nonfilled_perc,by=list(TUD$age_cat),mean)))
TUD_age<-TUD_age[,-c(1,3,5,7,9,11)]
TUD_age<-round(TUD_age,digits = 2)
write.table(TUD_age,"clipboard",sep="\t", row.names=FALSE)



##Gender####
TUD_gender<- data.frame(cbind(aggregate(home_perc,by=list(TUD$participant_gender),mean),aggregate(kinder_garten_perc,by=list(TUD$participant_gender),mean),aggregate(school_perc,by=list(TUD$participant_gender),mean),
                              aggregate(workplace_perc,by=list(TUD$participant_gender),mean),aggregate(transport_perc,by=list(TUD$participant_gender),mean),aggregate(family_perc,by=list(TUD$participant_gender),mean),
                              aggregate(leisure_perc,by=list(TUD$participant_gender),mean),aggregate(other_perc,by=list(TUD$participant_gender),mean),aggregate(nonfilled_perc,by=list(TUD$participant_gender),mean)))

TUD_gender<-TUD_gender[,-c(1,3,5,7,9,11,13,15,17)]
TUD_gender<-round(TUD_gender,digits = 2)
write.table(TUD_gender,"clipboard",sep="\t", row.names=FALSE)

#merge
TUD_gender<-data.frame(cbind(aggregate(home_perc,by=list(TUD$participant_gender),mean),aggregate(school_merge,by=list(TUD$participant_gender),mean),
                          aggregate(workplace_perc,by=list(TUD$participant_gender),mean),aggregate(transport_perc,by=list(TUD$participant_gender),mean),
                          aggregate(other_merge,by=list(TUD$participant_gender),mean),aggregate(nonfilled_perc,by=list(TUD$participant_gender),mean)))
TUD_gender<-TUD_gender[,-c(1,3,5,7,9,11)]
TUD_gender<-round(TUD_gender,digits = 2)
write.table(TUD_gender,"clipboard",sep="\t", row.names=FALSE)


####Day types#####
TUD_week<- data.frame(cbind(aggregate(home_perc,by=list(TUD$week),mean),aggregate(kinder_garten_perc,by=list(TUD$week),mean),aggregate(school_perc,by=list(TUD$week),mean),
                            aggregate(workplace_perc,by=list(TUD$week),mean),aggregate(transport_perc,by=list(TUD$week),mean),aggregate(family_perc,by=list(TUD$week),mean),
                            aggregate(leisure_perc,by=list(TUD$week),mean),aggregate(other_perc,by=list(TUD$week),mean),aggregate(nonfilled_perc,by=list(TUD$week),mean)))

TUD_week<-TUD_week[,-c(1,3,5,7,9,11,13,15,17)]
TUD_week<-round(TUD_week,digits = 2)
write.table(TUD_week,"clipboard",sep="\t", row.names=FALSE)

#merge
TUD_week<-data.frame(cbind(aggregate(home_perc,by=list(TUD$week),mean),aggregate(school_merge,by=list(TUD$week),mean),
                             aggregate(workplace_perc,by=list(TUD$week),mean),aggregate(transport_perc,by=list(TUD$week),mean),
                             aggregate(other_merge,by=list(TUD$week),mean),aggregate(nonfilled_perc,by=list(TUD$week),mean)))
TUD_week<-TUD_week[,-c(1,3,5,7,9,11)]
TUD_week<-round(TUD_week,digits = 2)
write.table(TUD_week,"clipboard",sep="\t", row.names=FALSE)


####period types#####
TUD_period<- data.frame(cbind(aggregate(home_perc,by=list(TUD$holiday),mean),aggregate(kinder_garten_perc,by=list(TUD$holiday),mean),aggregate(school_perc,by=list(TUD$holiday),mean),
                            aggregate(workplace_perc,by=list(TUD$holiday),mean),aggregate(transport_perc,by=list(TUD$holiday),mean),aggregate(family_perc,by=list(TUD$holiday),mean),
                            aggregate(leisure_perc,by=list(TUD$holiday),mean),aggregate(other_perc,by=list(TUD$holiday),mean),aggregate(nonfilled_perc,by=list(TUD$holiday),mean)))

TUD_period<-TUD_period[,-c(1,3,5,7,9,11,13,15,17)]
TUD_period<-round(TUD_period,digits = 2)
write.table(TUD_period,"clipboard",sep="\t", row.names=FALSE)

#merge
TUD_period<-data.frame(cbind(aggregate(home_perc,by=list(TUD$holiday),mean),aggregate(school_merge,by=list(TUD$holiday),mean),
                           aggregate(workplace_perc,by=list(TUD$holiday),mean),aggregate(transport_perc,by=list(TUD$holiday),mean),
                           aggregate(other_merge,by=list(TUD$holiday),mean),aggregate(nonfilled_perc,by=list(TUD$holiday),mean)))
TUD_period<-TUD_period[,-c(1,3,5,7,9,11)]
TUD_period<-round(TUD_period,digits = 2)
write.table(TUD_period,"clipboard",sep="\t", row.names=FALSE)


###household parental type
#merge
TUD_hhs<-data.frame(cbind(aggregate(home_perc,by=list(TUD$hh_parental_type),mean),aggregate(school_merge,by=list(TUD$hh_parental_type),mean),
                             aggregate(workplace_perc,by=list(TUD$hh_parental_type),mean),aggregate(transport_perc,by=list(TUD$hh_parental_type),mean),
                             aggregate(other_merge,by=list(TUD$hh_parental_type),mean),aggregate(nonfilled_perc,by=list(TUD$hh_parental_type),mean)))
TUD_hhs<-TUD_hhs[,-c(1,3,5,7,9,11)]
TUD_hhs<-round(TUD_hhs,digits = 2)
write.table(TUD_hhs,"clipboard",sep="\t", row.names=FALSE)



####Illness####
##46 illness cases
#Weekday Weekend 
#35      11 

##Holiday
##N  Y 
##40  6

##age_cat
#1  2  3  4  5  6  7  8 
#5  5  1  2  2 17  9  5 

var_ext<-part[c("local_id","uncommon_day","uncommon_day_reason")]
TUD_illness<-merge(TUD,var_ext,by="local_id")
TUD_illness$illness<-NULL
TUD_illness$illness[TUD_illness$uncommon_day_reason==1]<-"y"
TUD_illness$illness[TUD_illness$uncommon_day_reason!=1]<-"n"
TUD_illness$illness[is.na(TUD_illness$uncommon_day_reason)]<-"n"
TUD_illness$illness[is.na(TUD_illness$uncommon_day)]<-NA  ##7 cases missing

prop.table(table(TUD_illness$illness))

TUD_ill<- data.frame(cbind(aggregate(home_perc,by=list(TUD_illness$illness),mean),aggregate(kinder_garten_perc,by=list(TUD_illness$illness),mean),aggregate(school_perc,by=list(TUD_illness$illness),mean),
                              aggregate(workplace_perc,by=list(TUD_illness$illness),mean),aggregate(transport_perc,by=list(TUD_illness$illness),mean),aggregate(family_perc,by=list(TUD_illness$illness),mean),
                              aggregate(leisure_perc,by=list(TUD_illness$illness),mean),aggregate(other_perc,by=list(TUD_illness$illness),mean),aggregate(nonfilled_perc,by=list(TUD_illness$illness),mean)))

TUD_ill<-TUD_ill[,-c(1,3,5,7,9,11,13,15,17)]
TUD_ill<-round(TUD_ill,digits = 2)
write.table(TUD_ill,"clipboard",sep="\t", row.names=FALSE)

#merge
TUD_ill<-data.frame(cbind(aggregate(home_perc,by=list(TUD_illness$illness),mean),aggregate(school_merge,by=list(TUD_illness$illness),mean),
                             aggregate(workplace_perc,by=list(TUD_illness$illness),mean),aggregate(transport_perc,by=list(TUD_illness$illness),mean),
                             aggregate(other_merge,by=list(TUD_illness$illness),mean),aggregate(nonfilled_perc,by=list(TUD_illness$illness),mean)))
TUD_ill<-TUD_ill[,-c(1,3,5,7,9,11)]
TUD_ill<-round(TUD_ill,digits = 2)
write.table(TUD_ill,"clipboard",sep="\t", row.names=FALSE)

###================================#####
####participants of 25-65 living with children and teenager (less than 19)
###================================#####

part1<-subset(part,part$participant_age>=25&part$participant_age<=65)

hh_member_index<-12
hh_member_age_col<-paste0("hh_age_",1:hh_member_index)

get_children<-function(part_data)
{part_data[,hh_member_age_col][part_data[,hh_member_age_col]<=13]<-1
part_data[,hh_member_age_col][part_data[,hh_member_age_col]>13]<-0
part_data[,hh_member_age_col][is.na(part_data[,hh_member_age_col])]<-0
return(part_data)
}

part2<-get_children(part1)
part2$living_with_children<-NULL
part2$living_with_children<-rowSums(part2[,hh_member_age_col])
part2$living_with_children[part2$living_with_children>=1]<-1
prop.table(table(part2$living_with_children))

hh_age_ext<-part2[c("local_id","living_with_children")]
TUD_living_child<-merge(TUD,hh_age_ext,by="local_id",all.y = T)

attach(TUD_living_child)
TUD_living_children<-data.frame(cbind(aggregate(home_perc,by=list(TUD_living_child$living_with_children),mean),aggregate(school_merge,by=list(TUD_living_child$living_with_children),mean),
                          aggregate(workplace_perc,by=list(TUD_living_child$living_with_children),mean),aggregate(transport_perc,by=list(TUD_living_child$living_with_children),mean),
                          aggregate(other_merge,by=list(TUD_living_child$living_with_children),mean),aggregate(nonfilled_perc,by=list(TUD_living_child$living_with_children),mean)))
TUD_living_children<-TUD_living_children[,-c(1,3,5,7,9,11)]
TUD_living_children<-round(TUD_living_children,digits = 2)
write.table(TUD_living_children,"clipboard",sep="\t", row.names=FALSE)


mean(TUD$home_perc)
mean(TUD$kinder_garten_perc)
mean(TUD$school_perc)
mean(TUD$workplace_perc)
mean(TUD$transport_perc)
mean(TUD$family_perc)
mean(TUD$leisure_perc)
mean(TUD$other_perc)
mean(TUD$nonfilled_perc)

###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#####
#####Cumulative probability over time and by age categories plot####

####Time-use-by-each-location data 
# Weekday vs weekends
# Holiday vs regular
# Age
# Gender
# Travel to work
# Number of contacts

TUD1<-TUD
TUD1 <- data.frame(TUD[c(1,2,3,4,5,23,24,25)], stack(TUD[6:22]))
TUD1<-TUD1[order(TUD1$local_id),]
View(TUD1)

#Change order of values
names(TUD1)[names(TUD1)=="ind"]<- "Time"
names(TUD1)[names(TUD1)=="values"]<- "Location"

TUD1$Time <- factor(TUD1$Time, levels = c("05-08h", "08-09h", "09-10h", "10-11h", "11-12h", "12-13h", "13-14h", "14-15h",
                                         "15-16h", "16-17h", "17-18h", "18-19h", "19-20h", "20-22h", "22-24h", "24-02h","02-05h"))

TUD1$Location <- factor(TUD1$Location,levels=c("home","kinder-garten","school","workplace","transport","family","leisure","other","nonfilled"))


levels(TUD1$Time)
levels(TUD1$Location)


library(plyr)
library(ggplot2)

#1.THE GENERAL TABLE
general<-data.frame(table(TUD1$Time,TUD1$Location))

general$Var1<-as.factor(general$Var1)

# Get the levels for type in the required order
general$Var1 = factor(general$Var1, levels = c(levels(general$Var1)))

# Calculate the percentages
general = ddply(general, .(Var1), transform, percent = Freq/sum(Freq) * 100)

general$percent<-round(general$percent, digits = 2)
general_table<-matrix(general$percent,ncol = 9, byrow = TRUE)

write.table(general_table,"clipboard", sep="\t", row.names=FALSE)

# Format the labels and calculate their positions
general_table= ddply(general_table, .(Var1), transform, pos = (cumsum(Freq) - 0.5 * Freq))
#df$label = paste0(sprintf("%.0f", df$percent), "%")

general$location<-general$Var2
# Plot
ggplot(general, aes(x = factor(Var1), y = percent, fill = location)) +
  geom_bar(stat = "identity", width = 0.95) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("time")


#1.1.version 1
version1<-TUD1[TUD1$version=="Version 1",]

version1<-data.frame(table(version1$Time,version1$Location))

version1$Var1<-as.factor(version1$Var1)

# Get the levels for type in the required order
version1$Var1 = factor(version1$Var1, levels = c(levels(version1$Var1)))

# Calculate the percentages
version1 = ddply(version1, .(Var1), transform, percent = Freq/sum(Freq) * 100)

version1$percent<-round(version1$percent, digits = 2)

version1_table<-matrix(version1$percent,ncol = 9, byrow = TRUE)

write.table(version1_table,"clipboard", sep="\t", row.names=FALSE)


#1.2.version 2
version2<-TUD1[TUD1$version=="Version 2",]

version2<-data.frame(table(version2$Time,version2$Location))

version2$Var1<-as.factor(version2$Var1)

# Get the levels for type in the required order
version2$Var1 = factor(version2$Var1, levels = c(levels(version2$Var1)))

# Calculate the percentages
version2 = ddply(version2, .(Var1), transform, percent = Freq/sum(Freq) * 100)

version2$percent<-round(version2$percent, digits = 2)

version2_table<-matrix(version2$percent,ncol = 9, byrow = TRUE)

write.table(version2_table,"clipboard", sep="\t", row.names=FALSE)


#1.3.version 3
version3<-TUD1[TUD1$version=="Version 3",]

version3<-data.frame(table(version3$Time,version3$Location))

version3$Var1<-as.factor(version3$Var1)

# Get the levels for type in the required order
version3$Var1 = factor(version3$Var1, levels = c(levels(version3$Var1)))

# Calculate the percentages
version3 = ddply(version3, .(Var1), transform, percent = Freq/sum(Freq) * 100)

version3$percent<-round(version3$percent, digits = 2)

version3_table<-matrix(version3$percent,ncol = 9, byrow = TRUE)

write.table(version3_table,"clipboard", sep="\t", row.names=FALSE)

ggplot(version3, aes(x = factor(Var1), y = percent, fill = Var2))+
geom_bar(stat = "identity", width = 0.95)

#put 3 version in one figures

three_version<-rbind(version1,version2,version3)
three_version$version<-rep(c("Version 1","Version 2","Version 3"),each=153)
three_version$location<-three_version$Var2

ggplot(three_version, aes(x = factor(Var1), y = percent, fill = location)) +
  geom_bar(stat = "identity", width = 0.95) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_rect(fill = 'white'),
        axis.title = element_text(size=18),
        axis.text.y = element_text(size=13),
        legend.text =element_text(size=14),
        legend.title = element_blank(),
        strip.text = element_text(size = 20),
        strip.background = element_rect(fill = "white",colour = "NA"))+
  facet_grid(.~version) + 
  xlab("Time slots") +ylab("Percent (%)") +scale_y_continuous(expand = c(0,0))

##1.4.GENDER#
#1.4.1.Male
male<- TUD1[TUD1$participant_gender=="M",]
male<-data.frame(table(male$Time,male$Location))
male$Var1<-as.factor(male$Var1)
male$Var1 = factor(male$Var1, levels = c(levels(male$Var1)))
male= ddply(male, .(Var1), transform, percent = Freq/sum(Freq) * 100)
male$percent<-round(male$percent, digits = 2)

#1.4.2.Male
female<- TUD1[TUD1$participant_gender=="F",]

female<-data.frame(table(female$Time,female$Location))
female$Var1<-as.factor(female$Var1)
female$Var1 = factor(female$Var1, levels = c(levels(female$Var1)))
female= ddply(female, .(Var1), transform, percent = Freq/sum(Freq) * 100)
female$percent<-round(female$percent, digits = 2)

#plot together
gender<-rbind(male,female)
gender$gender<-rep(c("M","F"),each=153)
gender$location<-gender$Var2
ggplot(gender, aes(x = factor(Var1), y = percent, fill = location)) +
  geom_bar(stat = "identity", width = 0.91) + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(.~gender) +
  xlab("time")


#2. Temporal factor
names(TUD1)
table(TUD1$temporal)

TUD1$temporal<-factor(TUD1$week):factor(TUD1$holiday)

#2.1.weekday_regular
weekday_regular<- TUD1[TUD1$temporal=="Weekday:N",]

weekday_regular<-data.frame(table(weekday_regular$Time,weekday_regular$Location))
weekday_regular$Var1<-as.factor(weekday_regular$Var1)
weekday_regular$Var1 = factor(weekday_regular$Var1, levels = c(levels(weekday_regular$Var1)))
weekday_regular = ddply(weekday_regular, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekday_regular$percent<-round(weekday_regular$percent, digits = 2)

#2.1.1.weekday_regular for version 1
weekday_regular_ver1<-TUD1[TUD1$temporal=="Weekday:N" & TUD1$version=="Version 1",]

weekday_regular_ver1<-data.frame(table(weekday_regular_ver1$Time,weekday_regular_ver1$Location))
weekday_regular_ver1$Var1<-as.factor(weekday_regular_ver1$Var1)
weekday_regular_ver1$Var1 = factor(weekday_regular_ver1$Var1, levels = c(levels(weekday_regular_ver1$Var1)))
weekday_regular_ver1 = ddply(weekday_regular_ver1, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekday_regular_ver1$percent<-round(weekday_regular_ver1$percent, digits = 2)


#2.1.2.weekday_regular for version 2
weekday_regular_ver2<-TUD1[TUD1$temporal=="Weekday:N" & TUD1$version=="Version 2",]

weekday_regular_ver2<-data.frame(table(weekday_regular_ver2$Time,weekday_regular_ver2$Location))
weekday_regular_ver2$Var1<-as.factor(weekday_regular_ver2$Var1)
weekday_regular_ver2$Var1 = factor(weekday_regular_ver2$Var1, levels = c(levels(weekday_regular_ver2$Var1)))
weekday_regular_ver2 = ddply(weekday_regular_ver2, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekday_regular_ver2$percent<-round(weekday_regular_ver2$percent, digits = 2)

#2.1.3.weekday_regular for version 3
weekday_regular_ver3<-TUD1[TUD1$temporal=="Weekday:N" & TUD1$version=="Version 3",]

weekday_regular_ver3<-data.frame(table(weekday_regular_ver3$Time,weekday_regular_ver3$Location))
weekday_regular_ver3$Var1<-as.factor(weekday_regular_ver3$Var1)
weekday_regular_ver3$Var1 = factor(weekday_regular_ver3$Var1, levels = c(levels(weekday_regular_ver3$Var1)))
weekday_regular_ver3 = ddply(weekday_regular_ver3, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekday_regular_ver3$percent<-round(weekday_regular_ver3$percent, digits = 2)


#plot together
weekday_verion<-rbind(weekday_regular_ver1,weekday_regular_ver2,weekday_regular_ver3)
weekday_verion$version<-rep(c("version 1","version 2","version 3"),each=153)
weekday_verion$location<-weekday_verion$Var2
weekday_reg<-ggplot(weekday_verion, aes(x = factor(Var1), y = percent, fill = location)) +
  geom_bar(stat = "identity", width = 0.91) + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(.~version) +
  xlab("time")+
ggtitle("Weekday_regular")+
  theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position="none")
  

#2.2.weekend_regular
weekend_regular<- TUD1[TUD1$temporal=="Weekend:N",]
weekend_regular<-data.frame(table(weekend_regular$Time,weekend_regular$Location))
weekend_regular$Var1<-as.factor(weekend_regular$Var1)
weekend_regular$Var1 = factor(weekend_regular$Var1, levels = c(levels(weekend_regular$Var1)))
weekend_regular = ddply(weekend_regular, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekend_regular$percent<-round(weekend_regular$percent, digits = 2)

#2.2.1.weekend_regular for version 1
weekend_regular_ver1<-TUD1[TUD1$temporal=="Weekend:N" & TUD1$version=="Version 1",]

weekend_regular_ver1<-data.frame(table(weekend_regular_ver1$Time,weekend_regular_ver1$Location))
weekend_regular_ver1$Var1<-as.factor(weekend_regular_ver1$Var1)
weekend_regular_ver1$Var1 = factor(weekend_regular_ver1$Var1, levels = c(levels(weekend_regular_ver1$Var1)))
weekend_regular_ver1 = ddply(weekend_regular_ver1, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekend_regular_ver1$percent<-round(weekend_regular_ver1$percent, digits = 2)


#2.2.2.weekend_regular for version 2
weekend_regular_ver2<-TUD1[TUD1$temporal=="Weekend:N" & TUD1$version=="Version 2",]

weekend_regular_ver2<-data.frame(table(weekend_regular_ver2$Time,weekend_regular_ver2$Location))
weekend_regular_ver2$Var1<-as.factor(weekend_regular_ver2$Var1)
weekend_regular_ver2$Var1 = factor(weekend_regular_ver2$Var1, levels = c(levels(weekend_regular_ver2$Var1)))
weekend_regular_ver2 = ddply(weekend_regular_ver2, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekend_regular_ver2$percent<-round(weekend_regular_ver2$percent, digits = 2)

#2.2.3.weekend_regular for version 3
weekend_regular_ver3<-TUD1[TUD1$temporal=="Weekend:N" & TUD1$version=="Version 3",]

weekend_regular_ver3<-data.frame(table(weekend_regular_ver3$Time,weekend_regular_ver3$Location))
weekend_regular_ver3$Var1<-as.factor(weekend_regular_ver3$Var1)
weekend_regular_ver3$Var1 = factor(weekend_regular_ver3$Var1, levels = c(levels(weekend_regular_ver3$Var1)))
weekend_regular_ver3 = ddply(weekend_regular_ver3, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekend_regular_ver3$percent<-round(weekend_regular_ver3$percent, digits = 2)

#plot together
weekend_verion<-rbind(weekend_regular_ver1,weekend_regular_ver2,weekend_regular_ver3)
weekend_verion$version<-rep(c("version 1","version 2","version 3"),each=153)
weekend_verion$location<-weekend_verion$Var2
weekend_reg<- ggplot(weekend_verion, aes(x = factor(Var1), y = percent, fill = location)) +
  geom_bar(stat = "identity", width = 0.91) + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(.~version) +
  xlab("time")+
  ggtitle("Weekend_regular")+
  theme(plot.title = element_text(hjust = 0.5))

#2.3.weekday_holiday
weekday_holiday<- TUD1[TUD1$temporal=="Weekday:Y",]

weekday_holiday<-data.frame(table(weekday_holiday$Time,weekday_holiday$Location))
weekday_holiday$Var1<-as.factor(weekday_holiday$Var1)
weekday_holiday$Var1 = factor(weekday_holiday$Var1, levels = c(levels(weekday_holiday$Var1)))
weekday_holiday = ddply(weekday_holiday, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekday_holiday$percent<-round(weekday_holiday$percent, digits = 2)


#2.3.1.weekday_holiday for version 1
weekday_holiday_ver1<-TUD1[TUD1$temporal=="Weekday:Y" & TUD1$version=="Version 1",]

weekday_holiday_ver1<-data.frame(table(weekday_holiday_ver1$Time,weekday_holiday_ver1$Location))
weekday_holiday_ver1$Var1<-as.factor(weekday_holiday_ver1$Var1)
weekday_holiday_ver1$Var1 = factor(weekday_holiday_ver1$Var1, levels = c(levels(weekday_holiday_ver1$Var1)))
weekday_holiday_ver1 = ddply(weekday_holiday_ver1, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekday_holiday_ver1$percent<-round(weekday_holiday_ver1$percent, digits = 2)


#2.3.2.weekday_holiday for version 2
weekday_holiday_ver2<-TUD1[TUD1$temporal=="Weekday:Y" & TUD1$version=="Version 2",]

weekday_holiday_ver2<-data.frame(table(weekday_holiday_ver2$Time,weekday_holiday_ver2$Location))
weekday_holiday_ver2$Var1<-as.factor(weekday_holiday_ver2$Var1)
weekday_holiday_ver2$Var1 = factor(weekday_holiday_ver2$Var1, levels = c(levels(weekday_holiday_ver2$Var1)))
weekday_holiday_ver2 = ddply(weekday_holiday_ver2, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekday_holiday_ver2$percent<-round(weekday_holiday_ver2$percent, digits = 2)

#2.3.3.weekday_holiday for version 3
weekday_holiday_ver3<-TUD1[TUD1$temporal=="Weekday:Y" & TUD1$version=="Version 3",]

weekday_holiday_ver3<-data.frame(table(weekday_holiday_ver3$Time,weekday_holiday_ver3$Location))
weekday_holiday_ver3$Var1<-as.factor(weekday_holiday_ver3$Var1)
weekday_holiday_ver3$Var1 = factor(weekday_holiday_ver3$Var1, levels = c(levels(weekday_holiday_ver3$Var1)))
weekday_holiday_ver3 = ddply(weekday_holiday_ver3, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekday_holiday_ver3$percent<-round(weekday_holiday_ver3$percent, digits = 2)

#plot together
Weekday_holiday_verion<-rbind(weekday_holiday_ver1,weekday_holiday_ver2,weekday_holiday_ver3)
Weekday_holiday_verion $version<-rep(c("version 1","version 2","version 3"),each=153)
Weekday_holiday_verion $location<-weekend_verion$Var2
weekday_ho<-ggplot(Weekday_holiday_verion, aes(x = factor(Var1), y = percent, fill = location)) +
  geom_bar(stat = "identity", width = 0.91) + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(.~version) +
  xlab("time")+
  ggtitle("Weekday_holiday")+
  theme(plot.title = element_text(hjust = 0.5))+ theme(legend.position="none")


#2.4.weekend_holiday
weekend_holiday<- TUD1[TUD1$temporal=="Weekend:Y",]

weekend_holiday<-data.frame(table(weekend_holiday$Time,weekend_holiday$Location))
weekend_holiday$Var1<-as.factor(weekend_holiday$Var1)
weekend_holiday$Var1 = factor(weekend_holiday$Var1, levels = c(levels(weekend_holiday$Var1)))
weekend_holiday = ddply(weekend_holiday, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekend_holiday$percent<-round(weekend_holiday$percent, digits = 2)

#2.4.1.weekend_holiday for version 1
weekend_holiday_ver1<-TUD1[TUD1$temporal=="Weekend:Y" & TUD1$version=="Version 1",]

weekend_holiday_ver1<-data.frame(table(weekend_holiday_ver1$Time,weekend_holiday_ver1$Location))
weekend_holiday_ver1$Var1<-as.factor(weekend_holiday_ver1$Var1)
weekend_holiday_ver1$Var1 = factor(weekend_holiday_ver1$Var1, levels = c(levels(weekend_holiday_ver1$Var1)))
weekend_holiday_ver1 = ddply(weekend_holiday_ver1, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekend_holiday_ver1$percent<-round(weekend_holiday_ver1$percent, digits = 2)


#2.4.2.weekend_holiday for version 2
weekend_holiday_ver2<-TUD1[TUD1$temporal=="Weekend:Y" & TUD1$version=="Version 2",]

weekend_holiday_ver2<-data.frame(table(weekend_holiday_ver2$Time,weekend_holiday_ver2$Location))
weekend_holiday_ver2$Var1<-as.factor(weekend_holiday_ver2$Var1)
weekend_holiday_ver2$Var1 = factor(weekend_holiday_ver2$Var1, levels = c(levels(weekend_holiday_ver2$Var1)))
weekend_holiday_ver2 = ddply(weekend_holiday_ver2, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekend_holiday_ver2$percent<-round(weekend_holiday_ver2$percent, digits = 2)

#2.4.3.weekend_holiday for version 3
weekend_holiday_ver3<-TUD1[TUD1$temporal=="Weekend:Y" & TUD1$version=="Version 3",]

weekend_holiday_ver3<-data.frame(table(weekend_holiday_ver3$Time,weekend_holiday_ver3$Location))
weekend_holiday_ver3$Var1<-as.factor(weekend_holiday_ver3$Var1)
weekend_holiday_ver3$Var1 = factor(weekend_holiday_ver3$Var1, levels = c(levels(weekend_holiday_ver3$Var1)))
weekend_holiday_ver3 = ddply(weekend_holiday_ver3, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekend_holiday_ver3$percent<-round(weekend_holiday_ver3$percent, digits = 2)


#plot together
Weekend_holiday_verion<-rbind(weekend_holiday_ver1,weekend_holiday_ver2,weekend_holiday_ver3)
Weekend_holiday_verion $version<-rep(c("version 1","version 2","version 3"),each=153)
Weekend_holiday_verion $location<-weekend_verion$Var2
weekend_ho<-ggplot(Weekend_holiday_verion, aes(x = factor(Var1), y = percent, fill = location)) +
  geom_bar(stat = "identity", width = 0.91) + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(.~version) +
  xlab("time")+
  ggtitle("Weekend_holiday")+
  theme(plot.title = element_text(hjust = 0.5))


####plot 4 figures: temporal factors and versions####
install.packages("gridExtra")
library("gridExtra")

grid.arrange(weekday_reg,weekend_reg,weekday_ho,weekend_ho, 
             ncol = 2, nrow = 2)

#put 4 options in one figures

temporal<-rbind(weekday_regular,weekend_regular,weekday_holiday,weekend_holiday)
temporal$temporal<-rep(c("weekday_regular","weekend_regular","weekday_holiday","weekend_holiday"),each=153)
temporal$temporal<- factor(temporal$temporal,levels = c("weekday_regular","weekend_regular","weekday_holiday","weekend_holiday"))
temporal$location<-temporal$Var2
ggplot(temporal, aes(x = factor(Var1), y = percent, fill = location)) +
  geom_bar(stat = "identity", width = 0.91) + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(.~temporal) +
  xlab("time")


####3.Compare to other countries###
###Recalculate time use to compare with other countries
#####Calculate total duration spent on each location####
names(TUD)
TUD<-TUD[,-25]

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
    if (x[i,20]==y){a15[i] <- a15[i]+ 0} #remove 3 time slots
    if (x[i,21]==y){a16[i] <- a16[i]+ 0}
    if (x[i,22]==y){a17[i] <- a17[i]+ 0}
    a[i]<-a1[i] +a2[i]+a3[i]+a4[i]+a5[i]+a6[i]+a7[i]+a8[i]+a9[i]+a10[i]+a11[i]+a12[i]+a13[i]+a14[i]+a15[i]+a16[i]+a17[i]}
  return (a)}

TUD$home<-location(TUD,"home")
TUD$kinder_garten <- location(TUD,"kinder-garten")
TUD$school<- location(TUD,"school")
TUD$workplace <- location(TUD,"workplace")
TUD$transport <- location(TUD,"transport")
TUD$family <- location(TUD,"family")
TUD$leisure <- location(TUD,"leisure")
TUD$other <- location(TUD,"other")
TUD$nonfilled <- location(TUD,"nonfilled")

##check
TUD$total<-TUD$home + TUD$kinder_garten+ TUD$school + TUD$workplace + TUD$transport + TUD$family + TUD$leisure + TUD$other + TUD$nonfilled 

TUD$home_perc<-round((TUD$home/TUD$total)*100,digits=1)
TUD$kinder_garten_perc<-round((TUD$kinder_garten/TUD$total)*100,digits=1)
TUD$school_perc<-round((TUD$school/TUD$total)*100,digits=1)
TUD$workplace_perc<-round((TUD$workplace/TUD$total)*100,digits=1)
TUD$transport_perc<-round((TUD$transport/TUD$total)*100,digits=1)
TUD$family_perc<-round((TUD$family/TUD$total)*100,digits=1)
TUD$leisure_perc<-round((TUD$leisure/TUD$total)*100,digits=1)
TUD$other_perc<-round((TUD$other/TUD$total)*100,digits=1)
TUD$nonfilled_perc<-round((TUD$nonfilled/TUD$total)*100,digits=1)

TUD$total_perc <- TUD$home_perc + TUD$kinder_garten_perc + TUD$school_perc + TUD$workplace_perc + TUD$transport_perc +
  TUD$family_perc + TUD$leisure_perc + TUD$other_perc + TUD$nonfilled_perc

#check
#table(TUD$total_perc)

#####Table time time with other variables####
TUD_general<- data.frame(c(mean(TUD$home_perc),mean(TUD$kinder_garten_perc),mean(TUD$school_perc),mean(TUD$workplace_perc),mean(TUD$transport_perc),mean(TUD$family_perc),mean(TUD$leisure_perc),mean(TUD$other_perc),mean(TUD$nonfilled_perc)))


###Age###
attach(TUD)

TUD$age_cat[participant_age<=5]<- 1
TUD$age_cat[participant_age>=6 & participant_age<=11]<- 2
TUD$age_cat[participant_age>=12 & participant_age<=17]<- 3
TUD$age_cat[participant_age>=18 & participant_age<=44]<- 4
TUD$age_cat[participant_age>=45 & participant_age<=64]<- 5
TUD$age_cat[participant_age>=65]<- 6

TUD_age<-data.frame(cbind(aggregate(home_perc,by=list(TUD$age_cat),mean),aggregate(kinder_garten_perc,by=list(TUD$age_cat),mean),aggregate(school_perc,by=list(TUD$age_cat),mean),
                          aggregate(workplace_perc,by=list(TUD$age_cat),mean),aggregate(transport_perc,by=list(TUD$age_cat),mean),aggregate(family_perc,by=list(TUD$age_cat),mean),
                          aggregate(leisure_perc,by=list(TUD$age_cat),mean),aggregate(other_perc,by=list(TUD$age_cat),mean),aggregate(nonfilled_perc,by=list(TUD$age_cat),mean)))

TUD_age<-TUD_age[,-c(1,3,5,7,9,11,13,15,17)]
TUD_age<-round(TUD_age,digits = 1)
write.table(TUD_age,"clipboard",sep="\t", row.names=FALSE)

##Gender####
TUD_gender<- data.frame(cbind(aggregate(home_perc,by=list(TUD$participant_gender),mean),aggregate(kinder_garten_perc,by=list(TUD$participant_gender),mean),aggregate(school_perc,by=list(TUD$participant_gender),mean),
                              aggregate(workplace_perc,by=list(TUD$participant_gender),mean),aggregate(transport_perc,by=list(TUD$participant_gender),mean),aggregate(family_perc,by=list(TUD$participant_gender),mean),
                              aggregate(leisure_perc,by=list(TUD$participant_gender),mean),aggregate(other_perc,by=list(TUD$participant_gender),mean),aggregate(nonfilled_perc,by=list(TUD$participant_gender),mean)))

TUD_gender<-TUD_gender[,-c(1,3,5,7,9,11,13,15,17)]
TUD_gender<-round(TUD_gender,digits = 1)
write.table(TUD_gender,"clipboard",sep="\t", row.names=FALSE)

####Day types#####
TUD_week<- data.frame(cbind(aggregate(home_perc,by=list(TUD$week),mean),aggregate(kinder_garten_perc,by=list(TUD$week),mean),aggregate(school_perc,by=list(TUD$week),mean),
                            aggregate(workplace_perc,by=list(TUD$week),mean),aggregate(transport_perc,by=list(TUD$week),mean),aggregate(family_perc,by=list(TUD$week),mean),
                            aggregate(leisure_perc,by=list(TUD$week),mean),aggregate(other_perc,by=list(TUD$week),mean),aggregate(nonfilled_perc,by=list(TUD$week),mean)))

TUD_week<-TUD_week[,-c(1,3,5,7,9,11,13,15,17)]
TUD_week<-round(TUD_week,digits = 1)
write.table(TUD_week,"clipboard",sep="\t", row.names=FALSE)


######Make a plot to compare with Italy and Zimbawee
names(TUD2)
TUD2<-TUD[-c(20:22)]

#recode location --> 4 options
TUD3 <- data.frame(TUD2[c(1,2,3,4,5,20,21)], stack(TUD2[6:19]))
TUD3<-TUD3[order(TUD3$local_id),]

names(TUD3)[names(TUD3)=="ind"]<- "Time"
names(TUD3)[names(TUD3)=="values"]<- "Location"

table(TUD3$Location)
TUD3$Location[TUD3$Location=="family"|TUD3$Location=="leisure"|TUD3$Location=="transport"|TUD3$Location=="other"]<-"General Community"
TUD3$Location[TUD3$Location=="school"|TUD3$Location=="Kinder-garten"] <- "school"
TUD3$Location[TUD3$Location=="nonfilled"]<- NA

TUD3$Location<-as.character(TUD3$Location)
TUD3$Location[TUD3$Location=="home"]<-"Household"
TUD3$Location<-as.factor(TUD3$Location)

TUD3$Time <- factor(TUD3$Time, levels = c("05-08h", "08-09h", "09-10h", "10-11h", "11-12h", "12-13h", "13-14h", "14-15h",
                                          "15-16h", "16-17h", "17-18h", "18-19h", "19-20h", "20-22h"))


TUD3$Location <- factor(TUD3$Location,levels=c("Household","school","workplace","General Community"))
#follow Italy and Zimbawee to fill in color#
TUD3$Location <- factor(TUD3$Location,levels=c("Household","General Community","school","workplace"))


TUD3$age_cat[TUD3$participant_age<=5] <- "0-5y"
TUD3$age_cat[TUD3$participant_age>=6 & TUD3$participant_age<=18] <- "6-18y"
TUD3$age_cat[TUD3$participant_age>=19 & TUD3$participant_age<=59] <- "19-59y"
TUD3$age_cat[TUD3$participant_age>59] <- "60y+"



####3.1. age_cat + weekday
###3.1.1. age1
weekday_age1<-TUD3[TUD3$age_cat=="0-5y" & TUD3$holiday=="N",]

weekday_age1<-data.frame(table(weekday_age1$Time,weekday_age1$Location))
weekday_age1$Var1<-as.factor(weekday_age1$Var1)
weekday_age1$Var1 = factor(weekday_age1$Var1, levels = c(levels(weekday_age1$Var1)))
weekday_age1 = ddply(weekday_age1, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekday_age1$percent<-round(weekday_age1$percent, digits = 2)

###3.1.2. age2
weekday_age2<-TUD3[TUD3$age_cat=="6-18y" & TUD3$holiday=="N",]

weekday_age2<-data.frame(table(weekday_age2$Time,weekday_age2$Location))
weekday_age2$Var1<-as.factor(weekday_age2$Var1)
weekday_age2$Var1 = factor(weekday_age2$Var1, levels = c(levels(weekday_age2$Var1)))
weekday_age2 = ddply(weekday_age2, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekday_age2$percent<-round(weekday_age2$percent, digits = 2)


###3.1.3. age3
weekday_age3<-TUD3[TUD3$age_cat=="19-59y" & TUD3$holiday=="N",]

weekday_age3<-data.frame(table(weekday_age3$Time,weekday_age3$Location))
weekday_age3$Var1<-as.factor(weekday_age3$Var1)
weekday_age3$Var1 = factor(weekday_age3$Var1, levels = c(levels(weekday_age3$Var1)))
weekday_age3 = ddply(weekday_age3, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekday_age3$percent<-round(weekday_age3$percent, digits = 2)

###3.1.4. age4
weekday_age4<-TUD3[TUD3$age_cat=="60y+" & TUD3$holiday=="N",]

weekday_age4<-data.frame(table(weekday_age4$Time,weekday_age4$Location))
weekday_age4$Var1<-as.factor(weekday_age4$Var1)
weekday_age4$Var1 = factor(weekday_age4$Var1, levels = c(levels(weekday_age4$Var1)))
weekday_age4 = ddply(weekday_age4, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekday_age4$percent<-round(weekday_age4$percent, digits = 2)

#Plot together
weekday_age<-rbind(weekday_age1,weekday_age2,weekday_age3,weekday_age4)
weekday_age$age<-rep(c("0-5y","6-18y","19-59y","60y+"),each=56)
weekday_age$age<- factor(weekday_age$age,levels = c("0-5y","6-18y","19-59y","60y+"))
weekday_age$location<-weekday_age$Var2
ggplot(weekday_age, aes(x = factor(Var1), y = percent, fill = location)) +
  geom_bar(stat = "identity", width = 0.91) + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(.~age) +
  xlab("time")


####3.2. age_cat + weekend
###3.2.1. age1
weekend_age1<-TUD3[TUD3$age_cat=="0-5y" & TUD3$week=="Weekend",]

weekend_age1<-data.frame(table(weekend_age1$Time,weekend_age1$Location))
weekend_age1$Var1<-as.factor(weekend_age1$Var1)
weekend_age1$Var1 = factor(weekend_age1$Var1, levels = c(levels(weekend_age1$Var1)))
weekend_age1 = ddply(weekend_age1, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekend_age1$percent<-round(weekend_age1$percent, digits = 2)

###3.2.2. age2
weekend_age2<-TUD3[TUD3$age_cat=="6-18y" & TUD3$week=="Weekend",]

weekend_age2<-data.frame(table(weekend_age2$Time,weekend_age2$Location))
weekend_age2$Var1<-as.factor(weekend_age2$Var1)
weekend_age2$Var1 = factor(weekend_age2$Var1, levels = c(levels(weekend_age2$Var1)))
weekend_age2 = ddply(weekend_age2, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekend_age2$percent<-round(weekend_age2$percent, digits = 2)


###3.2.3. age3
weekend_age3<-TUD3[TUD3$age_cat=="19-59y" & TUD3$week=="Weekend",]

weekend_age3<-data.frame(table(weekend_age3$Time,weekend_age3$Location))
weekend_age3$Var1<-as.factor(weekend_age3$Var1)
weekend_age3$Var1 = factor(weekend_age3$Var1, levels = c(levels(weekend_age3$Var1)))
weekend_age3 = ddply(weekend_age3, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekend_age3$percent<-round(weekend_age3$percent, digits = 2)

###3.2.4. age4
weekend_age4<-TUD3[TUD3$age_cat=="60y+" & TUD3$week=="Weekend",]

weekend_age4<-data.frame(table(weekend_age4$Time,weekend_age4$Location))
weekend_age4$Var1<-as.factor(weekend_age4$Var1)
weekend_age4$Var1 = factor(weekend_age4$Var1, levels = c(levels(weekend_age4$Var1)))
weekend_age4 = ddply(weekend_age4, .(Var1), transform, percent = Freq/sum(Freq) * 100)
weekend_age4$percent<-round(weekend_age4$percent, digits = 2)

#Plot together
weekend_age<-rbind(weekend_age1,weekend_age2,weekend_age3,weekend_age4)
weekend_age$age<-rep(c("0-5y","6-18y","19-59y","60y+"),each=56)
weekend_age$age<- factor(weekend_age$age,levels = c("0-5y","6-18y","19-59y","60y+"))
weekend_age$location<-weekend_age$Var2
ggplot(weekend_age, aes(x = factor(Var1), y = percent, fill = location)) +
  geom_bar(stat = "identity", width = 0.91) + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(.~age) +
  xlab("time")



library(gridExtra)
library(grid)


weekday_age$location <- factor(weekday_age$location, levels=rev(levels(weekday_age$location)))                             
p1<-ggplot(weekday_age, aes(x = factor(Var1), y = percent, fill = location)) +
  geom_bar(stat = "identity", width = 0.91) + 
  theme_bw()  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(.~age) +
  ylab("Weekdays") +
  scale_fill_manual(values = c("#D5301C","#FC8D55","#FCCC8C", "#FFF0D9"))  + guides(fill=FALSE) +
  theme(axis.title=element_text(size =13,face="bold"),axis.title.x = element_blank())+
  # to change ordering of legend, set reverse = TRUE
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))+ 
  theme(legend.position="none")+
  theme(axis.title=element_text(size =15,face="bold"),axis.title.x = element_blank())+
  theme(strip.text.x=element_text(size=19),strip.text.y = element_text(size = 17,angle=0),strip.background = element_rect(fill = "white",colour = "NA")) +
  theme(panel.border = element_blank())
  
weekend_age$location <- factor(weekend_age$location, levels=rev(levels(weekend_age$location)))                             
p2<-ggplot(weekend_age, aes(x = factor(Var1), y = percent, fill = location)) +
  geom_bar(stat = "identity", width = 0.91) +
  theme_bw()  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(.~age) +
  ylab("Weekends/holiday") +
  scale_fill_manual("",values =  c("#D5301C","#FC8D55","#FCCC8C", "#FFF0D9")) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))+
  theme(legend.position="none", legend.direction="horizontal") +
theme(axis.title=element_text(size =15,face="bold"),axis.title.x = element_blank())+
  theme(strip.text.x=element_text(size=0),strip.text.y = element_text(size = 15,angle=0),strip.background = element_rect(fill = "white",colour = "NA")) +
  theme(panel.border = element_blank())+
  scale_y_continuous(labels = scales::comma)


grid.arrange(p1, p2, ncol=1)

