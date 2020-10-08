
#############################################################################
# SOCIAL CONTACT DATA ANALYSIS
#
# Copyright 2019, SIMID
#############################################################################
#
# MODELLING NUMBER OF CONTACT USING TIME USE
#
#############################################################################

rm(list=ls(all=TRUE))
setwd("C:\\Users\\lucp9032\\Desktop\\TIME USE FOLDER\\CODE_MI_BOOSTRAP\\THANG")

devtools::install_github("lwillem/simid_rtools",force=F,quiet=T)
#devtools::uninstall(simid.rtools)
library('simid.rtools')

packages_list <- c('car',  
                   'VIM',   
                   'devtools',
                   'sqldf',
                   "gamlss")   

# load packages (and install if not present)
smd_load_packages(packages_list)  

# input/output folder
data_folder    <- smd_file_path('data','survey_flanders2010')
MI_data_folder <- smd_file_path('output','imputation')


# load data
part_data<-read.table(file.path(data_folder,"individualsurvey_participants_full.txt"),header=T,sep=",")
contacts<-read.table(file.path(data_folder,"individualsurvey_contacts.txt"),header=T,sep=",")
part_data<-part_data[,c("local_id","class_size")]
load(file=file.path(MI_data_folder,"MI_data_weight"))
part_imp<-MI_data_weight[[1]]

part<-merge(part_imp,part_data,by="local_id")

###Calculate time use
var<-c("local_id","participant_age","participant_gender","week","holiday", paste0("time_use_location_",1:17),"hh_size","class_size","cnt_count","diary_weight")

TUD<-part[var]


###Recode multiple variables in R
# select columns: time slot
num_time_slots             <- 17
colnames_time_use_location <- paste0("time_use_location_",1:num_time_slots)

# set time slot sizes
t1 <- colnames_time_use_location[2:13]  ### time slots of 1 hour
t2 <- colnames_time_use_location[14:16] ### time slots of 2 hours
t3 <- colnames_time_use_location[1:17]  ### time slots of 3 hours

TUD[,colnames_time_use_location] <- lapply(TUD[,colnames_time_use_location], function(x) 
  recode(x,"1='home';2='kinder-garten';3='school';
4='workplace';5='transport';6='family';7='leisure';8='other'"))

# Set as character
for(i in colnames_time_use_location){
  TUD[,i] <- as.character(TUD[,i])
}

TUD$version<-NULL
TUD$version[TUD$participant_age<12]<-"Version 1"
TUD$version[TUD$participant_age>=12&TUD$participant_age<=60]<-"Version 2"
TUD$version[TUD$participant_age>60]<-"Version 3"


#####Calculate total duration spent on each location####

get_time_at_location <- function(TUD_location,location_type){
  
  TUD_location[,colnames_time_use_location][TUD_location[,colnames_time_use_location]=="nonfilled"] <- NA
  TUD_location[,colnames_time_use_location][TUD_location[,colnames_time_use_location] != location_type] <- 0
  TUD_location[,t1][TUD_location[,t1]==location_type] <- 1
  TUD_location[,t2][TUD_location[,t2]==location_type] <- 2
  TUD_location[,t3][TUD_location[,t3]==location_type] <- 3
  
  for (i in colnames_time_use_location){
    TUD_location[,i]<-as.numeric(TUD_location[,i])
  }
  
  a<-rowSums(TUD_location[,colnames_time_use_location])
  
  return(a)
}

TUD$home<-get_time_at_location(TUD,"home")
TUD$kinder_garten <- get_time_at_location(TUD,"kinder-garten")
TUD$school<- get_time_at_location(TUD,"school")
TUD$workplace <- get_time_at_location(TUD,"workplace")
TUD$transport <- get_time_at_location(TUD,"transport")
TUD$family <- get_time_at_location(TUD,"family")
TUD$leisure <- get_time_at_location(TUD,"leisure")
TUD$other <- get_time_at_location(TUD,"other")

TUD$school<-TUD$school +TUD$kinder_garten
TUD$other<-TUD$other + TUD$family + TUD$leisure + TUD$transport
#check
table(TUD$home +TUD$school +TUD$workplace +TUD$other)

TUD$kinder_garten<-NULL
TUD$family <-NULL
TUD$leisure<-NULL
TUD$transport<-NULL

#contacts$cnt_home<-as.character(contacts$cnt_home)
combine <- sqldf(" select *
              from TUD
                     left join (select local_id, count(*) count_home
                     from contacts
                     where cnt_home= 1
                     group by local_id)
                     using (local_id)")
combine$count_home[is.na(combine$count_home)]<- 0


#contacts$cnt_work<-as.character(contacts$cnt_work)
combine <- sqldf(" select *
              from combine
                     left join (select local_id, count(*) count_work
                     from contacts
                     where cnt_work= 1
                     group by local_id)
                     using (local_id)")

combine$count_work[is.na(combine$count_work)]<- 0


#contacts$cnt_school<-as.character(contacts$cnt_school)
combine <- sqldf(" select *
                 from combine
                 left join (select local_id, count(*) count_school
                 from contacts
                 where cnt_school= 1
                 group by local_id)
                 using (local_id)")

combine$count_school[is.na(combine$count_school)]<- 0

#contacts$cnt_transport<-as.character(contacts$cnt_transport)
combine <- sqldf(" select *
                 from combine
                 left join (select local_id, count(*) count_transport
                 from contacts
                 where cnt_transport= 1
                 group by local_id)
                 using (local_id)")

combine$count_transport[is.na(combine$count_transport)]<- 0


#contacts$cnt_leisure<-as.character(contacts$cnt_leisure)

combine <- sqldf(" select *
                 from combine
                 left join (select local_id, count(*) count_leisure
                 from contacts
                 where cnt_leisure= 1
                 group by local_id)
                 using (local_id)")

combine$count_leisure[is.na(combine$count_leisure)]<- 0

#contacts$cnt_other<-as.character(contacts$cnt_other)
combine <- sqldf(" select *
                 from combine
                 left join (select local_id, count(*) count_other
                 from contacts
                 where cnt_otherplace= 1
                 group by local_id)
                 using (local_id)")

combine$count_other[is.na(combine$count_other)]<- 0


combine <- sqldf(" select *
                 from combine
                 left join (select local_id, count(*) count_kinder_garten
                 from contacts
                 where cnt_kindergarden= 1
                 group by local_id)
                 using (local_id)")

combine$count_kinder_garten[is.na(combine$count_kinder_garten)]<- 0

combine <- sqldf(" select *
                 from combine
                 left join (select local_id, count(*) count_family
                 from contacts
                 where cnt_otherplace_family= 1
                 group by local_id)
                 using (local_id)")

combine$count_family[is.na(combine$count_family)]<- 0

###combine counts at kindergarten + school#####
combine$count_school<-combine$count_school +combine$count_kinder_garten
combine$count_other<-combine$count_other + combine$count_family + combine$count_leisure +combine$count_transport

combine$count_kinder_garten<-NULL
combine$count_family<-NULL
combine$count_leisure<-NULL
combine$count_transport<-NULL

####%%%%%%%%%%%%%%%%%%%%%%%MODELLING THE NUMBER OF CONTACTS%%%%%%%%%%%%%%%%%%%%%%%%%%
###variables included in the model###
#1.gender
#2. age group
#3. time use
#4. household sizeof
#5. class size
#6. temporal factor

##Predictor variables;
#1. Overall
#2. Home
#3. School
#4. Work
#5. transport
#6. Leisure
#7. Others

combine$age_gr<-cut(combine$participant_age,breaks=c(0,3,6,12,18,25,45,65,90),include.lowest = TRUE,right = FALSE)
combine$temporal<-as.factor(combine$week):as.factor(combine$holiday)

var<-c("home", "school","workplace","other", "count_home","count_work","count_school",      
"count_other","age_gr","temporal", "participant_gender", "hh_size", "class_size","cnt_count","diary_weight") 

combine1<-combine[,var]

###pb: the current version of P-splines which uses SVD in the fitting and therefore is the most reliable
###ps:  the original P-splines with no facility of estimating the smoothing parameters

####model for overall number of contacts##

overall<-combine1[,c("home", "school","workplace","other",
                     "age_gr","temporal", "participant_gender", "hh_size","cnt_count","diary_weight")]


model_overal<-gamlss(cnt_count~ home+ school+ workplace+ other +participant_gender + age_gr +  hh_size + temporal,family=ZINBI,method=RS(2000),data=overall,weights=diary_weight)

overal_coef<-summary(model_overal, save=TRUE)

write.table(overal_coef$mu.coef.table,file="clipboard",sep='\t')

####model for home contacts###
home<-combine1[,c("home", "school","workplace","other",
                          "age_gr","temporal", "participant_gender", "hh_size","count_home","diary_weight")]

model_home<-gamlss(count_home~  home+ school+ workplace+ other+ participant_gender + age_gr  + hh_size + temporal ,family=ZINBI,method=RS(2000),data=home,weights=diary_weight)

home_coef<-summary(model_home, save=TRUE)

write.table(home_coef$mu.coef.table,file="clipboard",sep='\t')


####model for school contacts###
school<-combine1[,c("home", "school", "workplace","other",
                  "age_gr","temporal", "participant_gender", "class_size","hh_size","count_school","diary_weight")]

school<-school[!is.na(school$class_size),]

model_school<-gamlss(count_school~ home+ school + participant_gender + age_gr + hh_size + class_size +  temporal,family=ZINBI,method=RS(2000),data=school,weights=diary_weight)

school_coef<-summary(model_school, save=TRUE)

write.table(school_coef$mu.coef.table,file="clipboard",sep='\t')

####model for work contacts###
work<-combine[,c("home", "school","workplace","other",
                  "age_gr","temporal", "participant_gender", "hh_size","count_work","diary_weight","participant_age")]

work<-work[work$participant_age>25 & work$participant_age<66,]

model_work<-gamlss(count_work~ home+  workplace +participant_gender + hh_size + temporal,family=ZINBI,method=RS(2000),data=work,weights=diary_weight)

work_coef<-summary(model_work, save=TRUE)

write.table(work_coef$mu.coef.table,file="clipboard",sep='\t')


####model for other contacts###
other<-combine[,c("home", "school","workplace","other",
                  "age_gr","temporal", "participant_gender", "hh_size","count_other","diary_weight")]


model_other<-gamlss(count_other~ home+ school+ workplace+ other +participant_gender + age_gr +  hh_size + temporal,family=ZINBI,method=RS(2000),data=other,weights=diary_weight)
other_coef<-summary(model_other, save=TRUE)
write.table(other_coef$coef.table,file="clipboard",sep='\t')


########################%%%%%%%%%%ASSOCIATION BETWEEN TIME USE AND NUMBER OF CONTACTS%%%%%%%%%%%%%%%%%%%%%%%%################
####Notes: Pearson uses parametric method which entails normal assumptions on x and y###
######Spearman can not compute P_value with many ties####
######--> Use KENDALL rank-based correlation coefficient#####
##http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r##

cor.test(combine$count_home, combine$home, method=c("kendall"))
cor.test(combine$count_work, combine$workplace, method=c("kendall"))
cor.test(combine$count_school, combine$school, method=c("kendall"))
cor.test(combine$count_transport, combine$transport, method=c("kendall"))
cor.test(combine$count_leisure, combine$leisure, method=c("kendall"))
cor.test(combine$count_other, combine$other, method=c("kendall"))


##########################################
##########################################
##########################################
####Plot FITTED AND CI from gamlss with distribution ZINBI
###pb: the current version of P-splines which uses SVD in the fitting and therefore is the most reliable
###ps:  the original P-splines with no facility of estimating the smoothing parameters
combine1$class_size<-NULL

windows(width=18, height=14)
par(mfrow=c(2,2),mar = c(5, 5, 2, 2))
#par(mfrow=c(2,3))

####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#######
####%%%%%%%%%%MODEL WITH ONLY TIME USE%%%%%%%%%%%%%%%############
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#######

data<-combine1
count_location<-"count_home"
tu_location<-"home"
model_family<-"ZINBI"
main_tag<-"a)"
xlab_tag<-"contacts"
ylab_tag<-"time"

plot_func<-function(data,count_location,tu_location,main_tag,model_family,xlab_tag,ylab_tag){
count<-get(count_location,data)
tu<-get(tu_location,data)
model_location<-  gamlss(count~ pb(tu),family=model_family,method=RS(2000),data=data,weights = diary_weight)
pred<-predict(model_location,what="mu",type="response",se.fit=T,data=combine1)
#pred$se.fit

fitted<-as.data.frame(fitted(model_location))
names(fitted)<-"fitted"
fitted$se<-pred$se.fit
fitted$lower<-fitted$fitted -qnorm(0.975)*fitted$se
fitted$upper<-fitted$fitted + qnorm(0.975)*fitted$se
data_location<-cbind(data[c(count_location,tu_location)],fitted)
data_location_sort<-data_location[order(tu),]
data_location_sort<-within(data_location_sort,{
  tu<-as.factor(tu)
  levels(tu)[levels(tu)=="0"]<-"<1"
})

tu<-as.factor(tu)
levels(tu)[levels(tu)=="0"]<-"<1"

# plot(data_new$home,data_new$count_home , main="a)", 
# xlab="Time spent at home ", ylab="Contacts at home", pch=19,cex.lab=1.4,cex.axis=1.1,cex.main=2)
boxplot(count~tu,data = data_location_sort, main=main_tag, 
        xlab=xlab_tag, ylab=ylab_tag,cex.lab=1.4,cex.axis=1.1,cex.main=2)
points(aggregate(count~tu,data_location_sort,mean)[2],col="black",pch=19)
lines(1:length(levels(tu)),unique(round(data_location_sort$fitted,3)),type="l",col="red",pch=15,lwd=2)
lines(1:length(levels(tu)),unique(round(data_location_sort$lower,3)),type="l", lty=5,lwd=2,col="green")
lines(1:length(levels(tu)),unique(round(data_location_sort$upper,3)),type="l", lty=5,lwd=2,col="green")
}



windows(width=18, height=14)
par(mfrow=c(2,2),mar = c(5, 5, 2, 2))
plot_func(combine1,"count_home","home","a)","NBI","Time spent at home","Contacts at home")
combine7<-subset(combine,participant_age<25)
combine7<-subset(combine7,school<10)
combine7<-combine7[c("count_school","school","diary_weight")]
plot_func(combine7,"count_school","school","b)","ZINBI","Time spent at school","Contacts at school")

combine6<-subset(combine,participant_age>24 & participant_age<66)
combine6<-subset(combine6,workplace<12)
combine6<-subset(combine6,count_work<100)
combine6<-combine6[c("count_work","workplace","diary_weight")]

plot_func(combine6,"count_work","workplace","c)","ZINBI","Time spent at work","Contacts at work")
plot_func(combine1,"count_other","other","d)","NBI","Time spent at other places","Contacts at other places")

####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#######
####%%%%%%%%%%MODEL WITH TIME USE AND TEMPORAL FACTOR%%%%%%%%%%%%%%%############
####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#######
par(mfrow=c(2,2),mar = c(5, 5, 2, 2))

####home####
combine5<-combine1
model_home<-gamlss(count_home~ pb(home) + temporal, sigma.formula =~ temporal,family=ZINBI,method=RS(2000),data=combine5,weights = diary_weight)

pred_home<-as.data.frame(predict(model_home,what="mu",type="response",se.fit=T,data=combine5))
pred_home$lower<-pred_home$fit -qnorm(0.975)*pred_home$se.fit
pred_home$upper<-pred_home$fit + qnorm(0.975)*pred_home$se.fit
pred_home$home<-combine5$home
pred_home$temporal<-combine5$temporal
pred_home$count_home<-combine5$count_home

pred_home<-pred_home[order(pred_home$home),]
pred_tempo1_home<-pred_home[pred_home$temporal==levels(pred_home$temporal)[1],]
pred_tempo2_home<-pred_home[pred_home$temporal==levels(pred_home$temporal)[2],]
pred_tempo3_home<-pred_home[pred_home$temporal==levels(pred_home$temporal)[3],]
pred_tempo4_home<-pred_home[pred_home$temporal==levels(pred_home$temporal)[4],]


plot(pred_tempo1_home$home,pred_tempo1_home$count_home , main="Weekday:Regular", 
     xlab="Time spent at home ", ylab="Number of contacts at home", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo1_home$home,pred_tempo1_home$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo1_home$home,pred_tempo1_home$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo1_home$home,pred_tempo1_home$upper,type="l", lty=5,lwd=2,col="green")

plot(pred_tempo2_home$home,pred_tempo2_home$count_home , main="Weekday:Holiday", 
     xlab="Time spent at home ", ylab="Number of contacts at home", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo2_home$home,pred_tempo2_home$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo2_home$home,pred_tempo2_home$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo2_home$home,pred_tempo2_home$upper,type="l", lty=5,lwd=2,col="green")

plot(pred_tempo3_home$home,pred_tempo3_home$count_home , main="Weekend:Regular", 
     xlab="Time spent at home ", ylab="Number of contacts at home", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo3_home$home,pred_tempo3_home$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo3_home$home,pred_tempo3_home$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo3_home$home,pred_tempo3_home$upper,type="l", lty=5,lwd=2,col="green")

plot(pred_tempo4_home$home,pred_tempo4_home$count_home , main="Weekend:Holiday", 
     xlab="Time spent at home ", ylab="Number of contacts at home", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo4_home$home,pred_tempo4_home$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo4_home$home,pred_tempo4_home$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo4_home$home,pred_tempo4_home$upper,type="l", lty=5,lwd=2,col="green")


####school####
combine5<-combine1[combine1$school<12,]
model_school<-gamlss(count_school~ pb(school)+temporal,sigma.formula =~temporal,family=ZINBI,method=RS(2000),data=combine5,weights=diary_weight)

pred_school<-as.data.frame(predict(model_school,what="mu",type="response",se.fit=T,data=combine5))
pred_school$lower<-pred_school$fit -qnorm(0.975)*pred_school$se.fit
pred_school$upper<-pred_school$fit + qnorm(0.975)*pred_school$se.fit
pred_school$school<-combine5$school
pred_school$temporal<-combine5$temporal
pred_school$count_school<-combine5$count_school

pred_school<-pred_school[order(pred_school$school),]
pred_tempo1_school<-pred_school[pred_school$temporal==levels(pred_school$temporal)[1],]
pred_tempo2_school<-pred_school[pred_school$temporal==levels(pred_school$temporal)[2],]
pred_tempo3_school<-pred_school[pred_school$temporal==levels(pred_school$temporal)[3],]
pred_tempo4_school<-pred_school[pred_school$temporal==levels(pred_school$temporal)[4],]


plot(pred_tempo1_school$school,pred_tempo1_school$count_school , main="Weekday:Regular", 
     xlab="Time spent at school ", ylab="Number of contacts at school", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo1_school$school,pred_tempo1_school$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo1_school$school,pred_tempo1_school$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo1_school$school,pred_tempo1_school$upper,type="l", lty=5,lwd=2,col="green")

plot(pred_tempo2_school$school,pred_tempo2_school$count_school , main="Weekday:Holiday", 
     xlab="Time spent at school ", ylab="Number of contacts at school", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo2_school$school,pred_tempo2_school$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo2_school$school,pred_tempo2_school$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo2_school$school,pred_tempo2_school$upper,type="l", lty=5,lwd=2,col="green")

plot(pred_tempo3_school$school,pred_tempo3_school$count_school , main="Weekend:Regular", 
     xlab="Time spent at school ", ylab="Number of contacts at school", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo3_school$school,pred_tempo3_school$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo3_school$school,pred_tempo3_school$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo3_school$school,pred_tempo3_school$upper,type="l", lty=5,lwd=2,col="green")

plot(pred_tempo4_school$school,pred_tempo4_school$count_school , main="Weekend:Holiday", 
     xlab="Time spent at school ", ylab="Number of contacts at school", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo4_school$school,pred_tempo4_school$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo4_school$school,pred_tempo4_school$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo4_school$school,pred_tempo4_school$upper,type="l", lty=5,lwd=2,col="green")


#####Work####
combine5<-combine[combine$participant_age>12&combine$participant_age<65,]
combine5$class_size<-NULL
model_work<-gamlss(count_work~ pb(workplace)+temporal,sigma.formula =~temporal,family=ZINBI,method=RS(2000),data=combine5,weights=diary_weight)

pred_work<-as.data.frame(predict(model_work,what="mu",type="response",se.fit=T,data=combine5))
pred_work$lower<-pred_work$fit -qnorm(0.975)*pred_work$se.fit
pred_work$upper<-pred_work$fit + qnorm(0.975)*pred_work$se.fit
pred_work$work<-combine5$work
pred_work$temporal<-combine5$temporal
pred_work$count_work<-combine5$count_work

pred_work<-pred_work[order(pred_work$work),]
pred_tempo1_work<-pred_work[pred_work$temporal==levels(pred_work$temporal)[1],]
pred_tempo2_work<-pred_work[pred_work$temporal==levels(pred_work$temporal)[2],]
pred_tempo3_work<-pred_work[pred_work$temporal==levels(pred_work$temporal)[3],]
pred_tempo4_work<-pred_work[pred_work$temporal==levels(pred_work$temporal)[4],]


plot(pred_tempo1_work$work,pred_tempo1_work$count_work , main="Weekday:Regular", 
     xlab="Time spent at work ", ylab="Number of contacts at work", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo1_work$work,pred_tempo1_work$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo1_work$work,pred_tempo1_work$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo1_work$work,pred_tempo1_work$upper,type="l", lty=5,lwd=2,col="green")

plot(pred_tempo2_work$work,pred_tempo2_work$count_work ,  main="Weekday:Holiday", 
     xlab="Time spent at work ", ylab="Number of contacts at work", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo2_work$work,pred_tempo2_work$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo2_work$work,pred_tempo2_work$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo2_work$work,pred_tempo2_work$upper,type="l", lty=5,lwd=2,col="green")

plot(pred_tempo3_work$work,pred_tempo3_work$count_work ,  main="Weekend:Regular",
     xlab="Time spent at work ", ylab="Number of contacts at work", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo3_work$work,pred_tempo3_work$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo3_work$work,pred_tempo3_work$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo3_work$work,pred_tempo3_work$upper,type="l", lty=5,lwd=2,col="green")

plot(pred_tempo4_work$work,pred_tempo4_work$count_work , main="Weekend:Holiday", 
     xlab="Time spent at work ", ylab="Number of contacts at work", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo4_work$work,pred_tempo4_work$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo4_work$work,pred_tempo4_work$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo4_work$work,pred_tempo4_work$upper,type="l", lty=5,lwd=2,col="green")


#####Transport####
combine5<-combine1
model_transport<-gamlss(count_transport~ pb(transport)+temporal,sigma.formula =~temporal,family=ZINBI,method=RS(2000),data=combine5,weights=diary_weight)

pred_transport<-as.data.frame(predict(model_transport,what="mu",type="response",se.fit=T,data=combine5))
pred_transport$lower<-pred_transport$fit -qnorm(0.975)*pred_transport$se.fit
pred_transport$upper<-pred_transport$fit + qnorm(0.975)*pred_transport$se.fit
pred_transport$transport<-combine5$transport
pred_transport$temporal<-combine5$temporal
pred_transport$count_transport<-combine5$count_transport

pred_transport<-pred_transport[order(pred_transport$transport),]
pred_tempo1_transport<-pred_transport[pred_transport$temporal==levels(pred_transport$temporal)[1],]
pred_tempo2_transport<-pred_transport[pred_transport$temporal==levels(pred_transport$temporal)[2],]
pred_tempo3_transport<-pred_transport[pred_transport$temporal==levels(pred_transport$temporal)[3],]
pred_tempo4_transport<-pred_transport[pred_transport$temporal==levels(pred_transport$temporal)[4],]


plot(pred_tempo1_transport$transport,pred_tempo1_transport$count_transport , main="Weekday:Regular", 
     xlab="Time spent at transport ", ylab="Number of contacts at transport", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo1_transport$transport,pred_tempo1_transport$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo1_transport$transport,pred_tempo1_transport$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo1_transport$transport,pred_tempo1_transport$upper,type="l", lty=5,lwd=2,col="green")

plot(pred_tempo2_transport$transport,pred_tempo2_transport$count_transport ,  main="Weekday:Holiday", 
     xlab="Time spent at transport ", ylab="Number of contacts at transport", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo2_transport$transport,pred_tempo2_transport$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo2_transport$transport,pred_tempo2_transport$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo2_transport$transport,pred_tempo2_transport$upper,type="l", lty=5,lwd=2,col="green")

plot(pred_tempo3_transport$transport,pred_tempo3_transport$count_transport ,  main="Weekend:Regular",
     xlab="Time spent at transport ", ylab="Number of contacts at transport", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo3_transport$transport,pred_tempo3_transport$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo3_transport$transport,pred_tempo3_transport$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo3_transport$transport,pred_tempo3_transport$upper,type="l", lty=5,lwd=2,col="green")

plot(pred_tempo4_transport$transport,pred_tempo4_transport$count_transport , main="Weekend:Holiday", 
     xlab="Time spent at transport ", ylab="Number of contacts at transport", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo4_transport$transport,pred_tempo4_transport$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo4_transport$transport,pred_tempo4_transport$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo4_transport$transport,pred_tempo4_transport$upper,type="l", lty=5,lwd=2,col="green")


#####Leisure####
combine5<-combine1[combine1$leisure<11,]
model_leisure<-gamlss(count_leisure~ pb(leisure)+temporal,family=ZINBI,method=RS(2000),data=combine5,weights=diary_weight)

pred_leisure<-as.data.frame(predict(model_leisure,what="mu",type="response",se.fit=T,data=combine5))
pred_leisure$lower<-pred_leisure$fit -qnorm(0.975)*pred_leisure$se.fit
pred_leisure$upper<-pred_leisure$fit + qnorm(0.975)*pred_leisure$se.fit
pred_leisure$leisure<-combine5$leisure
pred_leisure$temporal<-combine5$temporal
pred_leisure$count_leisure<-combine5$count_leisure

pred_leisure<-pred_leisure[order(pred_leisure$leisure),]
pred_tempo1_leisure<-pred_leisure[pred_leisure$temporal==levels(pred_leisure$temporal)[1],]
pred_tempo2_leisure<-pred_leisure[pred_leisure$temporal==levels(pred_leisure$temporal)[2],]
pred_tempo3_leisure<-pred_leisure[pred_leisure$temporal==levels(pred_leisure$temporal)[3],]
pred_tempo4_leisure<-pred_leisure[pred_leisure$temporal==levels(pred_leisure$temporal)[4],]


plot(pred_tempo1_leisure$leisure,pred_tempo1_leisure$count_leisure , main="Weekday:Regular", 
     xlab="Time spent at leisure ", ylab="Number of contacts at leisure", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo1_leisure$leisure,pred_tempo1_leisure$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo1_leisure$leisure,pred_tempo1_leisure$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo1_leisure$leisure,pred_tempo1_leisure$upper,type="l", lty=5,lwd=2,col="green")

plot(pred_tempo2_leisure$leisure,pred_tempo2_leisure$count_leisure ,  main="Weekday:Holiday", 
     xlab="Time spent at leisure ", ylab="Number of contacts at leisure", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo2_leisure$leisure,pred_tempo2_leisure$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo2_leisure$leisure,pred_tempo2_leisure$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo2_leisure$leisure,pred_tempo2_leisure$upper,type="l", lty=5,lwd=2,col="green")

plot(pred_tempo3_leisure$leisure,pred_tempo3_leisure$count_leisure ,  main="Weekend:Regular",
     xlab="Time spent at leisure ", ylab="Number of contacts at leisure", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo3_leisure$leisure,pred_tempo3_leisure$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo3_leisure$leisure,pred_tempo3_leisure$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo3_leisure$leisure,pred_tempo3_leisure$upper,type="l", lty=5,lwd=2,col="green")

plot(pred_tempo4_leisure$leisure,pred_tempo4_leisure$count_leisure , main="Weekend:Holiday", 
     xlab="Time spent at leisure ", ylab="Number of contacts at leisure", pch=19,cex.lab=1.8,cex.main=2)
lines(pred_tempo4_leisure$leisure,pred_tempo4_leisure$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo4_leisure$leisure,pred_tempo4_leisure$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo4_leisure$leisure,pred_tempo4_leisure$upper,type="l", lty=5,lwd=2,col="green")

#####Other####
combine5<-combine1
model_other<-gamlss(count_other~ pb(other)+temporal,sigma.formula =~temporal,family=ZINBI,method=RS(2000),data=combine5,weights=diary_weight)

pred_other<-as.data.frame(predict(model_other,what="mu",type="response",se.fit=T,data=combine5))
pred_other$lower<-pred_other$fit -qnorm(0.975)*pred_other$se.fit
pred_other$upper<-pred_other$fit + qnorm(0.975)*pred_other$se.fit
pred_other$other<-combine5$other
pred_other$temporal<-combine5$temporal
pred_other$count_other<-combine5$count_other

pred_other<-pred_other[order(pred_other$other),]
pred_tempo1_other<-pred_other[pred_other$temporal==levels(pred_other$temporal)[1],]
pred_tempo2_other<-pred_other[pred_other$temporal==levels(pred_other$temporal)[2],]
pred_tempo3_other<-pred_other[pred_other$temporal==levels(pred_other$temporal)[3],]
pred_tempo4_other<-pred_other[pred_other$temporal==levels(pred_other$temporal)[4],]


plot(pred_tempo1_other$other,pred_tempo1_other$count_other , main="Weekday:Regular", 
     xlab="Time spent at other locations ", ylab="Number of contacts at other locations", pch=19,cex.lab=1.6,cex.main=2)
lines(pred_tempo1_other$other,pred_tempo1_other$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo1_other$other,pred_tempo1_other$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo1_other$other,pred_tempo1_other$upper,type="l", lty=5,lwd=2,col="green")

plot(pred_tempo2_other$other,pred_tempo2_other$count_other ,  main="Weekday:Holiday", 
     xlab="Time spent at other locations", ylab="Number of contacts at other locations", pch=19,cex.lab=1.6,cex.main=2)
lines(pred_tempo2_other$other,pred_tempo2_other$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo2_other$other,pred_tempo2_other$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo2_other$other,pred_tempo2_other$upper,type="l", lty=5,lwd=2,col="green")

plot(pred_tempo3_other$other,pred_tempo3_other$count_other ,  main="Weekend:Regular",
     xlab="Time spent at other locations", ylab="Number of contacts at other locations", pch=19,cex.lab=1.6,cex.main=2)
lines(pred_tempo3_other$other,pred_tempo3_other$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo3_other$other,pred_tempo3_other$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo3_other$other,pred_tempo3_other$upper,type="l", lty=5,lwd=2,col="green")

plot(pred_tempo4_other$other,pred_tempo4_other$count_other , main="Weekend:Holiday", 
     xlab="Time spent at other locations", ylab="Number of contacts at other locations", pch=19,cex.lab=1.6,cex.main=2)
lines(pred_tempo4_other$other,pred_tempo4_other$fit,type="l",col="red",pch=15,lwd=2)
lines(pred_tempo4_other$other,pred_tempo4_other$lower,type="l", lty=5,lwd=2,col="green")
lines(pred_tempo4_other$other,pred_tempo4_other$upper,type="l", lty=5,lwd=2,col="green")

