#############################################################################
# SOCIAL CONTACT DATA ANALYSIS
#
# Copyright 2019, SIMID
#############################################################################
#
# ANALYSE TIME USE DATA
#
#############################################################################

# clear global environment
rm(list = ls())

## set working directory (or open RStudio with this script)
#setwd("C:\\Users\\lucp9032\\Desktop\\TIME USE FOLDER\\pooled MI_bootstrap\\bootstrap MI")
#setwd("/vsc-hard-mounts/leuven-user/322/vsc32267/MI_Bootstrap")

require(devtools)
devtools::install_github("lwillem/simid_rtools",force=F,quiet=F)
library('simid.rtools')

smd_load_packages(c('car',   # 'combine' function
                    'tidyr',   # 'gather' function
                    'sqldf'))  # 'sql' join operation


########################################################
###  SETTINGS                                        ###
########################################################

# number of bootstraps
# TODO: just count files in bootstrap folder?
num_bootstraps <- 24

# random number engine seed
rng_seed <- 1235

# input/output folder
ref_data_folder        <- smd_file_path('data')
bootstrap_data_folder  <- smd_file_path('output','bootstrap')
output_folder          <- smd_file_path('output','time_use')

# age categories
ref_agecat <- c(0, 3, 6, 12,18,25,30,35,40,45,50,55,60,65,70,75,80,100)

#rm(list=ls(all=TRUE))
#install.packages(c("car","tidyr","sqldf"))
# library(mgcv)
# library(MASS)
# library(gamlss)
#library(car)
#library(dplyr)

 #library(tidyr)
# library(sqldf)

#setwd("/vsc-hard-mounts/leuven-user/322/vsc32267/MI_TUD_individual")
imputation_index <- 1
bootstrap_index <- 1
boot_TUD  <- function(imputation_index, bootstrap_index){
  #load(file="boot_MI1")      # orig
  #part <- boot_MI1[[boot]]   # orig
  #load(file=paste0("boot_MI",boot,".RData"))       # LW: temp
  #part <- BootData2

  BootData_imputation <- get(load(file=smd_file_path(bootstrap_data_folder,paste0("MI_data_bootstrap",bootstrap_index,".RData")))) 
  part                <- BootData_imputation[BootData_imputation$imputation_index == imputation_index,]
  
  # add new id varaible, to account for dupplicated participants by the bootstrap
  part$newid<-c(1:length(part[,1]))
  
  # select columns
  num_time_slots             <- 17
  colnames_time_use_location <- paste0("time_use_location_",1:num_time_slots)
  
  # time slot sizes
  t1 <- colnames_time_use_location[2:13]  ### time slots of 1 hour
  t2 <- colnames_time_use_location[14:16] ### time slots of 2 hours
  t3 <- colnames_time_use_location[1:17]  ### time slots of 3 hours

  # hh info columns  
  max_hhsize                 <- 12
  colnames_hh_gender         <- paste0("hh_member_gender_",1:max_hhsize)
  colnames_hh_age            <- paste0("hh_member_age_",1:max_hhsize)
  colnames_hh_present        <- paste0("hh_member_present_",1:max_hhsize)
  
  ####use newid, instead of local_id which is dupplicated in boostrap dataset#####
  var<-c("newid","participant_age","participant_gender","dayofweek","holiday", 
       colnames_time_use_location,"hh_size",
       colnames_hh_gender,
       colnames_hh_age,
       colnames_hh_present,
       'age_cat',
       'week',
       'version')

  # select TUD data from participant data  
  TUD<-part[,var]

  ### Recode multiple variables in R
  TUD[,colnames_time_use_location] <- lapply(TUD[,colnames_time_use_location], function(x) 
    recode(x,"1='home';2='kinder-garten';3='school';
           4='workplace';5='transport';6='family';7='leisure';8='other';9='nonfilled'"))
  
  # Set as character
  for(i in colnames_time_use_location){
    TUD[,i] <- as.character(TUD[,i])
  }
  
  # names(TUD)[names(TUD)=="time_use_location_1"]<-"05-08h"
  # names(TUD)[names(TUD)=="time_use_location_2"]<-"08-09h"
  # names(TUD)[names(TUD)=="time_use_location_3"]<-"09-10h"
  # names(TUD)[names(TUD)=="time_use_location_4"]<-"10-11h"
  # names(TUD)[names(TUD)=="time_use_location_5"]<-"11-12h"
  # names(TUD)[names(TUD)=="time_use_location_6"]<-"12-13h"
  # names(TUD)[names(TUD)=="time_use_location_7"]<-"13-14h"
  # names(TUD)[names(TUD)=="time_use_location_8"]<-"14-15h"
  # names(TUD)[names(TUD)=="time_use_location_9"]<-"15-16h"
  # names(TUD)[names(TUD)=="time_use_location_10"]<-"16-17h"
  # names(TUD)[names(TUD)=="time_use_location_11"]<-"17-18h"
  # names(TUD)[names(TUD)=="time_use_location_12"]<-"18-19h"
  # names(TUD)[names(TUD)=="time_use_location_13"]<-"19-20h"
  # names(TUD)[names(TUD)=="time_use_location_14"]<-"20-22h"
  # names(TUD)[names(TUD)=="time_use_location_15"]<-"22-24h"
  # names(TUD)[names(TUD)=="time_use_location_16"]<-"24-02h"
  # names(TUD)[names(TUD)=="time_use_location_17"]<-"02-05h"
  
  #TODO: remove extra columns?
  # add columns with time use slot (without remove the original columns)
  time_use_slots <- c("05-08h","08-09h","09-10h","10-11h","11-12h","12-13h","13-14h",
                      "14-15h","15-16h","16-17h","17-18h","18-19h","19-20h","20-22h",
                      "22-24h","24-02h","02-05h")
  TUD[,time_use_slots] <- TUD[,colnames_time_use_location]
  
  # replace 'week', after imputation procedure for 'dayofweek'
  # TODO: change in MICE procedure to impute 'week'
  TUD$week<-NULL
  TUD$week[TUD$dayofweek==0|TUD$dayofweek==6]<- "Weekend"
  TUD$week[TUD$dayofweek==1|TUD$dayofweek==2|TUD$dayofweek==3|TUD$dayofweek==4|TUD$dayofweek==5]<- "Weekday"
  # 
  # # 'version' is present in "part" data
  # TUD$version<-NULL
  # TUD$version[TUD$participant_age<12]<-"Version 1"
  # TUD$version[TUD$participant_age>=12&TUD$participant_age<=60]<-"Version 2"
  # TUD$version[TUD$participant_age>60]<-"Version 3"
  
  # # 'age_cat' is present in "part" data
  #TUD$age_cat <- cut(TUD$participant_age, breaks=c(0, 3, 6, 12,18,25,30,35,40,45,50,55,60,65,70,75,80,100),right = FALSE)
  TUD$factor  <- TUD$age_cat:factor(TUD$holiday):factor(TUD$week)
  
  
  #######################################################################
  ##############A. TIME-EXPOSURE MATRIX AT HOME#############################
  ##########################################################################
  
  ##Exclude participants from hhs member####
  ##2 Criteria for exclusion: the same age and the same gender###
  
  # LW: execute per column
  for(i_col in colnames_hh_gender){
    TUD[,i_col] <- as.character(TUD[,i_col])
  }
  # for (i in 1:length(TUD[,1])){
  #   for (j in 36:47){
  #     TUD[i,j]<-as.character(TUD[i,j])    
  #   }
  # }
  
  # LW: use column name instead of index
  TUD$participant_gender <- as.character(TUD$participant_gender)
  #TUD[,3]<-as.character(TUD[,3])
  
  # LW: run per participant... and use all hh data at once
  colnames_hh_age <- paste0("hh_member_age_",1:max_hhsize)
  # for (i in 1:length(TUD[,1])){
  #   for (j in colnames_hh_age){
  #     for (k in colnames_hh_age){
  #       
  #       if (!is.na(TUD[i,j]) & TUD[i,j]==TUD[i,2] & !is.na(TUD[i,k])& TUD[i,k]==TUD[i,3]) {TUD[i,j]<-NA}
  # }}}
  for (i_person in 1:nrow(TUD)){
    
    # check age and gender  
    bool_presence <- TUD$participant_gender[i_person] == TUD[i_person,colnames_hh_gender] &
                     TUD$participant_age[i_person]    == TUD[i_person,colnames_hh_age]
    
    # if a household member has the same age and gender
    if(any(bool_presence,na.rm = T)){
      # remove NA's
      bool_presence[is.na(bool_presence)] <- FALSE
      
      # select only 1
      bool_presence <- which(bool_presence)[1]
      
      # set person details to NA
      TUD[i_person,colnames_hh_age[bool_presence]]     <- NA
      TUD[i_person,colnames_hh_gender[bool_presence]]  <- NA
      
    }
  }
  
  
  ###Exclude hh members that are not present at home  ####
  TUD[,colnames_hh_age][TUD[,colnames_hh_present]=="N"] <- NA
  # TUD$hh_member_age_1[TUD$hh_member_present_1=="N"]<-NA
  # TUD$hh_member_age_2[TUD$hh_member_present_2=="N"]<-NA
  # TUD$hh_member_age_3[TUD$hh_member_present_3=="N"]<-NA
  # TUD$hh_member_age_4[TUD$hh_member_present_4=="N"]<-NA
  # TUD$hh_member_age_5[TUD$hh_member_present_5=="N"]<-NA
  # TUD$hh_member_age_6[TUD$hh_member_present_6=="N"]<-NA
  # TUD$hh_member_age_7[TUD$hh_member_present_7=="N"]<-NA
  # TUD$hh_member_age_8[TUD$hh_member_present_8=="N"]<-NA
  # TUD$hh_member_age_9[TUD$hh_member_present_9=="N"]<-NA
  # TUD$hh_member_age_10[TUD$hh_member_present_10=="N"]<-NA
  # TUD$hh_member_age_11[TUD$hh_member_present_11=="N"]<-NA
  # TUD$hh_member_age_12[TUD$hh_member_present_12=="N"]<-NA
  
  ##### Function to calculate TUD at a given location
  TUD_location <- TUD
  location_type <- 'kinder-garten'
  get_time_at_location <- function(TUD_location,location_type){
    
    TUD_location[,colnames_time_use_location][TUD_location[,colnames_time_use_location]=="nonfilled"] <- NA
    TUD_location[,colnames_time_use_location][TUD_location[,colnames_time_use_location] != location_type] <- 0
    TUD_location[,t1][TUD_location[,t1]==location_type] <- 1
    TUD_location[,t2][TUD_location[,t2]==location_type] <- 2
    TUD_location[,t3][TUD_location[,t3]==location_type] <- 3
    
    for (i in colnames_time_use_location){
      TUD_location[,i]<-as.numeric(TUD_location[,i])
    }
    
    return(TUD_location)
  }
  
  #####Calculate TUD_home
  TUD_home <- get_time_at_location(TUD,'home')
  
  ###Extract hh member to calculate time-exposure####
  extract_hh_member_data<-TUD[c("newid", 
                                colnames_time_use_location,
                                colnames_hh_age,
                                "holiday","week","age_cat")]
               
  # reformat and remove NA's 
  member <- gather(extract_hh_member_data, member, age, hh_member_age_1:hh_member_age_12, factor_key=TRUE)
  member <- member[!is.na(member$age),]

  member$m_age_cat <- cut(member$age, breaks=ref_agecat,right = FALSE)
  member$factor<-member$m_age_cat:factor(member$holiday):factor(member$week)
  
  ###exclude hh member in the same age group####
  member = member[!duplicated(member[c("newid","m_age_cat")]),]
  
  #####Calculate age-specific time-exposure at home and at time slot 2-5AM####
  member <- get_time_at_location(member,'home')
  
  colnames_time_use_location_x <- paste0('home_time_slot',1:num_time_slots)
  for(i in 1:nlevels(member$factor)){
    for(i_time in 1:length(colnames_time_use_location_x)){
       member[member$factor==levels(member$factor)[i],colnames_time_use_location_x[i_time]] <- mean(TUD_home[TUD_home$factor==levels(member$factor)[i],colnames_time_use_location[i_time]],na.rm = TRUE)
    }
  }

  # member$home_time_slot1<-NULL
  # member$home_time_slot2<-NULL
  # member$home_time_slot3<-NULL
  # member$home_time_slot4<-NULL
  # member$home_time_slot5<-NULL
  # member$home_time_slot6<-NULL
  # member$home_time_slot7<-NULL
  # member$home_time_slot8<-NULL
  # member$home_time_slot9<-NULL
  # member$home_time_slot10<-NULL
  # member$home_time_slot11<-NULL
  # member$home_time_slot12<-NULL
  # member$home_time_slot13<-NULL
  # member$home_time_slot14<-NULL
  # member$home_time_slot15<-NULL
  # member$home_time_slot16<-NULL
  # member$home_time_slot17<-NULL
  #for(i in 1:68){
   #member$home_time_slot1[member$factor==levels(member$factor)[i]] <- mean(TUD_home[TUD_home$factor==levels(member$factor)[i],colnames_time_use_location[1]],na.rm = TRUE)
  #   member$home_time_slot2[member$factor==levels(member$factor)[i]] <- mean(TUD_home$`08-09h`[TUD_home$factor==levels(member$factor)[i]],na.rm = TRUE)
  #   member$home_time_slot3[member$factor==levels(member$factor)[i]] <- mean(TUD_home$`09-10h`[TUD_home$factor==levels(member$factor)[i]],na.rm = TRUE)
  #   member$home_time_slot4[member$factor==levels(member$factor)[i]] <- mean(TUD_home$`10-11h`[TUD_home$factor==levels(member$factor)[i]],na.rm = TRUE)
  #   member$home_time_slot5[member$factor==levels(member$factor)[i]] <- mean(TUD_home$`11-12h`[TUD_home$factor==levels(member$factor)[i]],na.rm = TRUE)
  #   member$home_time_slot6[member$factor==levels(member$factor)[i]] <- mean(TUD_home$`12-13h`[TUD_home$factor==levels(member$factor)[i]],na.rm = TRUE)
  #   member$home_time_slot7[member$factor==levels(member$factor)[i]] <- mean(TUD_home$`13-14h`[TUD_home$factor==levels(member$factor)[i]],na.rm = TRUE)
  #   member$home_time_slot8[member$factor==levels(member$factor)[i]] <- mean(TUD_home$`14-15h`[TUD_home$factor==levels(member$factor)[i]],na.rm = TRUE)
  #   member$home_time_slot9[member$factor==levels(member$factor)[i]] <- mean(TUD_home$`15-16h`[TUD_home$factor==levels(member$factor)[i]],na.rm = TRUE)
  #   member$home_time_slot10[member$factor==levels(member$factor)[i]] <- mean(TUD_home$`16-17h`[TUD_home$factor==levels(member$factor)[i]],na.rm = TRUE)
  #   member$home_time_slot11[member$factor==levels(member$factor)[i]] <- mean(TUD_home$`17-18h`[TUD_home$factor==levels(member$factor)[i]],na.rm = TRUE)
  #   member$home_time_slot12[member$factor==levels(member$factor)[i]] <- mean(TUD_home$`18-19h`[TUD_home$factor==levels(member$factor)[i]],na.rm = TRUE)
  #   member$home_time_slot13[member$factor==levels(member$factor)[i]] <- mean(TUD_home$`19-20h`[TUD_home$factor==levels(member$factor)[i]],na.rm = TRUE)
  #   member$home_time_slot14[member$factor==levels(member$factor)[i]] <- mean(TUD_home$`20-22h`[TUD_home$factor==levels(member$factor)[i]],na.rm = TRUE)
  #   member$home_time_slot15[member$factor==levels(member$factor)[i]] <- mean(TUD_home$`22-24h`[TUD_home$factor==levels(member$factor)[i]],na.rm = TRUE)
  #   member$home_time_slot16[member$factor==levels(member$factor)[i]] <- mean(TUD_home$`24-02h`[TUD_home$factor==levels(member$factor)[i]],na.rm = TRUE)
  #   member$home_time_slot17[member$factor==levels(member$factor)[i]] <- mean(TUD_home$`02-05h`[TUD_home$factor==levels(member$factor)[i]],na.rm = TRUE)
  # }
  
  ####compare time at home of participants and time_at home of member
  ####if time-at-home of participants >time-at-home of member --> take time-at-home of member
  ####if time-at-home of participants <=time-at-home of member --> take time-at-home of paricipants
  
  fun_home<-function(x,y,z){
    for (i in 1: length(z[,1])){
      if (!is.na(y[i]) & (y[i]<= x[i]) & !is.na(x[i])){x[i]=y[i]}
      if (!is.na(y[i]) & (y[i]>= x[i]) & !is.na(x[i])){x[i]=x[i]}
      if (is.na(y[i])) {x[i]<-NA}
    }  
    return(x)}
  
  func_na<-function(x){
    # for (i in 41:57){
    #   x[,i][is.na(x[,i])]<-0
    # } 
    x[is.na(x)] <- 0
    return(x)
  }
  
  #TODO: option to avoid SQL?
  func_time<-function(x){
    y<-sqldf("select newid, participant_age, age_cat, `[0,3)`,`[3,6)`,`[6,12)`,`[12,18)`,`[18,25)`,`[25,30)`,`[30,35)`,`[35,40)`,`[40,45)`,`[45,50)`,`[50,55)`,`[55,60)`,`[60,65)`,
             `[65,70)`,`[70,75)`,`[75,80)`,`[80,100)`
             from TUD
             left join (select newid, sum(`[0,3)`) `[0,3)`, sum(`[3,6)`) `[3,6)`,sum(`[6,12)`) `[6,12)`,sum(`[12,18)`) `[12,18)`,sum(`[18,25)`) `[18,25)`,sum(`[25,30)`) `[25,30)`,sum(`[30,35)`) `[30,35)`,
             sum(`[35,40)`) `[35,40)`,sum(`[40,45)`) `[40,45)`,sum(`[45,50)`) `[45,50)`,sum(`[50,55)`) `[50,55)`,sum(`[55,60)`) `[55,60)`,sum(`[60,65)`) `[60,65)`,sum(`[65,70)`) `[65,70)`,sum(`[70,75)`) `[70,75)`,sum(`[75,80)`) `[75,80)`,sum(`[80,100)`) `[80,100)`
             from x
             group by newid)
             using (newid)")
    y[is.na(y)]<-0  ###turn  NA to 0
    return(y)
  }
  
  for(i_time in 1:num_time_slots){
    member[,colnames_time_use_location_x[i_time]] <- fun_home(member[,colnames_time_use_location_x[i_time]],member[,colnames_time_use_location[i_time]],member)
  }
  
  # member$home_time_slot1<-fun_home(member$home_time_slot1,member[,colnames_time_use_location[1]],member)
  # member$home_time_slot2<-fun_home(member$home_time_slot2,member$`08-09h`,member)
  # member$home_time_slot3<-fun_home(member$home_time_slot3,member$`09-10h`,member)
  # member$home_time_slot4<-fun_home(member$home_time_slot4,member$`10-11h`,member)
  # member$home_time_slot5<-fun_home(member$home_time_slot5,member$`11-12h`,member)
  # member$home_time_slot6<-fun_home(member$home_time_slot6,member$`12-13h`,member)
  # member$home_time_slot7<-fun_home(member$home_time_slot7,member$`13-14h`,member)
  # member$home_time_slot8<-fun_home(member$home_time_slot8,member$`14-15h`,member)
  # member$home_time_slot9<-fun_home(member$home_time_slot9,member$`15-16h`,member)
  # member$home_time_slot10<-fun_home(member$home_time_slot10,member$`16-17h`,member)
  # member$home_time_slot11<-fun_home(member$home_time_slot11,member$`17-18h`,member)
  # member$home_time_slot12<-fun_home(member$home_time_slot12,member$`18-19h`,member)
  # member$home_time_slot13<-fun_home(member$home_time_slot13,member$`19-20h`,member)
  # member$home_time_slot14<-fun_home(member$home_time_slot14,member$`20-22h`,member)
  # member$home_time_slot15<-fun_home(member$home_time_slot15,member$`22-24h`,member)
  # member$home_time_slot16<-fun_home(member$home_time_slot16,member$`24-02h`,member)
  # member$home_time_slot17<-fun_home(member$home_time_slot17,member$`02-05h`,member)
  
  i_time <- 1
  for(i_time in 1:num_time_slots){
    home_time_slot_x <- spread(member, m_age_cat,colnames_time_use_location_x[i_time])
    home_time_slot_x <- func_na(home_time_slot_x)
    home_time_slot_x <- func_time(home_time_slot_x)
  
    columns_age_cat <- grepl('\\[',names(home_time_slot_x))
    if(i_time ==1){
      tij_home_ind <- home_time_slot_x
      tij_home_ind[,columns_age_cat] <- 0
    }
    
    tij_home_ind[,columns_age_cat] <- tij_home_ind[,columns_age_cat] + 
                                        home_time_slot_x[columns_age_cat]
  }
  
  names(tij_home_ind)
  # remove redundant columns
  tij_home_ind<-tij_home_ind[,-c(1,2)]
  
  # save result
  Output1 = paste0("tij_home_ind_MI",imputation_index,"_",bootstrap_index, ".RData")
  save(tij_home_ind, file = smd_file_path(output_folder,Output1))
  
  # home_time_slot2<-spread(member, m_age_cat,home_time_slot2)
  # home_time_slot3<-spread(member, m_age_cat,home_time_slot3)
  # home_time_slot4<-spread(member, m_age_cat,home_time_slot4)
  # home_time_slot5<-spread(member, m_age_cat,home_time_slot5)
  # home_time_slot6<-spread(member, m_age_cat,home_time_slot6)
  # home_time_slot7<-spread(member, m_age_cat,home_time_slot7)
  # home_time_slot8<-spread(member, m_age_cat,home_time_slot8)
  # home_time_slot9<-spread(member, m_age_cat,home_time_slot9)
  # home_time_slot10<-spread(member, m_age_cat,home_time_slot10)
  # home_time_slot11<-spread(member, m_age_cat,home_time_slot11)
  # home_time_slot12<-spread(member, m_age_cat,home_time_slot12)
  # home_time_slot13<-spread(member, m_age_cat,home_time_slot13)
  # home_time_slot14<-spread(member, m_age_cat,home_time_slot14)
  # home_time_slot15<-spread(member, m_age_cat,home_time_slot15)
  # home_time_slot16<-spread(member, m_age_cat,home_time_slot16)
  # home_time_slot17<-spread(member, m_age_cat,home_time_slot17)
  # 
  # 
  # 
  # #install.packages("sqldf")
  # library(sqldf)
  # fun_na<-function(x){
  #   for (i in 41:57){
  #     x[,i][is.na(x[,i])]<-0
  #   } 
  #   return(x)
  # }
  # 
  # home_time_slot1<-fun_na(home_time_slot1)
  # home_time_slot2<-fun_na(home_time_slot2)
  # home_time_slot3<-fun_na(home_time_slot3)
  # home_time_slot4<-fun_na(home_time_slot4)
  # home_time_slot5<-fun_na(home_time_slot5)
  # home_time_slot6<-fun_na(home_time_slot6)
  # home_time_slot7<-fun_na(home_time_slot7)
  # home_time_slot8<-fun_na(home_time_slot8)
  # home_time_slot9<-fun_na(home_time_slot9)
  # home_time_slot10<-fun_na(home_time_slot10)
  # home_time_slot11<-fun_na(home_time_slot11)
  # home_time_slot12<-fun_na(home_time_slot12)
  # home_time_slot13<-fun_na(home_time_slot13)
  # home_time_slot14<-fun_na(home_time_slot14)
  # home_time_slot15<-fun_na(home_time_slot15)
  # home_time_slot16<-fun_na(home_time_slot16)
  # home_time_slot17<-fun_na(home_time_slot17)
  # 
  # 
  # 
  # 
  # home_time_slot1<-func_time(home_time_slot1)
  # home_time_slot2<-func_time(home_time_slot2)
  # home_time_slot3<-func_time(home_time_slot3)
  # home_time_slot4<-func_time(home_time_slot4)
  # home_time_slot5<-func_time(home_time_slot5)
  # home_time_slot6<-func_time(home_time_slot6)
  # home_time_slot7<-func_time(home_time_slot7)
  # home_time_slot8<-func_time(home_time_slot8)
  # home_time_slot9<-func_time(home_time_slot9)
  # home_time_slot10<-func_time(home_time_slot10)
  # home_time_slot11<-func_time(home_time_slot11)
  # home_time_slot12<-func_time(home_time_slot12)
  # home_time_slot13<-func_time(home_time_slot13)
  # home_time_slot14<-func_time(home_time_slot14)
  # home_time_slot15<-func_time(home_time_slot15)
  # home_time_slot16<-func_time(home_time_slot16)
  # home_time_slot17<-func_time(home_time_slot17)
  # 
  # 
  # tij_home_ind<-home_time_slot1
  # tij_home_ind[,4:20]<-NULL
  # 
  # #####individual time-exposure at home###
  # 
  # for (i in 4: 20){
  #   tij_home_ind[i]<-home_time_slot1[i]+ home_time_slot2[i]+ home_time_slot3[i]+ home_time_slot4[i]+ home_time_slot5[i]+ home_time_slot6[i]+ home_time_slot7[i]+
  #     home_time_slot8[i]+ home_time_slot9[i]+ home_time_slot10[i]+ home_time_slot11[i]+ home_time_slot12[i]+ home_time_slot13[i]+ home_time_slot14[i]+
  #     home_time_slot15[i]+ home_time_slot16[i]+ home_time_slot17[i]
  # }
  # tij_home_ind<-tij_home_ind[,-c(1,2)]
  # 
  # Output1 = paste0("tij_home_ind_MI1_",boot, ".RData")
  # save(tij_home_ind, file = Output1) 
  
  
  ################################################
  #### GENERIC HELP FUNCTIONS               ######
  ################################################
    
  #TODO: make more readable!
  # based on func_work
  
  # x <- TUD_location
  # y <- TUD_location$age_cat
  # z <- TUD_location[,colnames_time_use_location[16]]
  # k <- boolean_condition
  func_location<-function(x,y,z,k=NULL){
    tbl_age_location  <- table(y,z)
    
    # if the results needs to be conditional... (e.g. transport)
    if(!is.null(k)){
      tbl_age_location  <- table(y[k==1],z[k==1])
    }
    
    # if location is present, we have 2 columns...
    if(ncol(tbl_age_location)==2){
      frac_age_location <- tbl_age_location[,2] / sum(tbl_age_location[,2])
    
    # else, average count is always zero
    } else{
      frac_age_location <- rep(0,nrow(tbl_age_location))
    }
      
      # convert factorial age categories into strings
      y                 <- as.character(y)
      
      # loop over age categories... from... to...
      for(i in unique(y))
        for (j in unique(y)){
          x[y == j,i] <- z[y==j] * frac_age_location[i]
        }
    
    return(x)
  }
  
  # location_tag <- 'transport'
  # file_tag     <- 'transport'
  # boolean_condition <- TUD_transport$trans_Y
  get_TUD_location <- function(TUD,location_tag,file_tag, boolean_condition = NULL){
    
    # convert TUD with all info (home, work, school,...) into 0/1 per time slot for the given location tag
    TUD_location  <- get_time_at_location(TUD,location_tag)
    
    # get age categories
    age_cat <- levels(TUD_location$age_cat)
    
    #i_time <- 16
    # loop over the time slotes
    for(i_time in 1:num_time_slots){
      location_time_slot_x <- func_location(TUD_location,
                                            TUD_location$age_cat,
                                            TUD_location[,colnames_time_use_location[i_time]],
                                            boolean_condition)[,c('age_cat',age_cat)]
      
      #get columns with age categories
      columns_age_cat <- grepl('\\[',names(location_time_slot_x))
      
      # initiate 'tij_location_ind' in the first iteration
      if(i_time ==1){
        tij_location_ind <- location_time_slot_x
        tij_location_ind[,columns_age_cat] <- 0
      }
      
      # add time slot results with total 'tij'
      tij_location_ind[,columns_age_cat] <- tij_location_ind[,columns_age_cat] + location_time_slot_x[,columns_age_cat]
    }
    
    output_filename = paste0("tij_",file_tag,"_ind_MI",imputation_index,"_",bootstrap_index, ".RData")
    save(tij_location_ind, file = smd_file_path(output_folder,output_filename))
    
    # return result
    return(tij_location_ind)
  }
  
  
  # TUD_kinder2 <- get(load('kinder')) 
  # table(TUD_kinder2[,time_use_slots] == TUD_location[,colnames_time_use_location])
  # 
  
  
  #######################################################################
  ##############B. TIME-EXPOSURE MATRIX AT WORK#############################
  ##########################################################################
  #####Rerun part I.PROCESSING THE DATASET before runing the following code##############

  # get TUD at workplace
  tij_work_ind <- get_TUD_location(TUD,'workplace','work')
  #table(get(load('tij_work_ind_MI1_1.RData'))== get(load('output/time_use/tij_work_ind_MI1_1.RData')))
  
  #TUD_work<-TUD[,-c(23:59)]
  # TUD_work <- get_time_at_location(TUD,'workplace')
  # 
  # for(i in t1){
  #   TUD_work[,i]<-as.character(TUD_work[,i])
  #   TUD_work[,i][TUD_work[,i]=="nonfilled"]<-NA
  #   TUD_work[,i][TUD_work[,i]!="workplace"]<-0
  #   TUD_work[,i][TUD_work[,i]=="workplace"]<-1
  # }
  # 
  # for(i in t2){
  #   TUD_work[,i]<-as.character(TUD_work[,i])
  #   TUD_work[,i][TUD_work[,i]=="nonfilled"]<-NA
  #   TUD_work[,i][TUD_work[,i]!="workplace"]<-0
  #   TUD_work[,i][TUD_work[,i]=="workplace"]<-2
  # }
  # 
  # for(i in t3){
  #   TUD_work[,i]<-as.character(TUD_work[,i])
  #   TUD_work[,i][TUD_work[,i]=="nonfilled"]<-NA
  #   TUD_work[,i][TUD_work[,i]!="workplace"]<-0
  #   TUD_work[,i][TUD_work[,i]=="workplace"]<-3
  # }
  # 
  # for (i in 6:22){
  #   TUD_work[,i]<-as.numeric(TUD_work[,i])
  # }
  
  
  ##library(plyr) # for count function###
  
  
  # TUD_work$'[0,3)'<-0
  # TUD_work$'[3,6)'<-0
  # TUD_work$'[6,12)'<-0
  # TUD_work$'[12,18)'<-0
  # TUD_work$'[18,25)'<-0
  # TUD_work$'[25,30)'<-0
  # TUD_work$'[30,35)'<-0
  # TUD_work$'[35,40)'<-0
  # TUD_work$'[40,45)'<-0
  # TUD_work$'[45,50)'<-0
  # TUD_work$'[50,55)'<-0
  # TUD_work$'[55,60)'<-0
  # TUD_work$'[60,65)'<-0
  # TUD_work$'[65,70)'<-0
  # TUD_work$'[70,75)'<-0
  # TUD_work$'[75,80)'<-0
  # TUD_work$'[80,100)'<-0
  
  
  
  
  
    #   
    # 
    #   {x[,27][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[1,2]/sum(table(y,z)[,2]))
    #   x[,28][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[2,2]/sum(table(y,z)[,2]))
    #   x[,29][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[3,2]/sum(table(y,z)[,2]))
    #   x[,30][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[4,2]/sum(table(y,z)[,2]))
    #   x[,31][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[5,2]/sum(table(y,z)[,2]))
    #   x[,32][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[6,2]/sum(table(y,z)[,2]))
    #   x[,33][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[7,2]/sum(table(y,z)[,2]))
    #   x[,34][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[8,2]/sum(table(y,z)[,2]))
    #   x[,35][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[9,2]/sum(table(y,z)[,2]))
    #   x[,36][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[10,2]/sum(table(y,z)[,2]))
    #   x[,37][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[11,2]/sum(table(y,z)[,2]))
    #   x[,38][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[12,2]/sum(table(y,z)[,2]))
    #   x[,39][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[13,2]/sum(table(y,z)[,2]))
    #   x[,40][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[14,2]/sum(table(y,z)[,2]))
    #   x[,41][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[15,2]/sum(table(y,z)[,2]))
    #   x[,42][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[16,2]/sum(table(y,z)[,2]))   
    #   x[,42][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[17,2]/sum(table(y,z)[,2]))   #LW: index-error
    #   x[,43][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[17,2]/sum(table(y,z)[,2]))   #LW: index-error
    #   }
    # }
    # return(x)  
  # }  
  
  # age_cat <- levels(TUD_work$age_cat)
  # for(i_time in 1:num_time_slots){
  #   work_time_slot_x <- func_work(TUD_work,TUD_work$age_cat,TUD_work[,colnames_time_use_location[i_time]])[,c('age_cat',age_cat)]
  #   
  #  columns_age_cat <- grepl('\\[',names(work_time_slot_x))
  #   if(i_time ==1){
  #     tij_work_ind <- work_time_slot_x
  #     tij_work_ind[,columns_age_cat] <- 0
  #   }
  #   
  #   tij_work_ind[,columns_age_cat] <- tij_work_ind[,columns_age_cat] + work_time_slot_x[,columns_age_cat]
  # }
  
  # 
  # work_time_slot1 <-func_work_orig(TUD_work_orig,TUD_work_orig$age_cat,TUD_work_orig$`05-08h`)[,c(25,27:43)]
  #  work_time_slot2 <-func_work_orig(TUD_work_orig,TUD_work_orig$age_cat,TUD_work_orig$`08-09h`)[,c(25,27:43)]
  #  work_time_slot3 <-func_work_orig(TUD_work_orig,TUD_work_orig$age_cat,TUD_work_orig$`09-10h`)[,c(25,27:43)]
  #  work_time_slot4 <-func_work_orig(TUD_work_orig,TUD_work_orig$age_cat,TUD_work_orig$`10-11h`)[,c(25,27:43)]
  #  work_time_slot5 <-func_work_orig(TUD_work_orig,TUD_work_orig$age_cat,TUD_work_orig$`11-12h`)[,c(25,27:43)]
  #  work_time_slot6 <-func_work_orig(TUD_work_orig,TUD_work_orig$age_cat,TUD_work_orig$`12-13h`)[,c(25,27:43)]
  #  work_time_slot7 <-func_work_orig(TUD_work_orig,TUD_work_orig$age_cat,TUD_work_orig$`13-14h`)[,c(25,27:43)]
  #  work_time_slot8 <-func_work_orig(TUD_work_orig,TUD_work_orig$age_cat,TUD_work_orig$`14-15h`)[,c(25,27:43)]
  #  work_time_slot9 <-func_work_orig(TUD_work_orig,TUD_work_orig$age_cat,TUD_work_orig$`15-16h`)[,c(25,27:43)]
  #  work_time_slot10 <-func_work_orig(TUD_work_orig,TUD_work_orig$age_cat,TUD_work_orig$`16-17h`)[,c(25,27:43)]
  #  work_time_slot11 <-func_work_orig(TUD_work_orig,TUD_work_orig$age_cat,TUD_work_orig$`17-18h`)[,c(25,27:43)]
  #  work_time_slot12 <-func_work_orig(TUD_work_orig,TUD_work_orig$age_cat,TUD_work_orig$`18-19h`)[,c(25,27:43)]
  #  work_time_slot13 <-func_work_orig(TUD_work_orig,TUD_work_orig$age_cat,TUD_work_orig$`19-20h`)[,c(25,27:43)]
  #  work_time_slot14 <-func_work_orig(TUD_work_orig,TUD_work_orig$age_cat,TUD_work_orig$`20-22h`)[,c(25,27:43)]
  #  work_time_slot15 <-func_work_orig(TUD_work_orig,TUD_work_orig$age_cat,TUD_work_orig$`22-24h`)[,c(25,27:43)]
  #  work_time_slot16 <-func_work_orig(TUD_work_orig,TUD_work_orig$age_cat,TUD_work_orig$`24-02h`)[,c(25,27:43)]
  #  work_time_slot17 <-func_work_orig(TUD_work_orig,TUD_work_orig$age_cat,TUD_work_orig$`02-05h`)[,c(25,27:43)]
   
  # 
  # #####individual time-exposure at work###
  # 
  # tij_work_ind<-work_time_slot1
  # tij_work_ind[,2:18]<-NULL
  # 
  # for (i in 2: 18){
  #   tij_work_ind[i]<-work_time_slot1[i]+ work_time_slot2[i]+ work_time_slot3[i]+ work_time_slot4[i]+ work_time_slot5[i]+ work_time_slot6[i]+
  #     work_time_slot7[i]+ work_time_slot8[i]+ work_time_slot9[i]+ work_time_slot10[i]+ work_time_slot11[i]+ work_time_slot12[i]+ work_time_slot13[i]+
  #     work_time_slot14[i]+ work_time_slot15[i]+ work_time_slot16[i] + work_time_slot17[i] 
  # }
  
  # Output2 = paste0("tij_work_ind_MI1_",boot, ".RData")
  # save(tij_work_ind, file = Output2) 
  # Output2 = paste0("tij_work_ind_MI",imputation_index,"_",bootstrap_index, ".RData")
  # save(tij_work_ind, file = smd_file_path(output_folder,Output2))
  # 
  
  
  #######################################################################
  ##############c. TIME-EXPOSURE MATRIX AT KINDER-GARTEN#############################
  ##########################################################################
  #####Rerun part I.PROCESSING THE DATASET before runing the following code##############
  
  # get TUD at 'kinder-garten'
  get_TUD_location(TUD,'kinder-garten','kinder')
  #table(get(load('tij_kinder_ind_MI1_1.RData')) == get(load('output/time_use/tij_kinder_ind_MI1_1.RData')))
  
  
  # TUD_kinder_garten<-TUD[,-c(23:59)]
  # 
  # for(i in t1){
  #   TUD_kinder_garten[,i]<-as.character(TUD_kinder_garten[,i])
  #   TUD_kinder_garten[,i][TUD_kinder_garten[,i]=="nonfilled"]<-NA
  #   TUD_kinder_garten[,i][TUD_kinder_garten[,i]!="kinder-garten"]<-0
  #   TUD_kinder_garten[,i][TUD_kinder_garten[,i]=="kinder-garten"]<-1
  # }
  # 
  # for(i in t2){
  #   TUD_kinder_garten[,i]<-as.character(TUD_kinder_garten[,i])
  #   TUD_kinder_garten[,i][TUD_kinder_garten[,i]=="nonfilled"]<-NA
  #   TUD_kinder_garten[,i][TUD_kinder_garten[,i]!="kinder-garten"]<-0
  #   TUD_kinder_garten[,i][TUD_kinder_garten[,i]=="kinder-garten"]<-2
  # }
  # 
  # for(i in t3){
  #   TUD_kinder_garten[,i]<-as.character(TUD_kinder_garten[,i])
  #   TUD_kinder_garten[,i][TUD_kinder_garten[,i]=="nonfilled"]<-NA
  #   TUD_kinder_garten[,i][TUD_kinder_garten[,i]!="kinder-garten"]<-0
  #   TUD_kinder_garten[,i][TUD_kinder_garten[,i]=="kinder-garten"]<-3
  # }
  # 
  # for (i in 6:22){
  #   TUD_kinder_garten[,i]<-as.numeric(TUD_kinder_garten[,i])
  # }
  # 
  # 
  # TUD_kinder_garten$'[0,3)'<-0
  # TUD_kinder_garten$'[3,6)'<-0
  # TUD_kinder_garten$'[6,12)'<-0
  # TUD_kinder_garten$'[12,18)'<-0
  # TUD_kinder_garten$'[18,25)'<-0
  # TUD_kinder_garten$'[25,30)'<-0
  # TUD_kinder_garten$'[30,35)'<-0
  # TUD_kinder_garten$'[35,40)'<-0
  # TUD_kinder_garten$'[40,45)'<-0
  # TUD_kinder_garten$'[45,50)'<-0
  # TUD_kinder_garten$'[50,55)'<-0
  # TUD_kinder_garten$'[55,60)'<-0
  # TUD_kinder_garten$'[60,65)'<-0
  # TUD_kinder_garten$'[65,70)'<-0
  # TUD_kinder_garten$'[70,75)'<-0
  # TUD_kinder_garten$'[75,80)'<-0
  # TUD_kinder_garten$'[80,100)'<-0
  # 
  # 
  # func_work<-function(x,y,z){
  #   for (j in 1:17){
  #     if(dim(table(y,z))[2]==2)
  #     {x[,27][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[1,2]/sum(table(y,z)[,2]))
  #     x[,28][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[2,2]/sum(table(y,z)[,2]))
  #     x[,29][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[3,2]/sum(table(y,z)[,2]))
  #     x[,30][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[4,2]/sum(table(y,z)[,2]))
  #     x[,31][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[5,2]/sum(table(y,z)[,2]))
  #     x[,32][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[6,2]/sum(table(y,z)[,2]))
  #     x[,33][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[7,2]/sum(table(y,z)[,2]))
  #     x[,34][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[8,2]/sum(table(y,z)[,2]))
  #     x[,35][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[9,2]/sum(table(y,z)[,2]))
  #     x[,36][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[10,2]/sum(table(y,z)[,2]))
  #     x[,37][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[11,2]/sum(table(y,z)[,2]))
  #     x[,38][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[12,2]/sum(table(y,z)[,2]))
  #     x[,39][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[13,2]/sum(table(y,z)[,2]))
  #     x[,40][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[14,2]/sum(table(y,z)[,2]))
  #     x[,41][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[15,2]/sum(table(y,z)[,2]))
  #     x[,42][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[16,2]/sum(table(y,z)[,2]))
  #     x[,42][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[17,2]/sum(table(y,z)[,2]))   #LW: index-error
  #     x[,43][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[17,2]/sum(table(y,z)[,2]))   #LW: index-error
  #     }
  #   }
  #   return(x)  
  # }  
  # 
  # #table(TUD_kinder_garten$age_cat,TUD_kinder_garten$`05-08h`)
  # 
  # kinder_time_slot1 <-func_work(TUD_kinder_garten,TUD_kinder_garten$age_cat,TUD_kinder_garten$`05-08h`)[,c(25,27:43)]
  # kinder_time_slot2 <-func_work(TUD_kinder_garten,TUD_kinder_garten$age_cat,TUD_kinder_garten$`08-09h`)[,c(25,27:43)]
  # kinder_time_slot3 <-func_work(TUD_kinder_garten,TUD_kinder_garten$age_cat,TUD_kinder_garten$`09-10h`)[,c(25,27:43)]
  # kinder_time_slot4 <-func_work(TUD_kinder_garten,TUD_kinder_garten$age_cat,TUD_kinder_garten$`10-11h`)[,c(25,27:43)]
  # kinder_time_slot5 <-func_work(TUD_kinder_garten,TUD_kinder_garten$age_cat,TUD_kinder_garten$`11-12h`)[,c(25,27:43)]
  # kinder_time_slot6 <-func_work(TUD_kinder_garten,TUD_kinder_garten$age_cat,TUD_kinder_garten$`12-13h`)[,c(25,27:43)]
  # kinder_time_slot7 <-func_work(TUD_kinder_garten,TUD_kinder_garten$age_cat,TUD_kinder_garten$`13-14h`)[,c(25,27:43)]
  # kinder_time_slot8 <-func_work(TUD_kinder_garten,TUD_kinder_garten$age_cat,TUD_kinder_garten$`14-15h`)[,c(25,27:43)]
  # kinder_time_slot9 <-func_work(TUD_kinder_garten,TUD_kinder_garten$age_cat,TUD_kinder_garten$`15-16h`)[,c(25,27:43)]
  # kinder_time_slot10 <-func_work(TUD_kinder_garten,TUD_kinder_garten$age_cat,TUD_kinder_garten$`16-17h`)[,c(25,27:43)]
  # kinder_time_slot11 <-func_work(TUD_kinder_garten,TUD_kinder_garten$age_cat,TUD_kinder_garten$`17-18h`)[,c(25,27:43)]
  # kinder_time_slot12 <-func_work(TUD_kinder_garten,TUD_kinder_garten$age_cat,TUD_kinder_garten$`18-19h`)[,c(25,27:43)]
  # kinder_time_slot13 <-func_work(TUD_kinder_garten,TUD_kinder_garten$age_cat,TUD_kinder_garten$`19-20h`)[,c(25,27:43)]
  # kinder_time_slot14 <-func_work(TUD_kinder_garten,TUD_kinder_garten$age_cat,TUD_kinder_garten$`20-22h`)[,c(25,27:43)]
  # kinder_time_slot15 <-func_work(TUD_kinder_garten,TUD_kinder_garten$age_cat,TUD_kinder_garten$`22-24h`)[,c(25,27:43)]
  # kinder_time_slot16 <-func_work(TUD_kinder_garten,TUD_kinder_garten$age_cat,TUD_kinder_garten$`24-02h`)[,c(25,27:43)]
  # kinder_time_slot17 <-func_work(TUD_kinder_garten,TUD_kinder_garten$age_cat,TUD_kinder_garten$`02-05h`)[,c(25,27:43)]
  # 
  # 
  # #####individual time-exposure at kinder_garten###
  # tij_kinder_ind<-kinder_time_slot1
  # tij_kinder_ind[,2:18]<-NULL
  # 
  # for(i in 2:18){
  #   tij_kinder_ind[i]<-kinder_time_slot1[i] + kinder_time_slot2[i] +kinder_time_slot3[i]+kinder_time_slot4[i]+kinder_time_slot5[i]+kinder_time_slot6[i]+kinder_time_slot7[i]+kinder_time_slot8[i]+kinder_time_slot9[i]+
  #     kinder_time_slot10[i]+kinder_time_slot11[i]+kinder_time_slot12[i]+kinder_time_slot13[i]+kinder_time_slot14[i]+kinder_time_slot15[i]+kinder_time_slot16[i]+kinder_time_slot17[i]
  # }
  # 
  # Output3 = paste0("tij_kinder_ind_MI1_",boot, ".RData")
  # save(tij_kinder_ind, file = Output3) 
  
  
  
  #######################################################################
  ##############D. TIME-EXPOSURE MATRIX AT SCHOOL #############################
  ##########################################################################
  #####Rerun part I.PROCESSING THE DATASET before runing the following code##############
  
  # TUD_school<-TUD[,-c(23:59)]

  # for(i in t1){
  #   TUD_school[,i]<-as.character(TUD_school[,i])
  #   TUD_school[,i][TUD_school[,i]=="nonfilled"]<-NA
  #   TUD_school[,i][TUD_school[,i]!="school"]<-0
  #   TUD_school[,i][TUD_school[,i]=="school"]<-1
  # }
  # 
  # for(i in t2){
  #   TUD_school[,i]<-as.character(TUD_school[,i])
  #   TUD_school[,i][TUD_school[,i]=="nonfilled"]<-NA
  #   TUD_school[,i][TUD_school[,i]!="school"]<-0
  #   TUD_school[,i][TUD_school[,i]=="school"]<-2
  # }
  # 
  # for(i in t3){
  #   TUD_school[,i]<-as.character(TUD_school[,i])
  #   TUD_school[,i][TUD_school[,i]=="nonfilled"]<-NA
  #   TUD_school[,i][TUD_school[,i]!="school"]<-0
  #   TUD_school[,i][TUD_school[,i]=="school"]<-3
  # }
  # 
  # for (i in 6:22){
  #   TUD_school[,i]<-as.numeric(TUD_school[,i])
  # }
  # 
  # TUD_school$'[0,3)'<-0
  # TUD_school$'[3,6)'<-0
  # TUD_school$'[6,12)'<-0
  # TUD_school$'[12,18)'<-0
  # TUD_school$'[18,25)'<-0
  # TUD_school$'[25,30)'<-0
  # TUD_school$'[30,35)'<-0
  # TUD_school$'[35,40)'<-0
  # TUD_school$'[40,45)'<-0
  # TUD_school$'[45,50)'<-0
  # TUD_school$'[50,55)'<-0
  # TUD_school$'[55,60)'<-0
  # TUD_school$'[60,65)'<-0
  # TUD_school$'[65,70)'<-0
  # TUD_school$'[70,75)'<-0
  # TUD_school$'[75,80)'<-0
  # TUD_school$'[80,100)'<-0
  
  ###########Based on education system to calculate time-exposure of respondents. Particularly, children in age 6-12 will have time-exposure with children in the same age for full time-slot, and time-exposure with people older than 25 proportional to
  #their participantion in that time slot. children in age 6-12 will not have time-exposure with other school-age children, e.g. 0-3, 12-18...
  
  
  # school_time_slot4 <-func_school(TUD_school,TUD_school$age_cat,TUD_school$`10-11h`)[,c(25,27:43)]
  # x <- TUD_school
  # y <- TUD_school$age_cat
  # z <- TUD_school[,colnames_time_use_location[4]]
  func_school <- function(x,y,z){
    
    # first apply the general exposure procedure
    x_new <- func_location(x,y,z)

    # next: apply full exposure for own age, but '0' for other school-age-classes
    school_ages <- as.character(levels(y)[1:5])
    for(j in 1:length(school_ages)){
      
      school_age_x <- school_ages[j]
      school_age_other <- school_ages[-j]
      
      x_new[y == school_age_x,school_age_x] <- z[y==school_age_x]
      x_new[y == school_age_x,school_age_other] <- 0
      
    }
    
    return(x_new)
  }
  # 
  # j <- 1
  # func_school<-function(x,y,z){
  #   for (j in 1:17){
  #     if(dim(table(y,z))[2]==2)
  #     {
  #       x[,27][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[1,2]/sum(table(y,z)[,2]))
  #       x[,27][y=="[0,3)"] <- z[y=="[0,3)"]
  #       x[,27][y=="[3,6)"] <- 0
  #       x[,27][y=="[6,12)"] <- 0
  #       x[,27][y=="[12,18)"] <- 0
  #       x[,27][y=="[18,25)"] <- 0
  #       
  #       x[,28][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[2,2]/sum(table(y,z)[,2]))
  #       x[,28][y=="[3,6)"] <- z[y=="[3,6)"]
  #       x[,28][y=="[0,3)"] <- 0
  #       x[,28][y=="[6,12)"] <- 0
  #       x[,28][y=="[12,18)"] <- 0
  #       x[,28][y=="[18,25)"] <- 0
  #       
  #       x[,29][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[3,2]/sum(table(y,z)[,2]))
  #       x[,29][y=="[6,12)"] <- z[y=="[6,12)"]
  #       x[,29][y=="[0,3)"] <- 0
  #       x[,29][y=="[3,6)"] <- 0
  #       x[,29][y=="[12,18)"] <- 0
  #       x[,29][y=="[18,25)"] <- 0
  #       
  #       x[,30][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[4,2]/sum(table(y,z)[,2]))
  #       x[,30][y=="[12,18)"] <- z[y=="[12,18)"]
  #       x[,30][y=="[0,3)"] <- 0
  #       x[,30][y=="[3,6)"] <- 0
  #       x[,30][y=="[6,12)"] <- 0
  #       x[,30][y=="[18,25)"] <- 0
  #       
  #       x[,31][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[5,2]/sum(table(y,z)[,2]))
  #       x[,31][y=="[18,25)"] <- z[y=="[18,25)"]
  #       x[,31][y=="[0,3)"] <- 0
  #       x[,31][y=="[3,6)"] <- 0
  #       x[,31][y=="[6,12)"] <- 0
  #       x[,31][y=="[12,18)"] <- 0
  #       
  #       x[,32][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[6,2]/sum(table(y,z)[,2]))
  #       x[,33][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[7,2]/sum(table(y,z)[,2]))
  #       x[,34][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[8,2]/sum(table(y,z)[,2]))
  #       x[,35][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[9,2]/sum(table(y,z)[,2]))
  #       x[,36][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[10,2]/sum(table(y,z)[,2]))
  #       x[,37][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[11,2]/sum(table(y,z)[,2]))
  #       x[,38][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[12,2]/sum(table(y,z)[,2]))
  #       x[,39][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[13,2]/sum(table(y,z)[,2]))
  #       x[,40][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[14,2]/sum(table(y,z)[,2]))
  #       x[,41][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[15,2]/sum(table(y,z)[,2]))
  #       x[,42][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[16,2]/sum(table(y,z)[,2]))
  #       #x[,42][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[17,2]/sum(table(y,z)[,2])) #LW: index error
  #       x[,43][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[17,2]/sum(table(y,z)[,2]))
  #     }    
  #   }
  #   return(x)  
  # }  
  # 
  # 
  # school_time_slot1 <-func_school(TUD_school,TUD_school$age_cat,TUD_school$`05-08h`)[,c(25,27:43)]
  # school_time_slot2 <-func_school(TUD_school,TUD_school$age_cat,TUD_school$`08-09h`)[,c(25,27:43)]
  # school_time_slot3 <-func_school(TUD_school,TUD_school$age_cat,TUD_school$`09-10h`)[,c(25,27:43)]
  # school_time_slot4 <-func_school(TUD_school,TUD_school$age_cat,TUD_school$`10-11h`)[,c(25,27:43)]
  # school_time_slot5 <-func_school(TUD_school,TUD_school$age_cat,TUD_school$`11-12h`)[,c(25,27:43)]
  # school_time_slot6 <-func_school(TUD_school,TUD_school$age_cat,TUD_school$`12-13h`)[,c(25,27:43)]
  # school_time_slot7 <-func_school(TUD_school,TUD_school$age_cat,TUD_school$`13-14h`)[,c(25,27:43)]
  # school_time_slot8 <-func_school(TUD_school,TUD_school$age_cat,TUD_school$`14-15h`)[,c(25,27:43)]
  # school_time_slot9 <-func_school(TUD_school,TUD_school$age_cat,TUD_school$`15-16h`)[,c(25,27:43)]
  # school_time_slot10 <-func_school(TUD_school,TUD_school$age_cat,TUD_school$`16-17h`)[,c(25,27:43)]
  # school_time_slot11 <-func_school(TUD_school,TUD_school$age_cat,TUD_school$`17-18h`)[,c(25,27:43)]
  # school_time_slot12 <-func_school(TUD_school,TUD_school$age_cat,TUD_school$`18-19h`)[,c(25,27:43)]
  # school_time_slot13 <-func_school(TUD_school,TUD_school$age_cat,TUD_school$`19-20h`)[,c(25,27:43)]
  # school_time_slot14 <-func_school(TUD_school,TUD_school$age_cat,TUD_school$`20-22h`)[,c(25,27:43)]  
  # school_time_slot15 <-func_school(TUD_school,TUD_school$age_cat,TUD_school$`22-24h`)[,c(25,27:43)]  
  # school_time_slot16 <-func_school(TUD_school,TUD_school$age_cat,TUD_school$`24-02h`)[,c(25,27:43)]  
  # school_time_slot17 <-func_school(TUD_school,TUD_school$age_cat,TUD_school$`02-05h`)[,c(25,27:43)]
  # 
  # 
  # #####individual time-exposure at school###
  # tij_school_ind<-school_time_slot1
  # tij_school_ind[,2:18]<-NULL
  # 
  # for(i in 2:18){
  #   tij_school_ind[i]<-school_time_slot1[i] + school_time_slot2[i] +school_time_slot3[i]+school_time_slot4[i]+school_time_slot5[i]+school_time_slot6[i]+school_time_slot7[i]+school_time_slot8[i]+school_time_slot9[i]+
  #     school_time_slot10[i]+school_time_slot11[i]+school_time_slot12[i]+school_time_slot13[i]+school_time_slot14[i]+school_time_slot15[i]+school_time_slot16[i]+school_time_slot17[i]
  # }
  # 
  # Output4 = paste0("tij_school_ind_MI1_",boot, ".RData")
  # save(tij_school_ind, file = Output4)  
  
  
  
  TUD_location <- get_time_at_location(TUD,'school')
  
  age_cat <- levels(TUD_location$age_cat)
  for(i_time in 1:num_time_slots){
    # use func_school!!
    location_time_slot_x <- func_school(TUD_location,TUD_location$age_cat,TUD_location[,colnames_time_use_location[i_time]])[,c('age_cat',age_cat)]
    
    #print(table(location_time_slot_x == get(paste0('leisure_time_slot',i_time))))
    columns_age_cat <- grepl('\\[',names(location_time_slot_x))
    if(i_time ==1){
      tij_location_ind <- location_time_slot_x
      tij_location_ind[,columns_age_cat] <- 0
    }
    
    tij_location_ind[,columns_age_cat] <- tij_location_ind[,columns_age_cat] + location_time_slot_x[,columns_age_cat]
  }
  
  output_filename = paste0("tij_school_ind_MI",imputation_index,"_",bootstrap_index, ".RData")
  save(tij_location_ind, file = smd_file_path(output_folder,output_filename))
  #table(get(load('tij_school_ind_MI1_1.RData')) == get(load('output/time_use/tij_school_ind_MI1_1.RData')))
  
  
  #######################################################################
  ##############E. TIME-EXPOSURE MATRIX AT LEISURE#############################
  ##########################################################################
  #####Rerun part I.PROCESSING THE DATASET before runing the following code##############
  
  # get TUD at 'leisure
  get_TUD_location(TUD,'leisure','leisure')
  #table(get(load('tij_leisure_ind_MI1_1.RData')) == get(load('output/time_use/tij_leisure_ind_MI1_1.RData')))
  
  # TUD_leisure<-TUD[,-c(23:59)]
  # 
  # for(i in t1){
  #   TUD_leisure[,i]<-as.character(TUD_leisure[,i])
  #   TUD_leisure[,i][TUD_leisure[,i]=="nonfilled"]<-NA
  #   TUD_leisure[,i][TUD_leisure[,i]!="leisure"]<-0
  #   TUD_leisure[,i][TUD_leisure[,i]=="leisure"]<-1
  # }
  # 
  # for(i in t2){
  #   TUD_leisure[,i]<-as.character(TUD_leisure[,i])
  #   TUD_leisure[,i][TUD_leisure[,i]=="nonfilled"]<-NA
  #   TUD_leisure[,i][TUD_leisure[,i]!="leisure"]<-0
  #   TUD_leisure[,i][TUD_leisure[,i]=="leisure"]<-2
  # }
  # 
  # for(i in t3){
  #   TUD_leisure[,i]<-as.character(TUD_leisure[,i])
  #   TUD_leisure[,i][TUD_leisure[,i]=="nonfilled"]<-NA
  #   TUD_leisure[,i][TUD_leisure[,i]!="leisure"]<-0
  #   TUD_leisure[,i][TUD_leisure[,i]=="leisure"]<-3
  # }
  # 
  # for (i in 6:22){
  #   TUD_leisure[,i]<-as.numeric(TUD_leisure[,i])
  # }
  # 
  # 
  # TUD_leisure$'[0,3)'<-0
  # TUD_leisure$'[3,6)'<-0
  # TUD_leisure$'[6,12)'<-0
  # TUD_leisure$'[12,18)'<-0
  # TUD_leisure$'[18,25)'<-0
  # TUD_leisure$'[25,30)'<-0
  # TUD_leisure$'[30,35)'<-0
  # TUD_leisure$'[35,40)'<-0
  # TUD_leisure$'[40,45)'<-0
  # TUD_leisure$'[45,50)'<-0
  # TUD_leisure$'[50,55)'<-0
  # TUD_leisure$'[55,60)'<-0
  # TUD_leisure$'[60,65)'<-0
  # TUD_leisure$'[65,70)'<-0
  # TUD_leisure$'[70,75)'<-0
  # TUD_leisure$'[75,80)'<-0
  # TUD_leisure$'[80,100)'<-0
  # 
  # 
  # func_work<-function(x,y,z){
  #   for (j in 1:17){
  #     if(dim(table(y,z))[2]==2)
  #     {x[,27][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[1,2]/sum(table(y,z)[,2]))
  #     x[,28][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[2,2]/sum(table(y,z)[,2]))
  #     x[,29][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[3,2]/sum(table(y,z)[,2]))
  #     x[,30][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[4,2]/sum(table(y,z)[,2]))
  #     x[,31][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[5,2]/sum(table(y,z)[,2]))
  #     x[,32][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[6,2]/sum(table(y,z)[,2]))
  #     x[,33][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[7,2]/sum(table(y,z)[,2]))
  #     x[,34][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[8,2]/sum(table(y,z)[,2]))
  #     x[,35][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[9,2]/sum(table(y,z)[,2]))
  #     x[,36][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[10,2]/sum(table(y,z)[,2]))
  #     x[,37][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[11,2]/sum(table(y,z)[,2]))
  #     x[,38][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[12,2]/sum(table(y,z)[,2]))
  #     x[,39][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[13,2]/sum(table(y,z)[,2]))
  #     x[,40][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[14,2]/sum(table(y,z)[,2]))
  #     x[,41][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[15,2]/sum(table(y,z)[,2]))
  #     x[,42][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[16,2]/sum(table(y,z)[,2]))
  #     x[,42][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[17,2]/sum(table(y,z)[,2]))
  #     x[,43][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[17,2]/sum(table(y,z)[,2]))
  #     }
  #   }
  #   return(x)  
  # }   
  # 
  # 
  # leisure_time_slot1 <-func_work(TUD_leisure,TUD_leisure$age_cat,TUD_leisure$`05-08h`)[,c(25,27:43)]
  # leisure_time_slot2 <-func_work(TUD_leisure,TUD_leisure$age_cat,TUD_leisure$`08-09h`)[,c(25,27:43)]
  # leisure_time_slot3 <-func_work(TUD_leisure,TUD_leisure$age_cat,TUD_leisure$`09-10h`)[,c(25,27:43)]
  # leisure_time_slot4 <-func_work(TUD_leisure,TUD_leisure$age_cat,TUD_leisure$`10-11h`)[,c(25,27:43)]
  # leisure_time_slot5 <-func_work(TUD_leisure,TUD_leisure$age_cat,TUD_leisure$`11-12h`)[,c(25,27:43)]
  # leisure_time_slot6 <-func_work(TUD_leisure,TUD_leisure$age_cat,TUD_leisure$`12-13h`)[,c(25,27:43)]
  # leisure_time_slot7 <-func_work(TUD_leisure,TUD_leisure$age_cat,TUD_leisure$`13-14h`)[,c(25,27:43)]
  # leisure_time_slot8 <-func_work(TUD_leisure,TUD_leisure$age_cat,TUD_leisure$`14-15h`)[,c(25,27:43)]
  # leisure_time_slot9 <-func_work(TUD_leisure,TUD_leisure$age_cat,TUD_leisure$`15-16h`)[,c(25,27:43)]
  # leisure_time_slot10 <-func_work(TUD_leisure,TUD_leisure$age_cat,TUD_leisure$`16-17h`)[,c(25,27:43)]
  # leisure_time_slot11 <-func_work(TUD_leisure,TUD_leisure$age_cat,TUD_leisure$`17-18h`)[,c(25,27:43)]
  # leisure_time_slot12 <-func_work(TUD_leisure,TUD_leisure$age_cat,TUD_leisure$`18-19h`)[,c(25,27:43)]
  # leisure_time_slot13 <-func_work(TUD_leisure,TUD_leisure$age_cat,TUD_leisure$`19-20h`)[,c(25,27:43)]
  # leisure_time_slot14 <-func_work(TUD_leisure,TUD_leisure$age_cat,TUD_leisure$`19-20h`)[,c(25,27:43)] #LW: index-error
  # leisure_time_slot15 <-func_work(TUD_leisure,TUD_leisure$age_cat,TUD_leisure$`19-20h`)[,c(25,27:43)] #LW: index-error
  # leisure_time_slot16 <-func_work(TUD_leisure,TUD_leisure$age_cat,TUD_leisure$`19-20h`)[,c(25,27:43)] #LW: index-error
  # leisure_time_slot17 <-func_work(TUD_leisure,TUD_leisure$age_cat,TUD_leisure$`19-20h`)[,c(25,27:43)] #LW: index-error
  # 
  # 
  # #####individual time-exposure at leisure place###
  # tij_leisure_ind<-leisure_time_slot1
  # tij_leisure_ind[,2:18]<-NULL
  # 
  # for(i in 2:18){
  #   tij_leisure_ind[i]<-leisure_time_slot1[i] + leisure_time_slot2[i] +leisure_time_slot3[i]+leisure_time_slot4[i]+leisure_time_slot5[i]+leisure_time_slot6[i]+leisure_time_slot7[i]+leisure_time_slot8[i]+leisure_time_slot9[i]+
  #     leisure_time_slot10[i]+leisure_time_slot11[i]+leisure_time_slot12[i]+leisure_time_slot13[i]+leisure_time_slot14[i]+leisure_time_slot15[i]+leisure_time_slot16[i]+leisure_time_slot17[i]
  # }
  # 
  # table(is.na(tij_leisure_ind[,2]))
  # 
  # 
  # Output5 = paste0("tij_leisure_ind_MI1_",boot, ".RData")
  # save(tij_leisure_ind, file = Output5) 
  
  
  #######################################################################
  ##############F. TIME-EXPOSURE MATRIX AT OTHER PLACES#############################
  ##########################################################################
  #####Rerun part I.PROCESSING THE DATASET before runing the following code##############
  
  # get TUD at 'other'
  tij_other_ind <- get_TUD_location(TUD,'other','other')
  #table(get(load('tij_other_ind_MI1_1.RData')) == get(load('output/time_use/tij_other_ind_MI1_1.RData')))
   
  dim(tij_other_ind)
  tij_other_ind[1:5,1:5]
  
  # TUD_other<-TUD[,-c(23:59)]
  # 
  # for(i in t1){
  #   TUD_other[,i]<-as.character(TUD_other[,i])
  #   TUD_other[,i][TUD_other[,i]=="nonfilled"]<-NA
  #   TUD_other[,i][TUD_other[,i]!="other"]<-0
  #   TUD_other[,i][TUD_other[,i]=="other"]<-1
  # }
  # 
  # for(i in t2){
  #   TUD_other[,i]<-as.character(TUD_other[,i])
  #   TUD_other[,i][TUD_other[,i]=="nonfilled"]<-NA
  #   TUD_other[,i][TUD_other[,i]!="other"]<-0
  #   TUD_other[,i][TUD_other[,i]=="other"]<-2
  # }
  # 
  # for(i in t3){
  #   TUD_other[,i]<-as.character(TUD_other[,i])
  #   TUD_other[,i][TUD_other[,i]=="nonfilled"]<-NA
  #   TUD_other[,i][TUD_other[,i]!="other"]<-0
  #   TUD_other[,i][TUD_other[,i]=="other"]<-3
  # }
  # 
  # for (i in 6:22){
  #   TUD_other[,i]<-as.numeric(TUD_other[,i])
  # }
  # 
  # 
  # TUD_other$'[0,3)'<-0
  # TUD_other$'[3,6)'<-0
  # TUD_other$'[6,12)'<-0
  # TUD_other$'[12,18)'<-0
  # TUD_other$'[18,25)'<-0
  # TUD_other$'[25,30)'<-0
  # TUD_other$'[30,35)'<-0
  # TUD_other$'[35,40)'<-0
  # TUD_other$'[40,45)'<-0
  # TUD_other$'[45,50)'<-0
  # TUD_other$'[50,55)'<-0
  # TUD_other$'[55,60)'<-0
  # TUD_other$'[60,65)'<-0
  # TUD_other$'[65,70)'<-0
  # TUD_other$'[70,75)'<-0
  # TUD_other$'[75,80)'<-0
  # TUD_other$'[80,100)'<-0
  # 
  # 
  # func_work<-function(x,y,z){
  #   for (j in 1:17){
  #     if(dim(table(y,z))[2]==2)
  #     {x[,27][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[1,2]/sum(table(y,z)[,2]))
  #     x[,28][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[2,2]/sum(table(y,z)[,2]))
  #     x[,29][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[3,2]/sum(table(y,z)[,2]))
  #     x[,30][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[4,2]/sum(table(y,z)[,2]))
  #     x[,31][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[5,2]/sum(table(y,z)[,2]))
  #     x[,32][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[6,2]/sum(table(y,z)[,2]))
  #     x[,33][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[7,2]/sum(table(y,z)[,2]))
  #     x[,34][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[8,2]/sum(table(y,z)[,2]))
  #     x[,35][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[9,2]/sum(table(y,z)[,2]))
  #     x[,36][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[10,2]/sum(table(y,z)[,2]))
  #     x[,37][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[11,2]/sum(table(y,z)[,2]))
  #     x[,38][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[12,2]/sum(table(y,z)[,2]))
  #     x[,39][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[13,2]/sum(table(y,z)[,2]))
  #     x[,40][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[14,2]/sum(table(y,z)[,2]))
  #     x[,41][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[15,2]/sum(table(y,z)[,2]))
  #     x[,42][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[16,2]/sum(table(y,z)[,2]))
  #     x[,42][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[17,2]/sum(table(y,z)[,2]))
  #     x[,43][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[17,2]/sum(table(y,z)[,2]))
  #     }
  #   }
  #   return(x)  
  # } 
  # 
  # 
  # other_time_slot1 <-func_work(TUD_other,TUD_other$age_cat,TUD_other$`05-08h`)[,c(25,27:43)]
  # other_time_slot2 <-func_work(TUD_other,TUD_other$age_cat,TUD_other$`08-09h`)[,c(25,27:43)]
  # other_time_slot3 <-func_work(TUD_other,TUD_other$age_cat,TUD_other$`09-10h`)[,c(25,27:43)]
  # other_time_slot4 <-func_work(TUD_other,TUD_other$age_cat,TUD_other$`10-11h`)[,c(25,27:43)]
  # other_time_slot5 <-func_work(TUD_other,TUD_other$age_cat,TUD_other$`11-12h`)[,c(25,27:43)]
  # other_time_slot6 <-func_work(TUD_other,TUD_other$age_cat,TUD_other$`12-13h`)[,c(25,27:43)]
  # other_time_slot7 <-func_work(TUD_other,TUD_other$age_cat,TUD_other$`13-14h`)[,c(25,27:43)]
  # other_time_slot8 <-func_work(TUD_other,TUD_other$age_cat,TUD_other$`14-15h`)[,c(25,27:43)]
  # other_time_slot9 <-func_work(TUD_other,TUD_other$age_cat,TUD_other$`15-16h`)[,c(25,27:43)]
  # other_time_slot10 <-func_work(TUD_other,TUD_other$age_cat,TUD_other$`16-17h`)[,c(25,27:43)]
  # other_time_slot11 <-func_work(TUD_other,TUD_other$age_cat,TUD_other$`17-18h`)[,c(25,27:43)]
  # other_time_slot12 <-func_work(TUD_other,TUD_other$age_cat,TUD_other$`18-19h`)[,c(25,27:43)]
  # other_time_slot13 <-func_work(TUD_other,TUD_other$age_cat,TUD_other$`19-20h`)[,c(25,27:43)]
  # other_time_slot14 <-func_work(TUD_other,TUD_other$age_cat,TUD_other$`19-20h`)[,c(25,27:43)] #LW: index-error
  # other_time_slot15 <-func_work(TUD_other,TUD_other$age_cat,TUD_other$`19-20h`)[,c(25,27:43)] #LW: index-error
  # other_time_slot16 <-func_work(TUD_other,TUD_other$age_cat,TUD_other$`19-20h`)[,c(25,27:43)] #LW: index-error
  # other_time_slot17 <-func_work(TUD_other,TUD_other$age_cat,TUD_other$`19-20h`)[,c(25,27:43)] #LW: index-error
  # 
  # 
  # #####individual time-exposure at other places###
  # tij_other_ind<-other_time_slot1
  # tij_other_ind[,2:18]<-NULL
  # 
  # for(i in 2:18){
  #   tij_other_ind[i]<-other_time_slot1[i] + other_time_slot2[i] +other_time_slot3[i]+other_time_slot4[i]+other_time_slot5[i]+other_time_slot6[i]+other_time_slot7[i]+other_time_slot8[i]+other_time_slot9[i]+
  #     other_time_slot10[i]+other_time_slot11[i]+other_time_slot12[i]+other_time_slot13[i]+other_time_slot14[i]+other_time_slot15[i]+other_time_slot16[i]+other_time_slot17[i]
  # }
  # 
  # #table(is.na(tij_other_ind[,2]))
  # 
  # Output6 = paste0("tij_other_ind_MI1",boot, ".RData")
  # save(tij_other_ind, file = Output6) 
  
  
  #######################################################################
  ##############G. TIME-EXPOSURE MATRIX AT FAMILY#############################
  ##########################################################################
  #####Rerun part I.PROCESSING THE DATASET before runing the following code##############
  
  get_TUD_location(TUD,'family','family')
  #table(get(load('tij_family_ind_MI1_1.RData')) == get(load('output/time_use/tij_family_ind_MI1_1.RData')))
  
  
  # TUD_family<-TUD[,-c(23:59)]
  # 
  # for(i in t1){
  #   TUD_family[,i]<-as.character(TUD_family[,i])
  #   TUD_family[,i][TUD_family[,i]=="nonfilled"]<-NA
  #   TUD_family[,i][TUD_family[,i]!="family"]<-0
  #   TUD_family[,i][TUD_family[,i]=="family"]<-1
  # }
  # 
  # for(i in t2){
  #   TUD_family[,i]<-as.character(TUD_family[,i])
  #   TUD_family[,i][TUD_family[,i]=="nonfilled"]<-NA
  #   TUD_family[,i][TUD_family[,i]!="family"]<-0
  #   TUD_family[,i][TUD_family[,i]=="family"]<-2
  # }
  # 
  # for(i in t3){
  #   TUD_family[,i]<-as.character(TUD_family[,i])
  #   TUD_family[,i][TUD_family[,i]=="nonfilled"]<-NA
  #   TUD_family[,i][TUD_family[,i]!="family"]<-0
  #   TUD_family[,i][TUD_family[,i]=="family"]<-3
  # }
  # 
  # for (i in 6:22){
  #   TUD_family[,i]<-as.numeric(TUD_family[,i])
  # }
  # 
  # 
  # TUD_family$'[0,3)'<-0
  # TUD_family$'[3,6)'<-0
  # TUD_family$'[6,12)'<-0
  # TUD_family$'[12,18)'<-0
  # TUD_family$'[18,25)'<-0
  # TUD_family$'[25,30)'<-0
  # TUD_family$'[30,35)'<-0
  # TUD_family$'[35,40)'<-0
  # TUD_family$'[40,45)'<-0
  # TUD_family$'[45,50)'<-0
  # TUD_family$'[50,55)'<-0
  # TUD_family$'[55,60)'<-0
  # TUD_family$'[60,65)'<-0
  # TUD_family$'[65,70)'<-0
  # TUD_family$'[70,75)'<-0
  # TUD_family$'[75,80)'<-0
  # TUD_family$'[80,100)'<-0
  # 
  # 
  # func_work<-function(x,y,z){
  #   for (j in 1:17){
  #     if(dim(table(y,z))[2]==2)
  #     {x[,27][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[1,2]/sum(table(y,z)[,2]))
  #     x[,28][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[2,2]/sum(table(y,z)[,2]))
  #     x[,29][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[3,2]/sum(table(y,z)[,2]))
  #     x[,30][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[4,2]/sum(table(y,z)[,2]))
  #     x[,31][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[5,2]/sum(table(y,z)[,2]))
  #     x[,32][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[6,2]/sum(table(y,z)[,2]))
  #     x[,33][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[7,2]/sum(table(y,z)[,2]))
  #     x[,34][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[8,2]/sum(table(y,z)[,2]))
  #     x[,35][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[9,2]/sum(table(y,z)[,2]))
  #     x[,36][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[10,2]/sum(table(y,z)[,2]))
  #     x[,37][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[11,2]/sum(table(y,z)[,2]))
  #     x[,38][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[12,2]/sum(table(y,z)[,2]))
  #     x[,39][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[13,2]/sum(table(y,z)[,2]))
  #     x[,40][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[14,2]/sum(table(y,z)[,2]))
  #     x[,41][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[15,2]/sum(table(y,z)[,2]))
  #     x[,42][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[16,2]/sum(table(y,z)[,2]))
  #     #x[,42][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[17,2]/sum(table(y,z)[,2])) #LW: index-error
  #     x[,43][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y,z)[17,2]/sum(table(y,z)[,2]))
  #     }
  #   }
  #   return(x)  
  # }   
  # 
  # family_time_slot1 <-func_work(TUD_family,TUD_family$age_cat,TUD_family$`05-08h`)[,c(25,27:43)]
  # family_time_slot2 <-func_work(TUD_family,TUD_family$age_cat,TUD_family$`08-09h`)[,c(25,27:43)]
  # family_time_slot3 <-func_work(TUD_family,TUD_family$age_cat,TUD_family$`09-10h`)[,c(25,27:43)]
  # family_time_slot4 <-func_work(TUD_family,TUD_family$age_cat,TUD_family$`10-11h`)[,c(25,27:43)]
  # family_time_slot5 <-func_work(TUD_family,TUD_family$age_cat,TUD_family$`11-12h`)[,c(25,27:43)]
  # family_time_slot6 <-func_work(TUD_family,TUD_family$age_cat,TUD_family$`12-13h`)[,c(25,27:43)]
  # family_time_slot7 <-func_work(TUD_family,TUD_family$age_cat,TUD_family$`13-14h`)[,c(25,27:43)]
  # family_time_slot8 <-func_work(TUD_family,TUD_family$age_cat,TUD_family$`14-15h`)[,c(25,27:43)]
  # family_time_slot9 <-func_work(TUD_family,TUD_family$age_cat,TUD_family$`15-16h`)[,c(25,27:43)]
  # family_time_slot10 <-func_work(TUD_family,TUD_family$age_cat,TUD_family$`16-17h`)[,c(25,27:43)]
  # family_time_slot11 <-func_work(TUD_family,TUD_family$age_cat,TUD_family$`17-18h`)[,c(25,27:43)]
  # family_time_slot12 <-func_work(TUD_family,TUD_family$age_cat,TUD_family$`18-19h`)[,c(25,27:43)]
  # family_time_slot13 <-func_work(TUD_family,TUD_family$age_cat,TUD_family$`19-20h`)[,c(25,27:43)]
  # family_time_slot14 <-func_work(TUD_family,TUD_family$age_cat,TUD_family$`19-20h`)[,c(25,27:43)] #LW: index-error
  # family_time_slot15 <-func_work(TUD_family,TUD_family$age_cat,TUD_family$`19-20h`)[,c(25,27:43)] #LW: index-error
  # family_time_slot16 <-func_work(TUD_family,TUD_family$age_cat,TUD_family$`19-20h`)[,c(25,27:43)] #LW: index-error
  # family_time_slot17 <-func_work(TUD_family,TUD_family$age_cat,TUD_family$`19-20h`)[,c(25,27:43)] #LW: index-error
  # 
  # 
  # #####individual time-exposure at family places###
  # tij_family_ind<-family_time_slot1
  # tij_family_ind[,2:18]<-NULL
  # 
  # for(i in 2:18){
  #   tij_family_ind[i]<-family_time_slot1[i] + family_time_slot2[i] +family_time_slot3[i]+family_time_slot4[i]+family_time_slot5[i]+family_time_slot6[i]+family_time_slot7[i]+family_time_slot8[i]+family_time_slot9[i]+
  #     family_time_slot10[i]+family_time_slot11[i]+family_time_slot12[i]+family_time_slot13[i]+family_time_slot14[i]+family_time_slot15[i]+family_time_slot16[i]+family_time_slot17[i]
  # }
  # 
  # #table(is.na(tij_family_ind[,2]))
  # 
  # 
  # Output7 = paste0("tij_family_ind_MI1_",boot, ".RData")
  # save(tij_family_ind, file = Output7) 
  
  #######################################################################
  ##############H. TIME-EXPOSURE MATRIX AT TRANSPORT#############################
  ##########################################################################
  
  # add transport data to TUD variable
  part_transport_data   <- part[c("newid","public_transport_bus","public_transport_train")]
  TUD_transport         <- merge(TUD,part_transport_data,by="newid")
  
  # add numeric boolean for 'used public transport'
  TUD_transport$trans_Y <- as.numeric(TUD_transport$public_transport_bus=="Y"|TUD_transport$public_transport_train=="Y")
  TUD_transport$trans_Y[is.na(TUD_transport$trans_Y)] <- 0
  
  get_TUD_location(TUD,'transport','transport',TUD_transport$trans_Y)
  #table(get(load('tij_transport_ind_MI1_1.RData')) == get(load('output/time_use/tij_transport_ind_MI1_1.RData')))
  
  # TUD_transport<-TUD_trans[,-c(23:59)]
  # 
  # for(i in t1){
  #   TUD_transport[,i]<-as.character(TUD_transport[,i])
  #   TUD_transport[,i][TUD_transport[,i]=="nonfilled"]<-NA
  #   TUD_transport[,i][TUD_transport[,i]!="transport"]<-0
  #   TUD_transport[,i][TUD_transport[,i]=="transport"]<-1
  # }
  # 
  # for(i in t2){
  #   TUD_transport[,i]<-as.character(TUD_transport[,i])
  #   TUD_transport[,i][TUD_transport[,i]=="nonfilled"]<-NA
  #   TUD_transport[,i][TUD_transport[,i]!="transport"]<-0
  #   TUD_transport[,i][TUD_transport[,i]=="transport"]<-2
  # }
  # 
  # for(i in t3){
  #   TUD_transport[,i]<-as.character(TUD_transport[,i])
  #   TUD_transport[,i][TUD_transport[,i]=="nonfilled"]<-NA
  #   TUD_transport[,i][TUD_transport[,i]!="transport"]<-0
  #   TUD_transport[,i][TUD_transport[,i]=="transport"]<-3
  # }
  # 
  # for (i in 6:22){
  #   TUD_transport[,i]<-as.numeric(TUD_transport[,i])
  # }
  # 
  # 
  # TUD_transport$'[0,3)'<-0
  # TUD_transport$'[3,6)'<-0
  # TUD_transport$'[6,12)'<-0
  # TUD_transport$'[12,18)'<-0
  # TUD_transport$'[18,25)'<-0
  # TUD_transport$'[25,30)'<-0
  # TUD_transport$'[30,35)'<-0
  # TUD_transport$'[35,40)'<-0
  # TUD_transport$'[40,45)'<-0
  # TUD_transport$'[45,50)'<-0
  # TUD_transport$'[50,55)'<-0
  # TUD_transport$'[55,60)'<-0
  # TUD_transport$'[60,65)'<-0
  # TUD_transport$'[65,70)'<-0
  # TUD_transport$'[70,75)'<-0
  # TUD_transport$'[75,80)'<-0
  # TUD_transport$'[80,100)'<-0
  # 
  # 
  # TUD_transport$trans_Y[TUD_transport$public_transport_bus=="Y"|TUD_transport$public_transport_train=="Y"] <-1
# 
#   func_trans<-function(x,y,z,k){
#     for (j in 1:17){
#       if(dim(table(y[k==1],z[k==1]))[2]==2)
#       {x[,29][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y[k==1],z[k==1])[1,2]/sum(table(y[k==1],z[k==1])[,2]))
#       x[,30][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y[k==1],z[k==1])[2,2]/sum(table(y[k==1],z[k==1])[,2]))
#       x[,31][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y[k==1],z[k==1])[3,2]/sum(table(y[k==1],z[k==1])[,2]))
#       x[,32][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y[k==1],z[k==1])[4,2]/sum(table(y[k==1],z[k==1])[,2]))
#       x[,33][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y[k==1],z[k==1])[5,2]/sum(table(y[k==1],z[k==1])[,2]))
#       x[,34][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y[k==1],z[k==1])[6,2]/sum(table(y[k==1],z[k==1])[,2]))
#       x[,35][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y[k==1],z[k==1])[7,2]/sum(table(y[k==1],z[k==1])[,2]))
#       x[,36][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y[k==1],z[k==1])[8,2]/sum(table(y[k==1],z[k==1])[,2]))
#       x[,37][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y[k==1],z[k==1])[9,2]/sum(table(y[k==1],z[k==1])[,2]))
#       x[,38][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y[k==1],z[k==1])[10,2]/sum(table(y[k==1],z[k==1])[,2]))
#       x[,39][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y[k==1],z[k==1])[11,2]/sum(table(y[k==1],z[k==1])[,2]))
#       x[,40][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y[k==1],z[k==1])[12,2]/sum(table(y[k==1],z[k==1])[,2]))
#       x[,41][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y[k==1],z[k==1])[13,2]/sum(table(y[k==1],z[k==1])[,2]))
#       x[,42][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y[k==1],z[k==1])[14,2]/sum(table(y[k==1],z[k==1])[,2]))
#       x[,43][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y[k==1],z[k==1])[15,2]/sum(table(y[k==1],z[k==1])[,2]))
#       x[,44][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y[k==1],z[k==1])[16,2]/sum(table(y[k==1],z[k==1])[,2]))
#       x[,45][y==levels(y)[j]] <- z[y==levels(y)[j]]* (table(y[k==1],z[k==1])[17,2]/sum(table(y[k==1],z[k==1])[,2]))
#       }
#     }
#     return(x)
#   }
# 
# 
#   transport_time_slot1 <-func_trans(TUD_transport,TUD_transport$age_cat,TUD_transport$`05-08h`,TUD_transport$trans_Y)[,c(25,29:45)]
#   transport_time_slot2 <-func_trans(TUD_transport,TUD_transport$age_cat,TUD_transport$`08-09h`,TUD_transport$trans_Y)[,c(25,29:45)]
#   transport_time_slot3 <-func_trans(TUD_transport,TUD_transport$age_cat,TUD_transport$`09-10h`,TUD_transport$trans_Y)[,c(25,29:45)]
#   transport_time_slot4 <-func_trans(TUD_transport,TUD_transport$age_cat,TUD_transport$`10-11h`,TUD_transport$trans_Y)[,c(25,29:45)]
#   transport_time_slot5 <-func_trans(TUD_transport,TUD_transport$age_cat,TUD_transport$`11-12h`,TUD_transport$trans_Y)[,c(25,29:45)]
#   transport_time_slot6 <-func_trans(TUD_transport,TUD_transport$age_cat,TUD_transport$`12-13h`,TUD_transport$trans_Y)[,c(25,29:45)]
#   transport_time_slot7 <-func_trans(TUD_transport,TUD_transport$age_cat,TUD_transport$`13-14h`,TUD_transport$trans_Y)[,c(25,29:45)]
#   transport_time_slot8 <-func_trans(TUD_transport,TUD_transport$age_cat,TUD_transport$`14-15h`,TUD_transport$trans_Y)[,c(25,29:45)]
#   transport_time_slot9 <-func_trans(TUD_transport,TUD_transport$age_cat,TUD_transport$`15-16h`,TUD_transport$trans_Y)[,c(25,29:45)]
#   transport_time_slot10 <-func_trans(TUD_transport,TUD_transport$age_cat,TUD_transport$`16-17h`,TUD_transport$trans_Y)[,c(25,29:45)]
#   transport_time_slot11 <-func_trans(TUD_transport,TUD_transport$age_cat,TUD_transport$`17-18h`,TUD_transport$trans_Y)[,c(25,29:45)]
#   transport_time_slot12 <-func_trans(TUD_transport,TUD_transport$age_cat,TUD_transport$`18-19h`,TUD_transport$trans_Y)[,c(25,29:45)]
#   transport_time_slot13 <-func_trans(TUD_transport,TUD_transport$age_cat,TUD_transport$`19-20h`,TUD_transport$trans_Y)[,c(25,29:45)]
#   transport_time_slot14 <-func_trans(TUD_transport,TUD_transport$age_cat,TUD_transport$`19-20h`,TUD_transport$trans_Y)[,c(25,29:45)] #LW: index-error
#   transport_time_slot15 <-func_trans(TUD_transport,TUD_transport$age_cat,TUD_transport$`19-20h`,TUD_transport$trans_Y)[,c(25,29:45)] #LW: index-error
#   transport_time_slot16 <-func_trans(TUD_transport,TUD_transport$age_cat,TUD_transport$`19-20h`,TUD_transport$trans_Y)[,c(25,29:45)] #LW: index-error
#   transport_time_slot17 <-func_trans(TUD_transport,TUD_transport$age_cat,TUD_transport$`19-20h`,TUD_transport$trans_Y)[,c(25,29:45)] #LW: index-error
# 
# 
#   #####individual time-exposure at transport places###
#   tij_transport_ind<-transport_time_slot1
#   tij_transport_ind[,2:18]<-NULL
# 
#   for(i in 2:18){
#     tij_transport_ind[i]<-transport_time_slot1[i] +transport_time_slot2[i] +transport_time_slot3[i]+transport_time_slot4[i]+transport_time_slot5[i]+transport_time_slot6[i]+transport_time_slot7[i]+transport_time_slot8[i]+transport_time_slot9[i]+
#       transport_time_slot10[i]+transport_time_slot11[i]+transport_time_slot12[i]+transport_time_slot13[i]+transport_time_slot14[i]+transport_time_slot15[i]+transport_time_slot16[i]+transport_time_slot17[i]
#   }
# 
#   Output8 = paste0("tij_transport_ind_MI1_",boot, ".RData")
#   save(tij_transport_ind, file = Output8)


  # #######################################################################
  # ##############ALL. GENERAL TIME-EXPOSURE MATRIX#############################
  # ########################################################################## 
  # 
  # tij_general_ind<-tij_home_ind
  # tij_general_ind[,2:18]<-NULL
  # 
  # for (i in 2: 18){
  #   tij_general_ind[i]<-  tij_home_ind[i] + tij_work_ind[i] + tij_kinder_ind[i] + tij_school_ind[i] + tij_leisure_ind[i] + tij_other_ind[i]+ tij_family_ind[i]+ tij_transport_ind[i]
  # }
  # Output9 = paste0("tij_general_ind_MI1_",boot, ".RData")
  # save(tij_general_ind, file = Output9) 
  
}

print(system.time(out1_1 <- boot_TUD(1,1)))

