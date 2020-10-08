comp_func<-function(part){
  
  ###Create TUD###
  var<-c("local_id","participant_age","participant_gender","week","holiday", paste0("time_use_location_",1:17),"hh_size","diary_weight")
  TUD<-part[var]
  
  ###Recode multiple variables in R
  # select columns: time slot
  num_time_slots             <- 17
  colnames_time_use_location <- paste0("time_use_location_",1:num_time_slots)
  
  # set time slot sizes
  t1 <- colnames_time_use_location[2:13]  ### time slots of 1 hour
  t2 <- colnames_time_use_location[14:16] ### time slots of 2 hours
  t3 <- colnames_time_use_location[c(1,17)]  ### time slots of 3 hours
  
  
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
  
  ##check
  TUD$total<-TUD$home + TUD$kinder_garten+ TUD$school + TUD$workplace + TUD$transport + TUD$family + TUD$leisure + TUD$other
  
  TUD$home_perc<-round((TUD$home/TUD$total)*100,digits=2)
  TUD$kinder_garten_perc<-round((TUD$kinder_garten/TUD$total)*100,digits=2)
  TUD$school_perc<-round((TUD$school/TUD$total)*100,digits=2)
  TUD$workplace_perc<-round((TUD$workplace/TUD$total)*100,digits=2)
  TUD$transport_perc<-round((TUD$transport/TUD$total)*100,digits=2)
  TUD$family_perc<-round((TUD$family/TUD$total)*100,digits=2)
  TUD$leisure_perc<-round((TUD$leisure/TUD$total)*100,digits=2)
  TUD$other_perc<-round((TUD$other/TUD$total)*100,digits=2)
  
  TUD$total_perc <- TUD$home_perc + TUD$kinder_garten_perc + TUD$school_perc + TUD$workplace_perc + TUD$transport_perc +
    TUD$family_perc + TUD$leisure_perc + TUD$other_perc
  
  ###Age###
  TUD$age_cat<-cut(TUD$participant_age,breaks = c(0,3,6,12,18,25,45,65,100),right = FALSE)
  
  #####PREPAE FOR MODEL####
  TUD$home_dep<-TUD$home_perc
  TUD$school_dep<-TUD$school_perc + TUD$kinder_garten_perc
  TUD$work_dep <- TUD$workplace_perc
  TUD$transport_dep<- TUD$transport_perc
  TUD$other_dep <- TUD$leisure_perc + TUD$other_perc + TUD$family_perc
  
  data1<-TUD[c("local_id","participant_gender","participant_age","age_cat","hh_size","week","holiday","home_dep","school_dep","work_dep","transport_dep","other_dep","diary_weight")]
  names(data1)
  
  data1$home_dep <-data1$home_dep/100
  data1$school_dep<-data1$school_dep/100
  data1$work_dep <-data1$work_dep/100
  data1$transport_dep <-data1$transport_dep/100
  data1$other_dep <- data1$other_dep/100
  
  
  data1$home_dep<- round(data1$home_dep,digits = 7)
  data1$school_dep<- round(data1$school_dep,digits = 7)
  data1$work_dep<- round(data1$work_dep,digits = 7)
  data1$transport_dep<- round(data1$transport_dep,digits = 7)
  data1$other_dep<- round(data1$other_dep,digits = 7)
  
  return(data1)
}