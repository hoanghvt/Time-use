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
setwd("C:\\Users\\lucp9032\\Desktop\\TIME USE FOLDER\\CODE_MI_BOOSTRAP\\THANG")

require(devtools)
devtools::install_github("lwillem/simid_rtools",force=F,quiet=F)
library('simid.rtools')

packages_list <- c('car',     # 'combine' function
                   'tidyr',   # 'gather' function
                   'sqldf')   # 'sql' join operation

# load packages (and install if not present)
smd_load_packages(packages_list)  


########################################################
###  SETTINGS                                        ###
########################################################

# number of bootstraps
# TODO: just count files in bootstrap folder?
num_bootstraps <- 1000

# number of imputations
# TODO: check results of one bootstrap...
num_imputations <- 10

# random number engine seed
rng_seed <- 1235

# input/output folder
ref_data_folder        <- smd_file_path('data')
bootstrap_data_folder  <- smd_file_path('output','bootstrap')
output_folder          <- smd_file_path('output','time_use')

# age categories
ref_agecat <- c(0, 3, 6, 12,18,25,30,35,40,45,50,55,60,65,70,75,80,100)

#imputation_index <- 1
#bootstrap_index <- 1
boot_TUD  <- function(imputation_index, bootstrap_index){

  BootData_imputation <- get(load(file=smd_file_path(bootstrap_data_folder,paste0("MI_data_bootstrap",bootstrap_index,".RData")))) 
  part                <- BootData_imputation[BootData_imputation$imputation_index == imputation_index,]
  
  # add new id varaible, to account for dupplicated participants by the bootstrap
  part$newid<-c(1:length(part[,1]))
  
  # select columns: time slot
  num_time_slots             <- 17
  colnames_time_use_location <- paste0("time_use_location_",1:num_time_slots)
  
  # set time slot sizes
  t1 <- colnames_time_use_location[2:13]  ### time slots of 1 hour
  t2 <- colnames_time_use_location[14:16] ### time slots of 2 hours
  t3 <- colnames_time_use_location[1:17]  ### time slots of 3 hours

  # set hh info columns  
  max_hhsize                 <- 12
  colnames_hh_gender         <- paste0("hh_member_gender_",1:max_hhsize)
  colnames_hh_age            <- paste0("hh_member_age_",1:max_hhsize)
  colnames_hh_present        <- paste0("hh_member_present_",1:max_hhsize)
  
  #### use newid, instead of local_id which is dupplicated in boostrap dataset#####
  var<-c("newid","participant_age","participant_gender","holiday", 
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
           4='workplace';5='transport';6='family';7='leisure';8='other'"))
  
  # Set as character
  for(i in colnames_time_use_location){
    TUD[,i] <- as.character(TUD[,i])
  }
  
  # #TODO: remove extra columns?
  # # add columns with time use slot (without remove the original columns)
  # time_use_slots <- c("05-08h","08-09h","09-10h","10-11h","11-12h","12-13h","13-14h",
  #                     "14-15h","15-16h","16-17h","17-18h","18-19h","19-20h","20-22h",
  #                     "22-24h","24-02h","02-05h")
  # TUD[,time_use_slots] <- TUD[,colnames_time_use_location]
   
  
  # use 'version'from "part" data
  # use 'age_cat' from "part" data
  
  # get factor
  TUD$factor  <- TUD$age_cat:factor(TUD$holiday):factor(TUD$week)
  
  ################################################
  #### GENERIC HELP FUNCTIONS               ######
  ################################################
  
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
  
  # get exposure at a given location, for a given age category (y)
  #   - x   TUD
  #   - y   age_categories
  #   - z   time present at given time_slot (0, 1, 2 or 3h)
  #   - k   conditional? [optional, 'NULL' by default]
  #
  # note: based on 'func_work'
  x <- TUD_location
  y <- TUD_location$age_cat
  z <- TUD_location[,colnames_time_use_location[1]]
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
      frac_age_location <- tbl_age_location[,1] * 0
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
  

  # get TUD at school: special case!  
  #Based on education system to calculate time-exposure of respondents. 
  # Particularly, children in age 6-12 will have time-exposure with children in the 
  # same age for full time-slot, and time-exposure with people older than 25 
  # proportional to their participantion in that time slot. children in age 6-12 
  # will not have time-exposure with other school-age children, e.g. 0-3, 12-18...
  #
  #   - x   TUD
  #   - y   age_categories
  #   - z   time present at given time_slot (0, 1, 2 or 3h)
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
  
  # get TUD for a given location
   location_tag <- 'kinder-garten'
   file_tag <- 'kinder'
  get_TUD_location <- function(TUD,location_tag,file_tag, boolean_condition = NULL){
    
    # convert TUD with all info (home, work, school,...) into 0/1 per time slot for the given location tag
    TUD_location  <- get_time_at_location(TUD,location_tag)
    
    # get age categories
    age_cat <- levels(TUD_location$age_cat)
    
    i_time <- 1
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
  
  
  #######################################################################
  ##############A. TIME-EXPOSURE MATRIX AT HOME#############################
  ##########################################################################
  
  ##Exclude participants from hhs member####
  ##2 Criteria for exclusion: the same age and the same gender###
  
  # LW: execute per column
  for(i_col in colnames_hh_gender){
    TUD[,i_col] <- as.character(TUD[,i_col])
  }
  
  # LW: use column name instead of index
  TUD$participant_gender <- as.character(TUD$participant_gender)
  
  # LW: run per participant... and use all hh data at once
  colnames_hh_age <- paste0("hh_member_age_",1:max_hhsize)
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

  ####compare time at home of participants and time_at home of member
  ####if time-at-home of participants >time-at-home of member --> take time-at-home of member
  ####if time-at-home of participants <=time-at-home of member --> take time-at-home of paricipants
  
  func_home<-function(x,y,z){
    for (i in 1: length(z[,1])){
      if (!is.na(y[i]) & (y[i]<= x[i]) & !is.na(x[i])){x[i]=y[i]}
      if (!is.na(y[i]) & (y[i]>= x[i]) & !is.na(x[i])){x[i]=x[i]}
      if (is.na(y[i])) {x[i]<-NA}
    }  
    return(x)}
  
  func_na<-function(x){
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
    member[,colnames_time_use_location_x[i_time]] <- func_home(member[,colnames_time_use_location_x[i_time]],
                                                               member[,colnames_time_use_location[i_time]],
                                                               member)
  }
  
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
  
  # remove redundant columns
  tij_home_ind<-tij_home_ind[,-c(1,2)]
  
  # save result
  Output1 = paste0("tij_home_ind_MI",imputation_index,"_",bootstrap_index, ".RData")
  save(tij_home_ind, file = smd_file_path(output_folder,Output1))
  
  
  #######################################################################
  ##############B. TIME-EXPOSURE MATRIX AT WORK#############################
  ##########################################################################
  
  # get TUD at workplace
  tij_work_ind <- get_TUD_location(TUD,'workplace','work')
  
  
  #######################################################################
  ##############c. TIME-EXPOSURE MATRIX AT KINDER-GARTEN#############################
  ##########################################################################
  
  # get TUD at 'kinder-garten'
  tij_kinder_ind <- get_TUD_location(TUD,'kinder-garten','kinder')
  
  
  #######################################################################
  ##############D. TIME-EXPOSURE MATRIX AT SCHOOL #############################
  ##########################################################################
  
  TUD_location <- get_time_at_location(TUD,'school')
  
  age_cat <- levels(TUD_location$age_cat)
  for(i_time in 1:num_time_slots){
    
    # use func_school!
    location_time_slot_x <- func_school(TUD_location,TUD_location$age_cat,TUD_location[,colnames_time_use_location[i_time]])[,c('age_cat',age_cat)]
    
    #print(table(location_time_slot_x == get(paste0('leisure_time_slot',i_time))))
    columns_age_cat <- grepl('\\[',names(location_time_slot_x))
    if(i_time ==1){
      tij_school_ind <- location_time_slot_x
      tij_school_ind[,columns_age_cat] <- 0
    }
    
    tij_school_ind[,columns_age_cat] <- tij_school_ind[,columns_age_cat] + location_time_slot_x[,columns_age_cat]
  }
  
  output_filename = paste0("tij_school_ind_MI",imputation_index,"_",bootstrap_index, ".RData")
  save(tij_school_ind, file = smd_file_path(output_folder,output_filename))

  
  #######################################################################
  ##############E. TIME-EXPOSURE MATRIX AT LEISURE#############################
  ##########################################################################
  
  # get TUD at 'leisure
  tij_leisure_ind <- get_TUD_location(TUD,'leisure','leisure')
  
  
  
  #######################################################################
  ##############F. TIME-EXPOSURE MATRIX AT OTHER PLACES#############################
  ##########################################################################
  
  # get TUD at 'other'
  tij_other_ind <- get_TUD_location(TUD,'other','other')
  
  
  #######################################################################
  ##############G. TIME-EXPOSURE MATRIX AT FAMILY#############################
  ##########################################################################
  
  # get TUD at 'family'
  tij_family_ind <- get_TUD_location(TUD,'family','family')
  
  
  #######################################################################
  ##############H. TIME-EXPOSURE MATRIX AT TRANSPORT#############################
  ##########################################################################
  
  # add transport data to TUD variable
  part_transport_data   <- part[c("newid","public_transport_bus","public_transport_train")]
  TUD_transport         <- merge(TUD,part_transport_data,by="newid")
  
  # add numeric boolean for 'used public transport'
  TUD_transport$trans_Y <- as.numeric(TUD_transport$public_transport_bus=="Y"|TUD_transport$public_transport_train=="Y")
  TUD_transport$trans_Y[is.na(TUD_transport$trans_Y)] <- 0
  
  # process conditional TUD
  tij_transport_ind <- get_TUD_location(TUD,'transport','transport',TUD_transport$trans_Y)
  

  # #######################################################################
  # ##############ALL. GENERAL TIME-EXPOSURE MATRIX#############################
  # ########################################################################## 
   
  # get aggregated 'tij' data
  tij_general_ind  <- tij_home_ind
  tij_general_ind[,age_cat] <- 0
  
  tij_general_ind[,age_cat] <-  tij_home_ind[,age_cat] +
                                tij_work_ind[,age_cat] +   
                                tij_kinder_ind[,age_cat] +
                                tij_school_ind[,age_cat] +
                                tij_leisure_ind[,age_cat] +
                                tij_other_ind[,age_cat] +
                                tij_family_ind[,age_cat] +
                                tij_transport_ind[,age_cat]

  output_filename <- paste0("tij_general_ind_MI",imputation_index,"_",bootstrap_index, ".RData")
  save(tij_general_ind, file = smd_file_path(output_folder,output_filename)) 
  
  # return general tij
  return(tij_general_ind)
  
}

# run function for imputed dataset 1 and bootstrap 1
#out1_1 <- boot_TUD(1,1)


#TODO: combine imputed datasets and bootstraps...
#============================ PARALELL PROCESSING =====================================================

# start parallel working nodes
# note: make sure you loaded all required user-defined functions at this point
par_nodes_info <- smd_start_cluster()

# run all bootstraps in parallel
smd_print('BOOTSTRAP...'); time_stamp <- Sys.time()

Sys.time()
# parallel loop ==>> for each bootstrap
foreach(i_bootstrap= 1:num_bootstraps,            # iteration index
        .combine    = rbind,                      # combine the output by row
        .packages   = 'simid.rtools')  %dopar%    # pass package and run in parallel
  
  {
    # print progress
    simid.rtools::smd_progress(i_bootstrap,num_bootstraps,time_stamp,par_nodes_info)
    
    # sequential loop => for each imputed dataset (to reduce the number of output files...)
    foreach(i_imputation = 1:num_imputations,    # iteration index
            .combine     = rbind,                # combine the output by row
            .packages    = c('simid.rtools',packages_list))  %do% # transfer packages and run sequential     
      {
        # run bootstrap function
        BootData <- boot_TUD(i_imputation,i_bootstrap)
        
        # return bootstrap data
        #cbind(BootData,bootstrap_index,imputation_index)
      } ->  BootData_imputation  # save result in BootData_imputation
    
    # save results per bootstrap
    #filename = paste0("MI_tij_general_bootstrap",i_bootstrap, ".RData")
    #save(BootData_imputation, file = smd_file_path(output_folder,filename))
    
    # return dummy variable for parallel loop
    1
  } -> foreach_out  # aggregate all dummy variables from the parallel foreach

# terminate parallel working nodes
smd_stop_cluster()

# command line statement
smd_print("BOOTSTRAP READY")
Sys.time()


