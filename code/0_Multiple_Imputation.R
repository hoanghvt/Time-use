#############################################################################
# SOCIAL CONTACT DATA ANALYSIS
#
# Copyright 2019, SIMID
#############################################################################
#
# MULTIVARIATE IMPUTATION BY CHAINED EQUATIONS
#
#############################################################################


rm(list=ls(all=TRUE))
setwd("C:\\Users\\lucp9032\\Desktop\\TIME USE FOLDER\\CODE_MI_BOOSTRAP\\THANG")

require(devtools)
devtools::install_github("lwillem/simid_rtools",force=F,quiet=T)
#devtools::uninstall(simid.rtools)
library('simid.rtools')

# load (and install) package
smd_load_packages('mice')



########################################################
###  SETTINGS                                        ###
########################################################

# number of bootstraps
num_imputations <- 10

# max number of iterations
max_iterations  <- 5

# random number engine seed
rng_seed        <- 133

# input/output folder
data_folder    <- smd_file_path('data','survey_flanders2010')
output_folder  <- smd_file_path('output','imputation')


########################################################
###  LOAD DATA                                       ###
########################################################

# load data
part <- read.table(file.path(data_folder,"individualsurvey_participants_full.txt"), sep=",",header = TRUE)

# age categories
ref_agecat <- c(0, 3, 6, 12,18,25,30,35,40,45,50,55,60,65,70,75,80,100)

########################################################
###  IMPUTATION                                      ###
########################################################

# select columns
max_hhsize <- 12
var<-c("local_id","participant_age","participant_gender","dayofweek","holiday", 
       paste0("time_use_location_",1:17),"hh_size",
       paste0("hh_member_age_",1:max_hhsize),
       paste0("hh_member_gender_",1:max_hhsize),
       paste0("hh_member_present_",1:max_hhsize),
       "public_transport_bus","public_transport_train")


# select individual data
TUD<-part[,var]

# add daytype: week/weekend
TUD$week<-NULL
TUD$week[TUD$dayofweek==0|TUD$dayofweek==6]<- "Weekend"
TUD$week[TUD$dayofweek==1|TUD$dayofweek==2|TUD$dayofweek==3|TUD$dayofweek==4|TUD$dayofweek==5]<- "Weekday"

# add version type
TUD$version<-NULL
TUD$version[TUD$participant_age<12]<-"Version 1"
TUD$version[TUD$participant_age>=12&TUD$participant_age<=60]<-"Version 2"
TUD$version[TUD$participant_age>60]<-"Version 3"

# add age category
TUD$age_cat <- cut(TUD$participant_age, breaks=ref_agecat,right = FALSE)
#TUD$factor  <- TUD$age_cat:factor(TUD$holiday):factor(TUD$week)

# set missing as NA
for (i in which(grepl('time_use_location',names(TUD)))){
  TUD[,i][TUD[,i]==9]<-NA
  TUD[,i]<-as.factor(TUD[,i])
}

# factorise person characteristics
TUD$age_cat            <- as.factor(TUD$age_cat)
TUD$participant_gender <- as.factor(TUD$participant_gender)
TUD$holiday            <- as.factor(TUD$holiday)
TUD$week               <- as.factor(TUD$week)

# setup MICE features
init   <- mice(TUD, maxit=0) 
meth   <- init$method
predM <-  init$predictorMatrix

###The code to remove the variable as a predictor but still will be imputed ####
###use age group, rather than age variable
###predictor variables: gender, week, holiday, age_cat: 3,62,5,64


predM[, c("local_id","participant_age","dayofweek",paste0("time_use_location_",1:17),"hh_size",
      paste0("hh_member_age_",1:max_hhsize),
      paste0("hh_member_gender_",1:max_hhsize),
      paste0("hh_member_present_",1:max_hhsize),
      "public_transport_bus","public_transport_train", "version")]=0    ###not use time slots as predictors

###These variables need not be imputed have method by using ""
meth[c("local_id","participant_age","participant_gender","dayofweek","holiday",
       paste0("hh_member_age_",1:max_hhsize),
       paste0("hh_member_gender_",1:max_hhsize),
       paste0("hh_member_present_",1:max_hhsize),
       "public_transport_bus","public_transport_train", "version","age_cat")]   <-""


#####polyreg: applys for 
meth[c(paste0("time_use_location_",1:17))] <- "polyreg"
meth[c("week")]    <- "polyreg"
meth[c("hh_size")]   <- "pmm"

# start parallel nodes
smd_start_cluster()

# run MICE (in parallel)
foreach(i_imputation = 1:num_imputations,   # iteration index
        .combine='rbind',                   # combine results by row
        .packages = c('mice')) %dopar% {    # pass packages and run in parallel
  
          # run mice
          imputed  <- mice(data            = TUD, 
                           method          = meth, 
                           predictorMatrix = predM, 
                           m               = 1, # one imputation per parallel worker
                           maxit           = max_iterations, 
                           seed            = rng_seed+i_imputation) # vary the RNG seed by imputation
  
          # reshape format and remove .imp and .m columns
          data_all <- complete(imputed, "long")
          data_all <- data_all[,!names(data_all) %in% c('.imp','.m')]
          
          # add imputation index
          data_all$imputation_index <- i_imputation
          
          # return imputed dataset
          data_all

} -> MI_data    # save all output in "MI_data"

# close parallen nodes
smd_stop_cluster()

# check MI_data
dim(MI_data)
table(MI_data$imputation_index)

# save results
save(MI_data,file= smd_file_path(output_folder,"MI_data.RData"))

# command line statement
smd_print("MICE READY")
