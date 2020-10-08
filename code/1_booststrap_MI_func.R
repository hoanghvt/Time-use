#############################################################################
# SOCIAL CONTACT DATA ANALYSIS
#
# Copyright 2019, SIMID
#############################################################################
#
# BOOTSTRAP MULTIPLE IMPUTATION DATA
#
#############################################################################

# clear global environment
rm(list = ls())
setwd("C:\\Users\\lucp9032\\Desktop\\TIME USE FOLDER\\CODE_MI_BOOSTRAP\\THANG")


require(devtools)
devtools::install_github("lwillem/simid_rtools",force=F,quiet=F)
#devtools::uninstall(simid.rtools)
library('simid.rtools')


########################################################
###  SETTINGS                                        ###
########################################################

# number of bootstraps
num_bootstraps <- 1000

# random number engine seed
rng_seed <- 1235

# input/output folder
ref_data_folder    <- smd_file_path('data')
cnt_data_folder    <- smd_file_path('output','imputation')
output_folder      <- smd_file_path('output','bootstrap')


########################################################
###  LOAD DATA AND SET RNG SEEd                      ###
########################################################

# load imputed data
MI_data<-get(load(file.path(cnt_data_folder,"MI_data.RData")))

# number of imputations
num_imputations <- max(MI_data$imputation_index)

# reference age categories
ref_agecat <- seq(0,100,5)

# reference hh size categories
ref_hhsize <- c(seq(1,11,1), 20)

# set rng seed (only once!)
set.seed(rng_seed)

########################################################
### FUNCTION                                         ###
########################################################


StratifiedBoot <- function(Data, Group, Seed, Replace){
  # Data: is a dataframe to be re-sampled
  # Group: is a factor variable used as strata
  # Seed: is a random seed number to reproduce the data again
  # Replace: is a TRUE/FALSE option to make sampling with or without replacement
  #set.seed(Seed)  
  GroupLevels <- levels(Data[,Group])
  GroupSizes  <- as.vector(table(Data[,Group]))
  BootData <- NULL
  for(i in 1:length(GroupLevels)){
    SubData <- subset(Data, Data[,Group]==GroupLevels[i])
    Indices <- sample(1:nrow(SubData), size=GroupSizes[i], replace=Replace)
    BootData <- rbind.data.frame(BootData, SubData[Indices, ])
  }
  return(BootData)
}


#imputation_index <- 1
#bootstrap_index <- 1
#rng_seed <- 1
BootFunc <- function(imputation_index,bootstrap_index){ 
  
  # seed random number engine
  #set.seed(rng_seed)
  
  # select data  
  ImputedData <- MI_data[MI_data$imputation_index == imputation_index,]
  
  # bootstrap
  BootData  <- StratifiedBoot(Data=ImputedData,Group="age_cat",
                                 Seed=rng_seed, Replace=TRUE)   #LW: don't seed the RNG multiple times with the same value
  #===============================================================================
  #>>>> Diary weights calculation
  #>> Population size data stratified by age and hhsize for brussels and flanders
  #===============================================================================
  # HH-Size: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11+
  # Age: 0-4, 5-9, 10-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54,
  # 55-59, 60-64, 65-69, 70-74, 75-79, 80-84, 85-89, 90-94, 95+
  # BRUS_PopSize_HSAge: The matrix of populatiuon size of Brussels region stratified
  # by age and household size
  # FlaPopHSAge: The matrix of populatiuon size of Flanders region stratified by 
  # age and household size
  #===============================================================================
  # population data
  FBPopHSAge <- as.matrix(read.table(file.path(ref_data_folder,"FBPopHSAge.txt"),header=F, sep=" ")) # Both regions 
  PopSize    <- as.vector(colSums(FBPopHSAge))# Pop sizes across each age groups

    #==================================
  ########## ii) Diary weights for the 2010 contact survey data 
  #==================================
  # Create agecat variable for participants data form 0:100 in a sequence of 5
  BootData$agecat <- cut(BootData$participant_age, breaks=seq(0,100,5),
                          right=FALSE, include.lowest=TRUE)# include.lowest is the way to put open bracket or not at the last class
  agelev2010 <- levels(BootData$agecat)
  
  # Create HScat variable for participants data form 1:11 in sequence of 1 and 20 (assuming 20 is a possible maximum household size)
  BootData$HScat <- cut(BootData$hh_size, breaks=c(seq(1,11,1), 20),
                         right=FALSE, include.lowest=TRUE)
  HSlev2010 <- levels(BootData$HScat)
  
  # pfraction = Cell counts of FBPopHSAge/total population size
  for(j in 1:dim(FBPopHSAge)[2]){
    for(k in 1:dim(FBPopHSAge)[1]){
      BootData$pfraction[BootData$agecat==agelev2010[j] &
                            BootData$HScat==HSlev2010[k]] <-  FBPopHSAge[k,j]/sum(PopSize)
    }
  }
  
  # Creating HHsize by Age table of the participant data
  BF2010_HSAgeSamp <- table(BootData$HScat, BootData$agecat)
  SampSize2010     <- as.vector(colSums(BF2010_HSAgeSamp)) # Size acrosss age groups
  
  # sfraction= cell counts of BF_2011_HH_Age_SAMP/total sample size
  for(s in 1:dim(BF2010_HSAgeSamp)[2]) {
    for(t in 1:dim(BF2010_HSAgeSamp)[1]) {
      BootData$sfraction[BootData$agecat==agelev2010[s] &
                            BootData$HScat==HSlev2010[t]] <-
        BF2010_HSAgeSamp[t,s]/sum(SampSize2010)
    }
  }
  
  # Assigning crude weights = pfraction/sfraction except HSize=NA
  BootData$diary_weight <- BootData$pfraction/BootData$sfraction
  
  # Assigning crude weights = pop density across age groups / sample density across age groups for Hsize=NA 
  mar_weight2010 <- (PopSize/sum(PopSize))/(SampSize2010/sum(SampSize2010))
  for(j in 1:dim(BF2010_HSAgeSamp)[2]){
    BootData$diary_weight[BootData$agecat==agelev2010[j] &
                             is.na(BootData$HScat)] <- mar_weight2010[j]
  }
  
  # Standardizing diary weights
  BootData$diary_weight <- (BootData$diary_weight*length(
    BootData$diary_weight))/sum(BootData$diary_weight)
  
  #=================================================================================
  #>>>>> Assessing weights before and after truncation 2010
  #=================================================================================
  ## Checking weights above 3 before truncation
  #### Truncating weights 
  BootData$diary_weight[BootData$diary_weight >=3] = 3 
  
  #### Standardizing diary weights
  BootData$diary_weight <- (BootData$diary_weight*length(
    BootData$diary_weight))/sum(BootData$diary_weight)
  
  #=================================================================================
  #>>>>> Variables to be excluded
  #=================================================================================
  remo <- c("agecat", "HScat", "pfraction", "sfraction")# Vars to exclude 
  BootData <- BootData[, !(colnames(BootData) %in% remo)]
  #=================================================================================
  
  # LW: don't save each bootstrap seperatly
  #filename = paste0("MI",imputation_index,"_",bootstrap_index, ".RData")
  #save(BootData, file = smd_file_path(output_folder,filename))
  
  # add bootstrap index
  BootData$bootstrap_index <- bootstrap_index
  
  # return
  BootData
      
}

#============================ PARALELL PROCESSING =====================================================


# start parallel working nodes
# note: make sure you loaded all required user-defined functions at this point
par_nodes_info <- smd_start_cluster()

# run all bootstraps in parallel
smd_print('BOOTSTRAP...'); time_stamp <- Sys.time()

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
           .packages    = 'simid.rtools')  %do% # transfer packages and run sequential     
    {
      # run bootstrap function
      BootData <- BootFunc(i_imputation,i_bootstrap)
      
      # return bootstrap data
      BootData
    } ->  BootData_imputation  # save result in BootData_imputation
    
    # save results per bootstrap
    filename = paste0("MI_data_bootstrap",i_bootstrap, ".RData")
    save(BootData_imputation, file = smd_file_path(output_folder,filename))
                
    # return dummy variable for parallel loop
    1
} -> foreach_out  # aggregate all dummy variables from the parallel foreach

# terminate parallel working nodes
smd_stop_cluster()

# command line statement
smd_print("BOOTSTRAP READY")

