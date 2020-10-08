#############################################################################
# SOCIAL CONTACT DATA ANALYSIS
#
# Copyright 2019, SIMID
#############################################################################
#
# ADD DIARY WEIGHTS TO IMPUTED DATASETS
#
#############################################################################


rm(list=ls(all=TRUE))
setwd("C:\\Users\\lucp9032\\Desktop\\TIME USE FOLDER\\CODE_MI_BOOSTRAP\\THANG")

require(devtools)
devtools::install_github("lwillem/simid_rtools",force=F,quiet=T)
#devtools::uninstall(simid.rtools)
library('simid.rtools')


# input/output folder
data_folder    <- smd_file_path('data')
MI_data_folder <- smd_file_path('output','imputation')


weight_func<-function(MI_data){
  #===============================================================================
  FBPopHSAge <- as.matrix(read.table(file.path(data_folder,"FBPopHSAge.txt"),header=F, sep=" ")) # Both regions 
  PopSize <- as.vector(colSums(FBPopHSAge))# Pop sizes across each age groups
  
  #==================================
  ########## ii) Diary weights for the 2010 contact survey data 
  #==================================
  # Create agecat variable for participants data form 0:100 in a sequence of 5
  MI_data$agecat <- cut(MI_data$participant_age, breaks=seq(0,100,5),
                        right=FALSE, include.lowest=TRUE)# include.lowest is the way to put open bracket or not at the last class
  agelev2010 <- levels(MI_data$agecat)
  
  # Create HScat variable for participants data form 1:11 in sequence of 1 and 20 (assuming 20 is a possible maximum household size)
  MI_data$HScat <- cut(MI_data$hh_size, breaks=c(seq(1,11,1), 20),
                       right=FALSE, include.lowest=TRUE)
  HSlev2010 <- levels(MI_data$HScat)
  
  # pfraction = Cell counts of FBPopHSAge/total population size
  for(j in 1:dim(FBPopHSAge)[2]){
    for(k in 1:dim(FBPopHSAge)[1]){
      MI_data$pfraction[MI_data$agecat==agelev2010[j] &
                          MI_data$HScat==HSlev2010[k]] <-
        FBPopHSAge[k,j]/sum(PopSize)
    }
  }
  
  # Creating HHsize by Age table of the participant data
  BF2010_HSAgeSamp <- table(MI_data$HScat, MI_data$agecat)
  SampSize2010 <- as.vector(colSums(BF2010_HSAgeSamp)) # Size acrosss age groups
  
  # sfraction= cell counts of BF_2011_HH_Age_SAMP/total sample size
  for(s in 1:dim(BF2010_HSAgeSamp)[2]) {
    for(t in 1:dim(BF2010_HSAgeSamp)[1]) {
      MI_data$sfraction[MI_data$agecat==agelev2010[s] &
                          MI_data$HScat==HSlev2010[t]] <-
        BF2010_HSAgeSamp[t,s]/sum(SampSize2010)
    }
  }
  
  # Assigning crude weights = pfraction/sfraction except HSize=NA
  MI_data$diary_weight <- MI_data$pfraction/MI_data$sfraction
  
  # Assigning crude weights = pop density across age groups / sample density across age groups for Hsize=NA 
  mar_weight2010 <- (PopSize/sum(PopSize))/(SampSize2010/sum(SampSize2010))
  for(j in 1:dim(BF2010_HSAgeSamp)[2]){
    MI_data$diary_weight[MI_data$agecat==agelev2010[j] &
                           is.na(MI_data$HScat)] <- mar_weight2010[j]
  }
  
  # Standardizing diary weights
  MI_data$diary_weight <- (MI_data$diary_weight*length(
    MI_data$diary_weight))/sum(MI_data$diary_weight)
  
  #=================================================================================
  #>>>>> Assessing weights before and after truncation 2010
  #=================================================================================
  ## Checking weights above 3 before truncation
  #### Truncating weights 
  MI_data$diary_weight[MI_data$diary_weight >=3] = 3 
  
  #### Standardizing diary weights
  MI_data$diary_weight <- (MI_data$diary_weight*length(
    MI_data$diary_weight))/sum(MI_data$diary_weight)
  
  #=================================================================================
  #>>>>> Variables to be excluded
  #=================================================================================
  remo <- c("agecat", "HScat", "pfraction", "sfraction")# Vars to exclude 
  MI_data <- MI_data[, !(colnames(MI_data) %in% remo)]
  #=================================================================================
  return(MI_data)  
}

MI_data<-get(load(file.path(MI_data_folder,"MI_data.RData")))

no_imputation<-10
MI_data_weight<-NULL
for (imputation_index in 1:no_imputation){
  MI_data_weight[[imputation_index]]<-weight_func(MI_data[MI_data$imputation_index==imputation_index,])
  
}
save.image(file=file.path(MI_data_folder,"MI_data_weight"))


