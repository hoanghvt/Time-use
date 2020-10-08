#############################################################################
# SOCIAL CONTACT DATA ANALYSIS
#
# Copyright 2019, SIMID
#############################################################################
#
# MODELLING AGE-SPECIFIC EXPOSURE TIME
#
#############################################################################


rm(list=ls(all=TRUE))
setwd("C:\\Users\\lucp9032\\Desktop\\TIME USE FOLDER\\CODE_MI_BOOSTRAP\\THANG")

require(devtools)
devtools::install_github("lwillem/simid_rtools",force=F,quiet=T)
#devtools::uninstall(simid.rtools)
library('simid.rtools')

packages_list <- c('plyr',
                   'tidyr',
                   'mgcv',
                   'gamlss',
                   'gamlss.inf')  #for fitting the model 

# load packages (and install if not present)
smd_load_packages(packages_list)  

# input/output folder
data_folder    <- smd_file_path('data')
MI_time_use_data_folder <- smd_file_path('output','MI_time_use')
MI_data_folder<- smd_file_path('output','imputation')
output_folder  <- smd_file_path('output','MI_exposure_time')


  
  ###############################################################################
TUD_func<-function(Imp_index,ind_TU_tag,TU_matrix_tag)
{
  tij_func<-function(data){
    data_ex_long<-gather(data, cont_agegr, time_exp, cont_agr1:cont_agr17, factor_key=TRUE)
    
    data_ex_long$time_exp<-as.numeric(data_ex_long$time_exp)
    data_ex_long<-na.omit(data_ex_long)
    
    assign("data_ex_long",data_ex_long,.GlobalEnv)
    #log_normal
    gamlssntr <- gamlss.control(nthreads=4, c.crit=0.0001, autostep=TRUE)
    t3 <- gamlssZadj(time_exp,~-1 + factor(cont_agegr):factor(age_cat), mu.formula=~-1 + factor(cont_agegr):factor(age_cat),sigma.formula =~-1 + factor(cont_agegr):factor(age_cat),  xi0.formula = ~-1 + factor(cont_agegr):factor(age_cat), family=LOGNO, trace=TRUE,weights = data_ex_long$diary_weight,data=data_ex_long,control=gamlssntr)
    #t3 <- gamlssZadj(time_exp,~-1 + factor(cont_agegr):factor(age_cat), mu.formula=~-1 + factor(cont_agegr):factor(age_cat),sigma.formula =~-1 + factor(age_cat),  xi0.formula = ~-1 + factor(age_cat), family=LOGNO, trace=TRUE,weights = data_ex_long$diary_weight,data=data_ex_long,control=gamlssntr)
    #t1 <- gamlssZadj(time_exp,~-1 + factor(cont_agegr):factor(age_cat), mu.formula=~-1 + factor(cont_agegr):factor(age_cat),sigma.formula =~-1 + factor(cont_agegr):factor(age_cat),  xi0.formula = ~-1 + factor(cont_agegr):factor(age_cat), family=GA, trace=TRUE,weights = data_ex_long$diary_weight,data=data_ex_long,control=gamlssntr)
    #t2 <- gamlssZadj(time_exp,~-1 + factor(cont_agegr):factor(age_cat), mu.formula=~-1 + factor(cont_agegr):factor(age_cat),sigma.formula =~-1 + factor(cont_agegr):factor(age_cat),  xi0.formula = ~-1 + factor(cont_agegr):factor(age_cat), family=IG, trace=TRUE,weights = data_ex_long$diary_weight,data=data_ex_long,control=gamlssntr)
    #t4 <- gamlssZadj(time_exp,~-1 + factor(cont_agegr):factor(age_cat), mu.formula=~-1 + factor(cont_agegr):factor(age_cat),sigma.formula =~-1 + factor(cont_agegr):factor(age_cat),  xi0.formula = ~-1 + factor(cont_agegr):factor(age_cat), family=WEI, trace=TRUE,weights = data_ex_long$diary_weight,data=data_ex_long,control=gamlssntr)
    #c(t1$aic,t2$aic,t3$aic,t4$aic) #--> logNormal
    #64776.79 58408.50 56596.11 62034.31
    
    ###Prediction####
    cont_agegr<-rep(levels(data_ex_long$cont_agegr),17)
    age_cat<-rep(levels(data_ex_long$age_cat),each=17)
    
    newdata=data.frame(age_cat,cont_agegr)
    
    tmp_mu=predict(t3,newdata,parameter = c("mu"),type="response")
    tmp_sigma=predict(t3,newdata,parameter = c("sigma"),type="response")
    tmp_pi=predict(t3,newdata,parameter = c("xi0"),type="response")
    
    tmp_common<-exp(tmp_mu+tmp_sigma/2)*(1-tmp_pi)
    
    
    aij<-matrix(tmp_common,ncol =17,byrow = T)
    
    popcount2011 <- read.delim(file.path(data_folder,"2011_pop_ages_FLBR.txt"))
    wj2010 <- popcount2011$popFLBR[1:(89+1)] 
    int.breaks2010<-c(0, 3, 6, 12,18,25,30,35,40,45,50,55,60,65,70,75,80,89)
    int.lengths=diff(int.breaks2010)
    wj.int2010 <-tapply(wj2010[1:(89+1)],c(rep(c(1:(length(int.breaks2010)-1)),diff(int.breaks2010),each=T),length(int.breaks2010)-1),sum)
    
    #wj.cat=rep(wj.int2010/int.lengths,1,each=T)
    wj.cat=rep(wj.int2010,1,each=T)
    Wj<-t(matrix(rep(wj.cat,17),ncol=17))
    aij<-((aij*t(Wj)+t(aij)*Wj)/2)/t(Wj)
    
    aij<- data.matrix(aij)
    aij_c<-aij/Wj
    
    ##############################################################
    #######HOME TIME EXPOSURE MATRICES 90X90############
    ##############################################################
    
    func<-function(x){
      a1<-c(3,3,6,6,7,5,5,5,5,5,5,5,5,5,5,5,10)  
      f1<-function(i1){
        rep(x[i1,],a1[i1])
      }
      x1<-matrix(unlist(sapply(1:17,f1)),ncol=17,byrow=TRUE)
      f2<-function(i1){
        rep(x1[,i1],a1[i1])
      }
      x2<-matrix(unlist(sapply(1:17,f2)),ncol=90,byrow=FALSE)
    }
    
    aij_90<-func(aij_c)
    
    return(aij_90)
  }
  
  location<-get(load(file=smd_file_path(MI_time_use_data_folder,paste0(ind_TU_tag,Imp_index,".RData"))))
  load(file=file.path(MI_data_folder,"MI_data_weight"))
  part<-MI_data_weight[[Imp_index]]
  location$diary_weight<-part$diary_weight
  names(location)[2:18]<-c(paste("cont_agr",1:17,sep=""))
  aij_location_MI<-tij_func(location)
  output_filename1 = paste0(TU_matrix_tag,Imp_index, ".RData")
  save(aij_location_MI, file = smd_file_path(output_folder,output_filename1)) 
}

no_imputation<-10
for(Imp_index in 1:no_imputation)
  {TUD_func(Imp_index,"tij_general_ind_MI","aij_general_MI")
}

for(Imp_index in 1:no_imputation)
{TUD_func(Imp_index,"tij_home_ind_MI","aij_home_MI")
}

for(Imp_index in 1:no_imputation)
{TUD_func(Imp_index,"tij_merged_school_ind_MI","aij_school_MI")
}

for(Imp_index in 1:no_imputation)
{TUD_func(Imp_index,"tij_work_ind_MI","aij_work_MI")
}

for(Imp_index in 1:no_imputation)
{TUD_func(Imp_index,"tij_transport_ind_MI","aij_transport_MI")
}

for(Imp_index in 1:no_imputation)
{TUD_func(Imp_index,"tij_merged_other_ind_MI","aij_other_MI")
}


