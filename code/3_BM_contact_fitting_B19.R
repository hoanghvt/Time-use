#############################################################################
# SOCIAL CONTACT DATA ANALYSIS
#
# Copyright 2019, SIMID
#############################################################################
#
# FITTING EXPOSURE TIME MATRICES TO B19#####
#
#############################################################################

rm(list=ls(all=TRUE))
setwd("C:\\Users\\lucp9032\\Desktop\\TIME USE FOLDER\\CODE_MI_BOOSTRAP\\THANG")

require(devtools)
devtools::install_github("lwillem/simid_rtools",force=F,quiet=T)
#devtools::uninstall(simid.rtools)
library('simid.rtools')


# load packages (and install if not present)
smd_load_packages(packages_list)  

# input/output folder
data_folder    <- smd_file_path('data')
BM_contact_matrice_folder  <- smd_file_path('output',"BM_contact_matrices")
output_folder       <- smd_file_path('output','fitted_b19','BM_contact')


#note by Thang: code was modified by assuming constant force of infection over age categories### 
### PARAMETERS
### Estimating the mortality function for 2006 for Belgium (source: EUROSTAT)
#Number of Death#
ND<-c(489,47,29,21,12,12,16,15,15,6,6,14,17,19,17,23,34,33,62,71,68,68,78,71,71,96,86,83,79,80,83,93,126,120,121,132,135,176,161,193,196,218,257,277,331,376,356,435,460,453,535,545,576,668,692,759,722,819,939,1015,1051,973,1113,996,940,1074,1252,1367,1468,1541,1661,1838,2012,2236,2517,2793,2938,2994,3311,3516,3727,3857,4088,4161,4261,4274,4061,2509,2049,2159,2205,2550,2330,1992,1569,1242,1000,726,533,996)
#Population 
PS<-c(118366,117271,114562,113894,116275,118030,116761,117742,119583,119887,118963,119958,124637,129143,131030,129724,127187,126433,124377,124883,122201,124482,126459,130129,133897,135009,134516,133495,132705,132040,130602,135638,140537,146151,150467,152113,151656,151412,153371,158268,162456,167652,164871,161671,162060,159735,160672,157030,153820,151114,148978,145929,142374,141215,135525,135968,134692,135991,134291,134131,113024,112198,105880,92772,84462,93787,100820,101866,97208,94145,92451,93027,91640,93593,91933,89900,81718,77891,73104,70082,67057,62178,57642,51786,47466,42065,28004,17186,14492,13838,13957,13358,10442,8063,5604,4289,2843,2068,1368,2146)
AGE<-c(0:(length(ND)-1))
# mortality rates
library(mgcv)
demfit<-gam(ND~s(AGE),offset=log(PS),family="poisson",link="log")
muy<-predict(demfit,type="response")
My<-exp(-cumsum(muy))
L<-mean(My)*100
# Mean duration of infectiousness
D<-6/365
# Maximum life (if type mortality this is the life expectancy)
Lmax<-100
# Population size
N<-sum(PS)
# Age of loss of maternal immunity (0<A<1)
A<-0.5
# Mortality function
My<-My[1:Lmax]
muy<-muy[1:Lmax]
# Age category
breakpoints<-c(0, 3, 6, 12,18,25,30,35,40,45,50,55,60,65,70,75,80,100)

###################################################
## Section 15.1: Estimation from Serological Data #
## and Data on Social Contacts                    #
##								  #
###################################################

### Reading in the data: VZV in Belgium: NEW DATA WITH AGE UP TO 71
B19<-read.csv(file="data/BEserologyESEN2v20110704.csv",header = T, sep = ";",dec=c(",","."))
B19<-B19[c( "ESENID","ageold","age..as.exact.as.possible.", "ageL","ageR","parvouml","parvores")]
B19$mean<-(B19$ageL +B19$ageR)/2
B19<-B19[!is.na(B19$parvores)&!is.na(B19$mean)&B19$mean>=1& B19$mean<70,]
B19<-B19[order(B19$mean),]

y<-B19$parvores
a<-B19$mean

################################################################################################
################################   MSIRWbext (phi=1)             ###############################
################################################################################################
#### Test

Lmax <- 85

#LW: new function, based on a loop instead of matrix indices
createmat <- function(give_vec){ # give_vec: vector of length Lmax: Need to modify if Lmax differs from 85
  
  L_vec   <- length(give_vec) #LW: we could use 'Lmax'... or use an intrinsic parameter, the length of give_vec
  
  # create matrix with 1 on diagonal
  mat_out <- diag(1,nrow=L_vec-1,ncol=L_vec-1)
  
  # add cumulative sum per age
  for(i_age in 1:nrow(mat_out)){
    mat_out[i_age,-(1:i_age)] <- exp(-cumsum(give_vec[-c(1:i_age,L_vec)]))
  }
  
  
  return(mat_out)
}


contact.fitterV2MSIRWb <-function(a,y,rij,muy,N,D,Lmax,startpar,plots="TRUE"){
  
  L<-Lmax*mean(exp(-cumsum(muy)))
  
  qproc<-function(a,y,param,rij,Lmax,N,D,plots="TRUE"){
    if (Lmax>100){return("Please specify Lmax<100")}
    bij<-365*param[1]*(rij)[1:Lmax,1:Lmax]
    foiiprev<-rep(0.01,Lmax)
    muy<-muy[1:Lmax]
    tol<-1
    it<-0
    while ((tol>1e-10)&(it<2000)){
      foii<-(N/L)*D*exp(-muy[1]*A)*bij%*%(as.matrix(foiiprev/(foiiprev+muy))*matrix(c(1-exp(-(1-A)*(foiiprev[1]+muy[1])),exp(-(1-A)*(foiiprev[1]+muy[1])-c(0,cumsum(foiiprev[-1]+muy[-1])[1:(Lmax-2)]))-exp(-(1-A)*(foiiprev[1]+muy[1])-c(cumsum(foiiprev[-1]+muy[-1])))),ncol=1))		
      foii<- apply(cbind(0,foii),1,max)
      foii<- apply(cbind(1,foii),1,min)
      tol<-base::sum((foii-foiiprev)^2)
      it<-it+1
      foiiprev<-foii 
    }
    
    
    #LW: NEW PROCEDURE
    # intermediate values
    a_floor <- floor(a)
    foii_a  <- foii[-1][a_floor]
    
    tmp_foii_a1     <- (foii_a)/(foii_a + param[2]) * ( 1-exp(-(foii_a + param[2])*(a-a_floor)))
    tmp_foii_a2     <- foii[1]/(foii[1]+param[2]) * (1-exp(-(foii[1] +param[2])*(a-A)))
    tmp_foii_L      <- (foii/(foii+param[2])*(1-exp(-(foii+param[2]))))[2:(Lmax-1)]
    foii_param2_mat <- createmat(foii+param[2])
    
    prev <- rep(NA,length(a))
    for (i in 1:length(a)){
      prev[i] <- (tmp_foii_a1[i]) +
        sum(c(tmp_foii_a2[i],tmp_foii_L) * foii_param2_mat[,a_floor[i]])
    }
    
    # set min and max
    prev[prev>1] <- 1
    prev[prev<0] <- 0
    
    # get log likelyhood
    ll <- y*log(prev+1e-8)+(1-y)*log(1-prev+1e-8)
    
    R0ij  <- (N/L)*D*bij[1:Lmax,1:Lmax]
    Mij   <- diag(c(My[1:Lmax]))
    R0vec <- eigen(Mij%*%R0ij,symmetric=FALSE,only.values=TRUE,EISPACK=FALSE)$values
    R0    <- max(Re(R0vec))
    
    return(list(ll=-2*base::sum(ll),eivalues=R0vec,prev=prev,foiiprev,bij=bij))
  }
  
  qproc.fitter<-function(param){
    return(qproc(a,y,param,rij,Lmax,N,D,plots="TRUE")$ll)
  }
  
  q.result <- nlm(qproc.fitter,startpar)
  result.global<-qproc(a=a,y=y,param=c(q.result$estimate),rij=rij,Lmax=Lmax,N=N,D=D)
  
  return(list(qhat=q.result$estimate,deviance=q.result$minimum,
              aic=q.result$minimum+sum(q.result$estimate!=0)*2,
              bic=q.result$minimum+sum(q.result$estimate!=0)*log(length(y)),
              bij=result.global$bij,
              R0=max(Re(result.global$eivalues)))) 
}

######TUD######

#load(file=file.path(BM_contact_matrice_folder,"contact_all_BM1"))
#save(contact_all,file=file.path(BM_contact_matrice_folder,"contact_all_BM.Rdata"))

#load(file=file.path(BM_contact_matrice_folder,"contacts_close_BM1"))
#load(file=file.path(BM_contact_matrice_folder,"contacts_non_close_BM1"))
#load(file=file.path(BM_contact_matrice_folder,"contacts_less_15_BM1"))
#load(file=file.path(BM_contact_matrice_folder,"contacts_gre_15_BM1"))
#load(file=file.path(BM_contact_matrice_folder,"contacts_gre_1h_BM1"))
#load(file=file.path(BM_contact_matrice_folder,"contacts_gre_4h_BM1"))
#load(file=file.path(BM_contact_matrice_folder,"contacts_home_BM1"))
#load(file=file.path(BM_contact_matrice_folder,"contacts_work_BM1"))
#load(file=file.path(BM_contact_matrice_folder,"contacts_transport_BM1"))
#load(file=file.path(BM_contact_matrice_folder,"contacts_other_BM1"))


#save(contacts_close,file=file.path(BM_contact_matrice_folder,"contacts_close_BM.Rdata"))
#save(contacts_non_close,file=file.path(BM_contact_matrice_folder,"contacts_non_close_BM.Rdata"))
#save(contacts_less_15,file=file.path(BM_contact_matrice_folder,"contacts_less_15_BM.Rdata"))
#save(contacts_gre_15,file=file.path(BM_contact_matrice_folder,"contacts_gre_15_BM.Rdata"))
#save(contacts_gre_1h,file=file.path(BM_contact_matrice_folder,"contacts_gre_1h_BM.Rdata"))
#save(contacts_gre_4h,file=file.path(BM_contact_matrice_folder,"contacts_gre_4h_BM.Rdata"))
#save(contacts_home,file=file.path(BM_contact_matrice_folder,"contacts_home_BM.Rdata"))
#save(contacts_work,file=file.path(BM_contact_matrice_folder,"contacts_work_BM.Rdata"))
#save(contacts_transport,file=file.path(BM_contact_matrice_folder,"contacts_transport_BM.Rdata"))
#save(contacts_other,file=file.path(BM_contact_matrice_folder,"contacts_other_BM.Rdata"))


func_boot_B19<-function(boot,location_tag,startpar){
  file_load<-get(load(file=file.path(BM_contact_matrice_folder,paste0(location_tag,".Rdata"))))
  general<-file_load[[boot]][,-1]
  contact.result<-contact.fitterV2MSIRWb(a=a,y=y,rij=general[1:85,1:85],muy=muy,N=N,D=D,Lmax=85,plots="TRUE",startpar=startpar)
  all<-c(contact.result$qhat,contact.result$epsilon,contact.result$R0,contact.result$aic,contact.result$bic)
}


# start parallel nodes
start<-Sys.time()
smd_start_cluster()

    foreach(boot = 1:1000,   # iteration index
            .combine     = rbind,                # combine the output by row
            .packages    = 'simid.rtools',
            .errorhandling = 'remove')  %dopar% # transfer packages and run parallel  
      {
        result<-func_boot_B19(boot,"contact_all_BM",c(0.0124529,0.005924744))
  }->result
    filename = paste0("BM_contact_all_B19", ".txt")
    write.table(result, file = smd_file_path(output_folder,filename), sep="\t",col.names = FALSE,row.names = FALSE)
    
smd_stop_cluster()
end<-Sys.time()
end -start #

# start parallel nodes
start<-Sys.time()
smd_start_cluster()

foreach(boot = 1:1000,   # iteration index
        .combine     = rbind,                # combine the output by row
        .packages    = 'simid.rtools',
        .errorhandling = 'remove')  %dopar% # transfer packages and run parallel  
  {
    result<-func_boot_B19(boot,"contacts_close_BM",c(0.02177689,0.005768475))
  }->result

filename = paste0("BM_contact_close_B19", ".txt")
write.table(result, file = smd_file_path(output_folder,filename), sep="\t",col.names = FALSE,row.names = FALSE)

smd_stop_cluster()
end<-Sys.time()
end -start #

# start parallel nodes
start<-Sys.time()
smd_start_cluster()

foreach(boot = 1:1000,   # iteration index
        .combine     = rbind,                # combine the output by row
        .packages    = 'simid.rtools',
        .errorhandling = 'remove')  %dopar% # transfer packages and run parallel  
  {
    result<-func_boot_B19(boot,"contacts_non_close_BM",c(0.03100856,0.006185591))
  }->result

filename = paste0("BM_contact_non_close_B19", ".txt")
write.table(result, file = smd_file_path(output_folder,filename), sep="\t",col.names = FALSE,row.names = FALSE)

smd_stop_cluster()
end<-Sys.time()
end -start #


# start parallel nodes
start<-Sys.time()
smd_start_cluster()

foreach(boot = 1:1000,   # iteration index
        .combine     = rbind,                # combine the output by row
        .packages    = 'simid.rtools',
        .errorhandling = 'remove')  %dopar% # transfer packages and run parallel  
  {
    result<-func_boot_B19(boot,"contacts_less_15_BM",c(1e-1,0.007))

  }->result

filename = paste0("BM_contact_less_15_B19", ".txt")
write.table(result, file = smd_file_path(output_folder,filename), sep="\t",col.names = FALSE,row.names = FALSE)

smd_stop_cluster()
end<-Sys.time()
end -start #


# start parallel nodes
start<-Sys.time()
smd_start_cluster()

foreach(boot = 1:1000,   # iteration index
        .combine     = rbind,                # combine the output by row
        .packages    = 'simid.rtools',
        .errorhandling = 'remove')  %dopar% # transfer packages and run parallel  
  {
    result<-func_boot_B19(boot,"contacts_gre_15_BM",c(0.02546418,0.005910424))
  }->result

filename = paste0("BM_contact_gre_15_B19", ".txt")
write.table(result, file = smd_file_path(output_folder,filename), sep="\t",col.names = FALSE,row.names = FALSE)

smd_stop_cluster()
end<-Sys.time()
end -start #


# start parallel nodes
start<-Sys.time()
smd_start_cluster()

foreach(boot = 1:1000,   # iteration index
        .combine     = rbind,                # combine the output by row
        .packages    = 'simid.rtools',
        .errorhandling = 'remove')  %dopar% # transfer packages and run parallel  
  {
    result<-func_boot_B19(boot,"contacts_gre_1h_BM",c(0.02887913,0.00538117))
  }->result

filename = paste0("BM_contact_gre_1h5_B19", ".txt")
write.table(result, file = smd_file_path(output_folder,filename), sep="\t",col.names = FALSE,row.names = FALSE)

smd_stop_cluster()
end<-Sys.time()
end -start #


# start parallel nodes
start<-Sys.time()
smd_start_cluster()

foreach(boot = 1:1000,   # iteration index
        .combine     = rbind,                # combine the output by row
        .packages    = 'simid.rtools',
        .errorhandling = 'remove')  %dopar% # transfer packages and run parallel  
  {
    result<-func_boot_B19(boot,"contacts_gre_4h_BM",c(0.05560363,0.006077322))
  }->result

filename = paste0("BM_contact_gre_4h_B19", ".txt")
write.table(result, file = smd_file_path(output_folder,filename), sep="\t",col.names = FALSE,row.names = FALSE)

smd_stop_cluster()
end<-Sys.time()
end -start #


# start parallel nodes
start<-Sys.time()
smd_start_cluster()

foreach(boot = 1:1000,   # iteration index
        .combine     = rbind,                # combine the output by row
        .packages    = 'simid.rtools',
        .errorhandling = 'remove')  %dopar% # transfer packages and run parallel  
  {
    result<-func_boot_B19(boot,"contacts_home_BM",c(5e-2,0.009654205))
  }->result

filename = paste0("BM_home_B19", ".txt")
write.table(result, file = smd_file_path(output_folder,filename), sep="\t",col.names = FALSE,row.names = FALSE)

smd_stop_cluster()
end<-Sys.time()
end -start #


# start parallel nodes
start<-Sys.time()
smd_start_cluster()

foreach(boot = 1:1000,   # iteration index
        .combine     = rbind,                # combine the output by row
        .packages    = 'simid.rtools',
        .errorhandling = 'remove')  %dopar% # transfer packages and run parallel  
  {
    result<-func_boot_B19(boot,"contacts_work_BM",c(0.0719854,0.007))
  }->result

filename = paste0("BM_work_B19", ".txt")
write.table(result, file = smd_file_path(output_folder,filename), sep="\t",col.names = FALSE,row.names = FALSE)

smd_stop_cluster()
end<-Sys.time()
end -start #

# start parallel nodes
start<-Sys.time()
smd_start_cluster()

foreach(boot = 1:1000,   # iteration index
        .combine     = rbind,                # combine the output by row
        .packages    = 'simid.rtools',
        .errorhandling = 'remove')  %dopar% # transfer packages and run parallel  
  {
    result<-func_boot_B19(boot,"contacts_transport_BM",c(0.2048392,0.01137224))
  }->result

filename = paste0("BM_transport_B19", ".txt")
write.table(result, file = smd_file_path(output_folder,filename), sep="\t",col.names = FALSE,row.names = FALSE)

smd_stop_cluster()
end<-Sys.time()
end -start #


# start parallel nodes
start<-Sys.time()
smd_start_cluster()

foreach(boot = 1:1000,   # iteration index
        .combine     = rbind,                # combine the output by row
        .packages    = 'simid.rtools',
        .errorhandling = 'remove')  %dopar% # transfer packages and run parallel  
  {
    result<-func_boot_B19(boot,"contacts_other_BM",c(0.05641319,0.01054568))
  }->result

filename = paste0("BM_other_B19", ".txt")
write.table(result, file = smd_file_path(output_folder,filename), sep="\t",col.names = FALSE,row.names = FALSE)

smd_stop_cluster()
end<-Sys.time()
end -start #

# start parallel nodes
start<-Sys.time()
smd_start_cluster()

foreach(boot = 1:1000,   # iteration index
        .combine     = rbind,                # combine the output by row
        .packages    = 'simid.rtools',
        .errorhandling = 'remove')  %dopar% # transfer packages and run parallel  
  {
    result<-func_boot_B19(boot,"contacts_other_BM",c(0.05641319,0.01054568))
  }->result

filename = paste0("BM_other_B19", ".txt")
write.table(result, file = smd_file_path(output_folder,filename), sep="\t",col.names = FALSE,row.names = FALSE)

smd_stop_cluster()
end<-Sys.time()
end -start #


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# start parallel nodes

func_boot_B19<-function(boot,location_tag,startpar){
  file_load<-get(load(file=file.path(BM_contact_matrice_folder,paste0(location_tag,".Rdata"))))
  file_load1<-NULL
  for (i in 1:1000){
    file_load1[[i]]<-  file_load[,(1+101*(i-1)):(101*i)]
  }
  general<-as.matrix(file_load1[[boot]][,-1])
  contact.result<-contact.fitterV2MSIRWb(a=a,y=y,rij=general[1:85,1:85],muy=muy,N=N,D=D,Lmax=85,plots="TRUE",startpar=startpar)
  all<-c(contact.result$qhat,contact.result$epsilon,contact.result$R0,contact.result$aic,contact.result$bic)
}



boot<-1
location_tag<-"contacts_school"
startpar=c(0.01878198,0.002563826)

start<-Sys.time()
smd_start_cluster()

foreach(boot = 1:1000,   # iteration index
        .combine     = rbind,                # combine the output by row
        .packages    = 'simid.rtools',
        .errorhandling = 'remove')  %dopar% # transfer packages and run parallel  
  {
    result<-func_boot_B19(boot,"contacts_school",startpar=c(0.01878198,0.002563826))
  }->result

filename = paste0("BM_school_B19", ".txt")
write.table(result, file = smd_file_path(output_folder,filename), sep="\t",col.names = FALSE,row.names = FALSE)

smd_stop_cluster()
end<-Sys.time()
end -start #


