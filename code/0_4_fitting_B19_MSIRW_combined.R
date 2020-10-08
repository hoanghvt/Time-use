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
TUD_matrice_folder  <- smd_file_path('output','MI_exposure_time')
output_folder       <- smd_file_path('output','fitted_b19')


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


contact.fitterV2MSIRWb <-function(a,y,rij1,rij2,muy,N,D,Lmax,startpar,plots="TRUE"){
  L<-Lmax*mean(exp(-cumsum(muy)))
  qproc<-function(a,y,param,rij1,rij2,Lmax,N,D,plots="TRUE"){
    param1<-param^2
    rij<-rij1*(1-exp(-param1[2]*rij2/rij1))
    bij<-365*param1[1]*(rij)[1:Lmax,1:Lmax]
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
      foiiprev<-foii }
    
   
    #LW: NEW PROCEDURE
    # intermediate values
    a_floor <- floor(a)
    foii_a  <- foii[-1][a_floor]
    
    tmp_foii_a1     <- (foii_a)/(foii_a + param1[3]) * ( 1-exp(-(foii_a + param1[3])*(a-a_floor)))
    tmp_foii_a2     <- foii[1]/(foii[1]+param1[3]) * (1-exp(-(foii[1] +param1[3])*(a-A)))
    tmp_foii_L      <- (foii/(foii+param1[3])*(1-exp(-(foii+param1[3]))))[2:(Lmax-1)]
    foii_param2_mat <- createmat(foii+param1[3])
    
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
  return(qproc(a,y,param,rij1,rij2,Lmax,N,D,plots="TRUE")$ll)
  }
  
  q.result <- nlm(qproc.fitter,startpar)
  result.global<-qproc(a=a,y=y,param=c(q.result$estimate),rij1=rij1,rij2=rij2,Lmax=Lmax,N=N,D=D)
  
  return(list(qhat=q.result$estimate^2,deviance=q.result$minimum,
              aic=q.result$minimum+sum(q.result$estimate!=0)*2,
              bic=q.result$minimum+sum(q.result$estimate!=0)*log(length(y)),
              bij=result.global$bij,
              R0=max(Re(result.global$eivalues)))) 
}


contact_all_2<-get(load(file.path(TUD_matrice_folder,"contact_all_2.RData")))
contact_all_2<-contact_all_2[,-1]

contact.result<-NULL
all<-NULL
func_boot_B19<-function(boot){
  general<-get(load(file=file.path(TUD_matrice_folder,paste0("aij_general_MI",boot,".RData"))))
  contact.result[[boot]]<-contact.fitterV2MSIRWb(a=a,y=y,rij1=contact_all_2,rij2=general,muy=muy,N=N,D=D,Lmax=85,plots="TRUE",startpar=c(0.22,0.54,0.083))
  all<-rbind(boot,c(contact.result[[boot]]$qhat, contact.result[[boot]]$R0,contact.result[[boot]]$aic,contact.result[[boot]]$bic))
  write(all,file=file.path(output_folder,"B19_combined_MI.txt"), ncol=12, sep="\t", append=T)
}

for (i in 1:10){
  func_boot_B19(i)
}



