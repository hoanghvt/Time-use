#############################################################################
# SOCIAL CONTACT DATA ANALYSIS
#
# Copyright 2019, SIMID
#############################################################################
#
# FITTING EXPOSURE TIME MATRICES TO VZV#####
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
output_folder       <- smd_file_path('output','fitted_VZV')
contact_folder<-smd_file_path("MI_contact_matrices","MI2")

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
Lmax<-89
# Population size
N<-sum(PS)
# Age of loss of maternal immunity (0<A<1)
A<-0.5
# Mortality function
My<-My[1:Lmax]
muy<-muy[1:Lmax]
# Age category
breakpoints<-c(0, 3, 6, 12,18,25,30,35,40,45,50,55,60,65,70,75,80,90)

###################################################
## Section 15.1: Estimation from Serological Data #
## and Data on Social Contacts                    #
##								  #
###################################################

### Reading in the data: VZV in Belgium: NEW DATA WITH AGE UP TO 71
VZV<- read.csv(file=file.path(data_folder,"BEserologyESEN2v20110704.csv"),header = T, sep = ";",dec=c(",","."))
VZV<-VZV[c( "ESENID","ageold","age..as.exact.as.possible.", "ageL","ageR","VZVmUIml","VZVres")]
#VZV$age..as.exact.as.possible. <- sub(",",".",VZV$age..as.exact.as.possible.)
#VZV$age..as.exact.as.possible. <- as.numeric(VZV$age..as.exact.as.possible.)
VZV$mean<-(VZV$ageL +VZV$ageR)/2
VZV<-VZV[!is.na(VZV$VZVres)&!is.na(VZV$mean)&VZV$mean>=0.5& VZV$mean<70,]
VZV<-VZV[order(VZV$mean),]

y<-VZV$VZVres
a<-VZV$mean


##### CONTACT.FITTER #######
contact.fitter<-function(a,y,rij,muy,N,D,Lmax,plots="TRUE",startpar){
  L<-Lmax*mean(exp(-cumsum(muy)))
  qproc<-function(a,y,qpar,rij,Lmax,N,D,plots="TRUE"){
    if (Lmax>100){return("Please specify Lmax<100")}
    bij<-365*qpar*(rij)[1:Lmax,1:Lmax]
    foiiprev<-rep(0.01,Lmax)
    muy<-muy[1:Lmax]
    tol<-1
    it<-0
    while ((tol>1e-10)&(it<2000)){
      foii<-(N/L)*D*bij%*%(as.matrix(foiiprev/(foiiprev+muy))*matrix(c(1-exp(-(1-A)*(foiiprev[1]+muy[1])),exp(-(1-A)*(foiiprev[1]+muy[1])-c(0,cumsum(foiiprev[-1]+muy[-1])[1:(Lmax-2)]))-exp(-(1-A)*(foiiprev[1]+muy[1])-c(cumsum(foiiprev[-1]+muy[-1])))),ncol=1))		
      foii<- apply(cbind(0,foii),1,max)
      foii<- apply(cbind(1,foii),1,min)
      tol<-sum((foii-foiiprev)^2)
      it<-it+1
      foiiprev<-foii
    }
    if (plots=="TRUE"){
      par(mfrow=c(1,1))
      par(mar=c(5,4,4,4)+0.3)
      plot(c(A,1:max(floor(a))),1-exp(c(0,-(1-A)*foii[1],-(1-A)*foii[1]-cumsum(foii[-1])[1:(max(floor(a))-1)])),type="l",xlab="age",ylab="prevalence",ylim=c(0,1),xlim=c(0,80),lwd=2)
      lines((max(floor(a))+1):(Lmax-1),1-exp(-(1-A)*foii[1]-cumsum(foii[-1])[max(floor(a)):(Lmax-2)]),lty=2,lwd=2)
      htab<-table(floor(a),y)
      points(c(A,sort(unique(floor(a)))[-1]),htab[,2]/(htab[,1]+htab[,2]),cex=0.02*(htab[,1]+htab[,2]),lwd=1.1)
      par(new=TRUE)
      for (i in 1:(length(breakpoints)-3)){lines(c(breakpoints[i],min(breakpoints[i+1],max(a))),c(unique(foii)[i],unique(foii)[i]),lwd=2)}
      for (i in 2:(length(breakpoints)-3)){lines(c(breakpoints[i],breakpoints[i]),c(unique(foii)[i-1],unique(foii)[i]),lwd=2,lty=3)}
      lines((max(floor(a))+1):(Lmax-1),foii[(max(floor(a))+2):Lmax],lty=2,lwd=2)
      axis(4,at=pretty(range(foii)))
      #plot(c(A,1:max(floor(a))),foii[1:(max(floor(a))+1)],type="l",axes=FALSE,bty="n",xlab="",ylab="",ylim=c(0,1),xlim=c(0,80),lwd=2)
      #lines((max(floor(a))+1):(Lmax-1),foii[(max(floor(a))+2):Lmax],lty=2,lwd=2)
      #axis(4,at=pretty(range(foii)))
      #contour(c(0:(Lmax-1)),c(0:(Lmax-1)),bij,xlab="age of susceptible",ylab="age of infectious")
    }	
    
    prev<-rep(NA,length(a))
    ll<-rep(NA,length(a))
    for (i in 1:length(a)){
      prev[i]<-(1-exp(c(-(a[i]-A)*foii[1],-(1-A)*foii[1]-cumsum(c(0,foii[-1]))-(foii[-1])[floor(a[i])]*(a[i]-floor(a[i])))))[floor(a[i])+1]
      ll[i]<-y[i]*log(prev[i]+1e-8)+(1-y[i])*log(1-prev[i]+1e-8)
      pi<-1-exp(c(0,-(1-A)*foii[1],-(1-A)*foii[1]-cumsum(foii[-1])))
    }
    R0ij<-(N/L)*D*bij[1:Lmax,1:Lmax]
    Mij<-diag(c(My[1:Lmax]))
    R0vec<-eigen(Mij%*%R0ij,symmetric=FALSE,only.values=TRUE,EISPACK=FALSE)$values
    return(list(ll=-2*sum(ll),eivalues=R0vec,prev=prev,bij=bij,foii=foii,pi=pi))
  }
  qproc.fitter<-function(qpar){return(qproc(a,y,qpar,rij,Lmax,N,D,plots="TRUE")$ll)}
  q.result<-nlm(qproc.fitter,startpar,hessian = TRUE)
  result.global<-qproc(a=a,y=y,q=q.result$estimate,rij=rij,Lmax=Lmax,N=N,D=D)
  return(list(hessian=q.result$hessian,FOI=result.global$foii,pi=result.global$pi,qhat=q.result$estimate,deviance=q.result$minimum,aic=q.result$minimum+2,bic=q.result$minimum+log(length(y)),bij=result.global$bij,R0=max(as.double(result.global$eivalues))))
  
  #USE NLMINB to restrict parameter space in a range (0;1)
  #q.result<-nlminb(startpar,qproc.fitter,lower = c(0),upper=c(1))
  #result.global<-qproc(a=a,y=y,q=q.result$par,rij=rij,Lmax=Lmax,N=N,D=D)
  #return(list(hessian=q.result$hessian,FOI=result.global$foii,pi=result.global$pi,qhat=q.result$par,deviance=q.result$objective,aic=q.result$objective+2,bic=q.result$objective+log(length(y)),bij=result.global$bij,RI=result.global$eigenvector,R0=max(as.double(result.global$eivalues))))
}

func_boot_VZV<-function(Imp_index,location_tag,location_save,startpar){
  general<-get(load(file=file.path(TUD_matrice_folder,paste0(location_tag,Imp_index,".RData"))))
  
  contact.result<-contact.fitter(a=a,y=y,rij=general,muy=muy,N=N,D=D,Lmax=85,plots="TRUE",startpar=startpar)
  all<-rbind(Imp_index,c(contact.result$qhat,contact.result$R0,contact.result$aic,contact.result$bic))
  write(all,file=file.path(output_folder,paste0(location_save,".txt")), ncol=8, sep="\t", append=T)
}

for(Imp_index in 1:10){
  func_boot_VZV(Imp_index,"aij_general_MI","VZV_general_MI",5e-2) 
}

for(Imp_index in 1:10){
  func_boot_VZV(Imp_index,"aij_home_MI","VZV_home_MI",5e-2) 
}

for(Imp_index in 1:10){
  func_boot_VZV(Imp_index,"aij_school_MI","VZV_school_MI",2e-1) 
}

for(Imp_index in 1:10){
  func_boot_VZV(Imp_index,"aij_work_MI","VZV_work_MI",10) 
}

for(Imp_index in 1:10){
  func_boot_VZV(Imp_index,"aij_transport_MI","VZV_transport_MI",0.9) 
}

for(Imp_index in 1:10){
  func_boot_VZV(Imp_index,"aij_other_MI","VZV_other_MI",5e-1) 
}

######Contact data######

func_boot_vzv_contact<-function(location_tag,location_save,startpar){
  general<-get(load(file=file.path(contact_folder,paste0(location_tag,".RData"))))[,-1]
  contact.result<-contact.fitter(a=a,y=y,rij=general[1:85,1:85],muy=muy,N=N,D=D,Lmax=85,plots="TRUE",startpar=startpar)
  all<-rbind(c(contact.result$qhat,contact.result$R0,contact.result$aic,contact.result$bic))
  write(all,file=file.path(output_folder,paste0(location_save,".txt")), ncol=8, sep="\t", append=T)
}

func_boot_vzv_contact("contact_all_2","VZV_contact_all",c(5e-2))
func_boot_vzv_contact("contacts_close_2","VZV_contact_close",c(5e-2))
func_boot_vzv_contact("contacts_non_close_2","VZV_contact_non_close",c(5e-2))
func_boot_vzv_contact("contacts_less_15_2","VZV_contact_less_15",c(5e-2))
func_boot_vzv_contact("contacts_gre_15_2","VZV_contact_gre_15",c(5e-2))
func_boot_vzv_contact("contacts_gre_1h_2","VZV_contact_gre_1h",c(5e-2))
func_boot_vzv_contact("contacts_gre_4h_2","VZV_contact_gre_4h",c(5e-2))
func_boot_vzv_contact("contacts_home_2","VZV_contact_home",c(5e-2))
func_boot_vzv_contact("contacts_school_2","VZV_contact_school",c(5e-2))
##change to continuous
##contacts_school_2<-as.matrix(read.table("school_continuous.txt"))

func_boot_vzv_contact("contacts_transport_2","VZV_contact_transport",c(5e-2))
func_boot_vzv_contact("contacts_work_2","VZV_contact_work",c(5e-2))
func_boot_vzv_contact("contacts_other_2","VZV_contact_other",c(5e-2))














