
rm(list = ls())
library("dplyr")
library("deSolve")
library("ie2misc")
if(require(rstudioapi) && isAvailable()){
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
}

source("function\\func_convert_matrices.R")
source("function\\func_fit_NB_5q.R")
# source("function\\func_fit_suitable_contact_POISSON_5q.R")
# source("function\\func_fit_suitable_contact_physical_non_physical.R")

###Load population data
pop<-read.csv("data\\pop_2011.txt",head=T,sep="\t")
int.breaks<-c(0,5,15,20,65,99)
N=tapply(pop[1:(99+1),2],c(rep(c(1:(length(int.breaks)-1)),diff(int.breaks),each=T),length(int.breaks)-1),sum)

###Load observed incidence rate
#note: already took account for population coverage of GP:1.75%
obs_inc<-read.csv("data\\observed_weekly_cases_2010_2011.csv",head=T,sep=54",")
names(obs_inc)[grepl("years.old",names(obs_inc))]<-c("[0-4] years old",
                                                     "[5-14] years old",
                                                     "[15-19] years old",
                                                     "[20-64] years old",
                                                     "65+ years old")

###Load matrices
cij_all <-get(load("contact_matrices\\contact_all_2.Rdata"))[,-1]
cij_non_close <-get(load("contact_matrices\\contacts_non_close_2.Rdata"))[,-1]
cij_close <-get(load("contact_matrices\\contacts_close_2.Rdata"))[,-1]
cij_less_15p <-get(load("contact_matrices\\contacts_less_15_2.Rdata"))[,-1]
cij_gre_15p <-get(load("contact_matrices\\contacts_gre_15_2.Rdata"))[,-1]
cij_gre_1h <-get(load("contact_matrices\\contacts_gre_1h_2.Rdata"))[,-1]
cij_gre_4h <-get(load("contact_matrices\\contacts_gre_4h_2.Rdata"))[,-1]
cij_work <-get(load("contact_matrices\\contacts_work_2.Rdata"))[,-1]

##fit contact matrices
start<-Sys.time()
contact_result<-c(func_fit(cij_all,c(beta=rep(0.1,5),dispersion=0.25,scale=0.25)))
Sys.time() -start

# $main
# beta1        beta2        beta3        beta4        beta5   dispersion        scale          AIC 
# 1.063134e-01 7.822299e-02 1.141277e-01 5.676277e-02 1.025127e+00 1.188620e+00 3.758314e-01 3.969237e+03 
# R0 
# 1.617164e+00

contact_result<-c(func_fit(cij_all,c(beta=c(1.315033e-01, 7.288021e-02, 1.992438e-01, 7.643035e-02, 1.498295e-01)
                                     ,dispersion=0.25,scale=0.2)))
#poisson
#1.315033e-01 7.288021e-02 1.992438e-01 7.643035e-02 1.498295e-01 3.189215e-01

contact_result<-rbind(func_fit(cij_all,c(beta=rep(0.1,5),scale=0.25)),
                      func_fit(cij_non_close,c(beta=rep(0.1,5),scale=0.25)),
                      func_fit(cij_close,c(beta=rep(0.1,5),scale=0.25)),
                      func_fit(cij_less_15p,c(beta=rep(0.1,5),scale=0.25)),
                      func_fit(cij_gre_15p,c(beta=rep(0.1,5),scale=0.25)),
                      func_fit(cij_gre_1h,c(beta=rep(0.1,5),scale=0.25)),
                      func_fit(cij_gre_4h,c(beta=rep(0.1,5),scale=0.25)))


write.table(contact_result,"result\\cont_result_poisson_5q.txt")
# write(contact_result,"result\\cont_result.Rdata")

# beta1        beta2        beta3        beta4        beta5        scale 
# 1.315033e-01 7.288021e-02 1.992438e-01 7.643035e-02 1.498295e-01 3.189215e-01 
# AIC           R0 
# 1.882897e+05 2.000335e+00 

###load time use matrices
tu_all1 <-get(load("MI_time_use\\aij_general_MI1.Rdata"))[1:90,1:90]
tu_all2 <-get(load("MI_time_use\\aij_general_MI2.Rdata"))[1:90,1:90]
tu_all3 <-get(load("MI_time_use\\aij_general_MI3.Rdata"))[1:90,1:90]
tu_all4 <-get(load("MI_time_use\\aij_general_MI4.Rdata"))[1:90,1:90]
tu_all5 <-get(load("MI_time_use\\aij_general_MI5.Rdata"))[1:90,1:90]
tu_all6 <-get(load("MI_time_use\\aij_general_MI6.Rdata"))[1:90,1:90]
tu_all7 <-get(load("MI_time_use\\aij_general_MI7.Rdata"))[1:90,1:90]
tu_all8 <-get(load("MI_time_use\\aij_general_MI8.Rdata"))[1:90,1:90]
tu_all9 <-get(load("MI_time_use\\aij_general_MI9.Rdata"))[1:90,1:90]
tu_all10 <-get(load("MI_time_use\\aij_general_MI10.Rdata"))[1:90,1:90]


##fit tu matrices
tu_result<-c(func_fit(tu_all1,c(beta=rep(0.1,5),scale=0.1)))
tu_result<-rbind(func_fit(tu_all1,c(beta=rep(0.1,5),scale=0.1)),func_fit(tu_all2,c(beta=rep(0.1,5),scale=0.1)),
                 func_fit(tu_all3,c(beta=rep(0.1,5),scale=0.2)),func_fit(tu_all4,c(beta=rep(0.1,5),scale=0.2)),
                 func_fit(tu_all5,c(beta=rep(0.1,5),scale=0.2)),func_fit(tu_all6,c(beta=rep(0.1,5),scale=0.1)),
                 func_fit(tu_all7,c(beta=rep(0.1,5),scale=0.2)),func_fit(tu_all8,c(beta=rep(0.1,5),scale=0.1)),
                 func_fit(tu_all9,c(beta=rep(0.1,5),scale=0.2)),func_fit(tu_all10,c(beta=rep(0.1,5),scale=0.1)))

write.table(tu_result,"result\\tu_result_poisson_5q.txt")
# write(tu_result,"result\\tu_result.Rdata")

# beta1        beta2        beta3        beta4        beta5        scale 
# 9.114460e-02 6.238247e-02 1.835164e-01 6.850480e-02 1.659906e-01 3.150238e-01 
# AIC           R0 
# 1.908913e+05 1.538185e+00


###fit suitable contacts
start<-Sys.time()
suitable_contact_result<-c(func_fit_suitable_contact(cij_all, tu_all1,rep(0.3,7)))
Sys.time() - start

suitable_contact_result<-rbind(func_fit_suitable_contact(cij_all, tu_all1,rep(0.3,7)),func_fit_suitable_contact(cij_all, tu_all2,rep(0.3,7)),
                               func_fit_suitable_contact(cij_all, tu_all3,rep(0.3,7)),func_fit_suitable_contact(cij_all, tu_all4,rep(0.3,7)),
                               func_fit_suitable_contact(cij_all, tu_all5,rep(0.3,7)),func_fit_suitable_contact(cij_all, tu_all6,rep(0.3,7)),
                               func_fit_suitable_contact(cij_all, tu_all7,rep(0.3,7)),func_fit_suitable_contact(cij_all, tu_all8,rep(0.3,7)),
                               func_fit_suitable_contact(cij_all, tu_all9,rep(0.3,7)),func_fit_suitable_contact(cij_all, tu_all10,rep(0.3,7)))
Sys.time() - start

# beta1        beta2        beta3        beta4        beta5           q2 
# 2.995585e-01 1.923660e-01 5.720462e-01 2.105469e-01 4.722033e-01 4.048631e-01 
# scale          AIC           R0 
# 3.238035e-01 1.906274e+05 4.556671e+00 


# write.table(suitable_contact_result,"result\\suitable_contact_result_poisson_5q.txt")
# write(suitable_contact_result,"result\\suitable_contact_result.Rdata")

###plot
source("function\\func_convert_matrices.R")

# source("function\\processing_code.R")
SEIR_model<-function(t,state,parms){
  beta=parms[1:5]
  dS<-NULL
  dE<-NULL
  dI<-NULL
  dR<-NULL
  
  n=5
  S<-state[1:n]
  E<-state[(n+1):(2*n)]
  I<-state[(2*n+1):(3*n)]
  R<-state[(3*n+1):(4*n)]
  
  with(as.list(c(state,parms)),{
    for(i in 1:5){
      dS[i]<- -beta[i]*S[i]*sum(mij[i,]*I/N)
      dE[i] <- beta[i]*S[i]*sum(mij[i,]*I/N) - (7)*E[i] ###latent period: 1 day
      dI[i] <- (7)*E[i]-(7/3.8)*I[i]  ### infectious period: 3.8 days
      dR[i]<- (7/3.8)*I[i]
    }
    list(c(dS,dE,dI,dR)) 
  })
}
#take into account population coveraged by GP
I=c(as.numeric(obs_inc[grepl("years.old",names(obs_inc))][1,]))   #take the number of cases at the first week as initial values for infected cases

# take into account specific -age vaccination percentage
R=c(0.066/100,0.066/100,5.5/100,20/100,60/100)*N
E=rep(0,5)
S=(N-I-E-R)
state<-c(S=S,E=E,I=I,R=R)
times<-c(1:52) # 52 weeks

mij<-func_convert_matrices(cij_all)
out_contact <- data.frame(ode(y = state, times = times, func = SEIR_model, parms = c(contact_result[1:5])))
mij<-func_convert_matrices(tu_all1)
out_tu <- data.frame(ode(y = state, times = times, func = SEIR_model, parms = c(tu_result[1:5])))

mij<-func_convert_matrices(cij_all)
tij<-func_convert_matrices(tu_all1)
out_suitable_cont <- data.frame(ode(y = state, times = times, func = SEIR_model_2, parms = suitable_contact_result[1:2]))  

out_contact<-out_contact[,grepl("I",names(out_contact))]
out_tu<-out_tu[,grepl("I",names(out_tu))]
out_suitable_cont<-out_suitable_cont[,grepl("I",names(out_suitable_cont))]

names<-c("[0-4] years old",
         "[5-14] years old",
         "[15-19] years old",
         "[20-64] years old",
         "65+ years old")

matplot(times, out_contact[,grepl("I",names(out_contact))]/sum(N),type="l",ylab="estimated incidence",xlab="week")
legend(30, 0.008, legend = names, col = 1:5, lty = 1:5,cex=0.7)

matplot(times, out_tu[,grepl("I",names(out_tu))]/sum(N),type="l",ylab="estimated incidence",xlab="week")
legend(30, 0.008, legend = names, col = 1:5, lty = 1:5,cex=0.7)

matplot(times, out_suitable_cont[,grepl("I",names(out_suitable_cont))]/sum(N),type="l",ylab="estimated incidence",xlab="week")
legend(30, 0.008, legend = names, col = 1:5, lty = 1:5,cex=0.7)


plot(times,rowSums(obs_inc[,grepl("years old",names(obs_inc))])/sum(N),type="p",col="blue",ylab = "Incidence rate",xlab="week",ylim=c(0,0.004))
lines(times, (contact_result[6])*rowSums(out_contact[,grepl("I",names(out_contact))])/sum(N),type="l",col="blue",lwd=2)
lines(times, (tu_result[6])*rowSums(out_tu[,grepl("I",names(out_tu))])/sum(N),type="l",col="red",lwd=2)
lines(times, (suitable_contact_result[3]+0.008)*rowSums(out_suitable_cont[,grepl("I",names(out_suitable_cont))])/sum(N),type="l",col="black",lwd=2)
legend(30, 0.003, legend = c("Combined approach","TU approach","Contact approach"), col = c("black","red","blue"), lty = 1,cex=0.9,lwd=2)

########fit suitable contacts incorporating physical and non-physical contacts;
# source("function\\func_fit_suitable_contact_physical_non_physical.R")
# start<-Sys.time()
# func_fit_suitable_contact_physical_non_physical(cij_all, cij_close,cij_non_close,tu_all1,c(0.4,0.3,0.9,0.8,0.3))
# Sys.time() - start

###################################################
#######PLOT age-specific fitting with LS values####
###################################################

#         beta     scale     ls_value       R0
# [1,] 0.0861672965 0.2741115327 0.0001064882 1.4432751979 

opt_parm<-contact_result[1:6]
opt_scale<-opt_parm[6]
mij<-func_convert_matrices(cij_all)


opt_parm<-tu_result[1:6]
opt_scale<-opt_parm[6]
mij<-func_convert_matrices(tu_all1)

out_tu <- data.frame(ode(y = state, times = times, func = SEIR_model, parms = opt_parm))

names<-c("[0-4] years old",
         "[5-14] years old",
         "[15-19] years old",
         "[20-64] years old",
         "65+ years old")

matplot(times, out_tu[,grepl("I",names(out_tu))]/sum(N),type="l",ylab="estimated incidence",xlab="week")
legend(30, 0.008, legend = names, col = 1:5, lty = 1:5,cex=0.7)

######
# par(mfrow=c(2,3),tck=-0.03,mgp=c(0,0,0))
age_gr<-1:5
main_tag <- c('Total population',
              'Age 0-4 years',
              'Age 5-14 years',
              'Age 15-19 years',
              'Age 20-64 years',
              "65+ years old")

obs_tag <- c(paste(sum(obs_inc[,grepl("years old",names(obs_inc))]),"cases"),
             paste(sum(obs_inc[,grepl("years old",names(obs_inc))][1]),"cases"),
             paste(sum(obs_inc[,grepl("years old",names(obs_inc))][2]),"cases"),
             paste(sum(obs_inc[,grepl("years old",names(obs_inc))][3]),"cases"),
             paste(sum(obs_inc[,grepl("years old",names(obs_inc))][4]),"cases"),
             paste(sum(obs_inc[,grepl("years old",names(obs_inc))][5]),"cases"))

fitted_tag <-c(paste(sum(round(opt_scale*sum(out_tu[,grepl("I",names(out_tu))]),0)),"cases"),
               paste(sum(round(opt_scale*sum(out_tu[,grepl("I",names(out_tu))][1]),0)),"cases"),
               paste(sum(round(opt_scale*sum(out_tu[,grepl("I",names(out_tu))][2]),0)),"cases"),
               paste(sum(round(opt_scale*sum(out_tu[,grepl("I",names(out_tu))][3]),0)),"cases"),
               paste(sum(round(opt_scale*sum(out_tu[,grepl("I",names(out_tu))][4]),0)),"cases"),
               paste(sum(round(opt_scale*sum(out_tu[,grepl("I",names(out_tu))][5]),0)),"cases"))

ratio_tag <- c(paste("ratio:",round(sum(obs_inc[,grepl("years old",names(obs_inc))])/
                                      sum(opt_scale*sum(out_tu[,grepl("I",names(out_tu))])),2)),
               paste("ratio:",round(sum(obs_inc[,grepl("years old",names(obs_inc))][1])/
                                      sum(opt_scale*sum(out_tu[,grepl("I",names(out_tu))][1])),2)),
               paste("ratio:",round(sum(obs_inc[,grepl("years old",names(obs_inc))][2])/
                                      sum(opt_scale*sum(out_tu[,grepl("I",names(out_tu))][2])),2)),
               paste("ratio:",round(sum(obs_inc[,grepl("years old",names(obs_inc))][3])/
                                      sum(opt_scale*sum(out_tu[,grepl("I",names(out_tu))][3])),2)),
               paste("ratio:",round(sum(obs_inc[,grepl("years old",names(obs_inc))][4])/
                                      sum(opt_scale*sum(out_tu[,grepl("I",names(out_tu))][4])),2)),
               paste("ratio:",round(sum(obs_inc[,grepl("years old",names(obs_inc))][5])/
                                      sum(opt_scale*sum(out_tu[,grepl("I",names(out_tu))][5])),2))
)


# pop_matrix<-matrix(rep(N,52),ncol=5,byrow=T)
# 
# ll<-NULL
# for(i in 1:5){
#   I_obs<-obs_inc[,grepl("years old",names(obs_inc))][i]
#   I_est<-opt_scale*out_tu[,grepl("I",names(out_tu))][i]
#   ll[i] <- sum(I_obs*log(I_est) -  I_est)
#   # AIC[i] <- 2*(2 - ll)
# }
# 
# ll_all <- round(c(sum(ll),ll),0)
# WLS_tag<-NULL
# for(i in 1:6){
#   WLS_tag[i]<- paste0("WLS:",WLS[i],"e-5")
# }

plot_func<-function(age_gr,main_tag,obs_tag, fitted_tag,ratio_tag, WLS_tag){
  plot(times,rowSums(obs_inc[,grepl("years old",names(obs_inc))][age_gr])/N[age_gr],type="l",col="blue",ylab = "Incidence rate",
       xlab="week",ylim=c(0,max(rowSums(obs_inc[,grepl("years old",names(obs_inc))][age_gr])/N[age_gr],
                                opt_scale*rowSums(out_tu[,grepl("I",names(out_tu))][age_gr])/N[age_gr])))
  y_max<-max(rowSums(obs_inc[,grepl("years old",names(obs_inc))][age_gr])/N[age_gr],
             opt_scale*rowSums(out_tu[,grepl("I",names(out_tu))][age_gr])/N[age_gr])
  lines(times, opt_scale*rowSums(out_tu[,grepl("I",names(out_tu))][age_gr])/N[age_gr],type="l",col="red",lwd=2)
  mtext(side = 3, line = 0.2, main_tag,cex=0.7)
  text(30,y_max-y_max/10, obs_tag, cex=0.8, col=c("blue"), lty=c(1.5,1.5),lwd = c(2,2))
  text(30,y_max-y_max/6, fitted_tag, cex=0.8, col=c("red"), lty=c(1.5,1.5),lwd = c(2,2))
  text(47,y_max-y_max/10, ratio_tag, cex=0.8, col=c("black"), lty=c(1.5,1.5),lwd = c(2,2))
  # text(30,y_max, WLS_tag, cex=0.8, col=c("black"), lty=c(1.5,1.5),lwd = c(2,2))
}


####Multiple plots####
windows(height=12/2.54,width=15/2.54)
par(mar=c(3, 1.8,2, 1), mfrow=c(2,3),
    oma = c(0, 2, 1, 0),tck=-0.03,mgp=c(1,0.1,0))
##total population
y_max<-max(rowSums(obs_inc[,grepl("years old",names(obs_inc))])/sum(N),
           opt_scale*rowSums(out_tu[,grepl("I",names(out_tu))])/sum(N)) 
plot(times,rowSums(obs_inc[,grepl("years old",names(obs_inc))])/sum(N),type="l",col="blue",
     ylab = "Incidence rate",xlab="week",ylim=c(0,y_max))
lines(times, opt_scale*rowSums(out_tu[,grepl("I",names(out_tu))])/sum(N),type="l",col="red",lwd=2)
mtext(side = 3, line = 0.2, main_tag[1],cex=0.7)
text(30,y_max-y_max/10, obs_tag[1], cex=0.8, col=c("blue"), lty=c(1.5,1.5),lwd = c(2,2))
text(30,y_max-y_max/6, fitted_tag[1], cex=0.8, col=c("red"), lty=c(1.5,1.5),lwd = c(2,2))
text(47,y_max-y_max/10, ratio_tag[1], cex=0.8, col=c("black"), lty=c(1.5,1.5),lwd = c(2,2))
# text(30,y_max, WLS_tag[1], cex=0.8, col=c("black"), lty=c(1.5,1.5),lwd = c(2,2))

for(i in 2:6){
  plot_func(age_gr[i-1],main_tag[i],obs_tag[i], fitted_tag[i],ratio_tag[i])
}



########################
########PLOT CI################

# SE<-contact_result$se
# opt_parm<-contact_result$main[1:5]
# opt_scale<-contact_result$main[7]
# mij<-func_convert_matrices(cij_all)
# out_tu <- data.frame(ode(y = state, times = times, func = SEIR_model, parms = opt_parm))
# out_data<-out_tu
# 
# 
# opt_scale_lower<-opt_scale-1.96*SE[7]
# opt_scale_upper<-opt_scale+1.96*SE[7]
# out_data_lower<-data.frame(ode(y = state, times = times, func = SEIR_model, parms = (opt_parm-1.96*SE[1:5])))
# out_data_upper<-data.frame(ode(y = state, times = times, func = SEIR_model, parms = (opt_parm+1.96*SE[1:5])))

opt_parm<-contact_result$main[1:5]
dispersion <-contact_result$main[6]
opt_scale<-contact_result$main[7]

out_tu <- data.frame(ode(y = state, times = times, func = SEIR_model, parms = opt_parm))
out_data<-out_tu
I_obs<-obs_inc[,grepl("years old",names(obs_inc))]

I_est<-opt_scale*out_tu[,grepl("I",names(out_tu))]


library(plyr)
raply(10000,rnbinom(n=length(rowSums(I_obs)),
                    mu=rowSums(I_est),
                    size=1/dispersion)) -> simdat_total
aaply(simdat_total,2,quantile,probs=c(0.025,0.5,0.975)) -> quantiles_total

raply(10000,rnbinom(n=dim(I_obs)[1]*dim(I_obs)[2],
                    mu=unlist(I_est),
                    size=1/dispersion)) -> simdat_age_gr
aaply(simdat_age_gr,2,quantile,probs=c(0.025,0.5,0.975)) -> quantiles_age_gr
quantiles_age_gr_ll<-matrix(quantiles_age_gr[,1],ncol =5,byrow = F )
quantiles_age_gr_ul<-matrix(quantiles_age_gr[,3],ncol =5,byrow = F )


#######

age_gr<-1:5
main_tag <- c('Total population',
              'Age 0-4 years',
              'Age 5-14 years',
              'Age 15-19 years',
              'Age 20-64 years',
              "65+ years old")

obs_tag <- c(paste(sum(obs_inc[,grepl("years old",names(obs_inc))]),"cases"),
             paste(sum(obs_inc[,grepl("years old",names(obs_inc))][1]),"cases"),
             paste(sum(obs_inc[,grepl("years old",names(obs_inc))][2]),"cases"),
             paste(sum(obs_inc[,grepl("years old",names(obs_inc))][3]),"cases"),
             paste(sum(obs_inc[,grepl("years old",names(obs_inc))][4]),"cases"),
             paste(sum(obs_inc[,grepl("years old",names(obs_inc))][5]),"cases"))

fitted_tag <-c(paste(sum(round(rowSums(opt_scale*out_data[,grepl("I",names(out_data))]),0)),"cases"),
               paste(sum(round(opt_scale*sum(out_data[,grepl("I",names(out_data))][1]),0)),"cases"),
               paste(sum(round(opt_scale*sum(out_data[,grepl("I",names(out_data))][2]),0)),"cases"),
               paste(sum(round(opt_scale*sum(out_data[,grepl("I",names(out_data))][3]),0)),"cases"),
               paste(sum(round(opt_scale*sum(out_data[,grepl("I",names(out_data))][4]),0)),"cases"),
               paste(sum(round(opt_scale*sum(out_data[,grepl("I",names(out_data))][5]),0)),"cases"))

ratio_tag <- c(paste0("ratio:",round(sum(obs_inc[,grepl("years old",names(obs_inc))])/
                                       sum(round(rowSums(opt_scale*out_data[,grepl("I",names(out_data))]),0)),2)),
               paste0("ratio:",round(sum(obs_inc[,grepl("years old",names(obs_inc))][1])/
                                       sum(opt_scale*sum(out_data[,grepl("I",names(out_data))][1])),2)),
               paste0("ratio:",round(sum(obs_inc[,grepl("years old",names(obs_inc))][2])/
                                       sum(opt_scale*sum(out_data[,grepl("I",names(out_data))][2])),2)),
               paste0("ratio:",round(sum(obs_inc[,grepl("years old",names(obs_inc))][3])/
                                       sum(opt_scale*sum(out_data[,grepl("I",names(out_data))][3])),2)),
               paste0("ratio:",round(sum(obs_inc[,grepl("years old",names(obs_inc))][4])/
                                       sum(opt_scale*sum(out_data[,grepl("I",names(out_data))][4])),2)),
               paste0("ratio:",round(sum(obs_inc[,grepl("years old",names(obs_inc))][5])/
                                       sum(opt_scale*sum(out_data[,grepl("I",names(out_data))][5])),2))
)

mae_tag<-c(paste0("MAE:",round(mae(unlist(obs_inc[,grepl("years old",names(obs_inc))]),
                                   unlist(round(opt_scale*out_data[,grepl("I",names(out_data))],0))),0)),
           paste0("MAE:",round(mae(unlist(obs_inc[,grepl("years old",names(obs_inc))][1]),
                                   unlist(round(opt_scale*out_data[,grepl("I",names(out_data))][1],0))),0)),
           paste0("MAE:",round(mae(unlist(obs_inc[,grepl("years old",names(obs_inc))][2]),
                                   unlist(round(opt_scale*out_data[,grepl("I",names(out_data))][2],0))),0)),
           paste0("MAE:",round(mae(unlist(obs_inc[,grepl("years old",names(obs_inc))][3]),
                                   unlist(round(opt_scale*out_data[,grepl("I",names(out_data))][3],0))),0)),
           paste0("MAE:",round(mae(unlist(obs_inc[,grepl("years old",names(obs_inc))][4]),
                                   unlist(round(opt_scale*out_data[,grepl("I",names(out_data))][4],0))),0)),
           paste0("MAE:",round(mae(unlist(obs_inc[,grepl("years old",names(obs_inc))][5]),
                                   unlist(round(opt_scale*out_data[,grepl("I",names(out_data))][5],0))),0))
)

# pop_matrix<-matrix(rep(N,52),ncol=5,byrow=T)
# # ssr<-sum((opt_scale*out_data[,grepl("I",names(out_data))]/pop_matrix-obs_inc[,grepl("years old",names(obs_inc))]/pop_matrix)^2)
# ssr1<-NULL
# for(i in 1:5){
#   ssr1[i]<-sum((opt_scale*out_data[,grepl("I",names(out_data))][,i]/pop_matrix[,i]-obs_inc[,grepl("years old",names(obs_inc))][,i]/pop_matrix[,i])^2)
# }

# ssr<-sum(ssr1)
# 
# WLS <- round(c(ssr,ssr1)*10^5,2)
# WLS_tag<-NULL
# for(i in 1:6){
#   WLS_tag[i]<- paste0("LS:",WLS[i],"e-5")
# }

# age_gr<-age_gr[4];main_tag=main_tag[4];obs_tag=obs_tag[4]
# fitted_tag=fitted_tag[4];ratio_tag=ratio_tag[4]; WLS_tag=WLS_tag[4]

plot_func<-function(age_gr,main_tag,obs_tag, fitted_tag,ratio_tag,mae_tag, WLS_tag){
  y_max<-max(rowSums(obs_inc[,grepl("years old",names(obs_inc))][age_gr])/N[age_gr],
             rev(quantiles_age_gr_ul[,age_gr])/N[age_gr])
  plot(times,rowSums(obs_inc[,grepl("years old",names(obs_inc))][age_gr])/N[age_gr],type="l",col="blue",ylab = "Incidence rate",
       xlab="week",ylim=c(0,y_max))
  polygon(c(rev(times), times), c(rev(quantiles_age_gr_ul[,age_gr])/N[age_gr], quantiles_age_gr_ll[,age_gr]/N[age_gr]), col = 'grey88', border = NA)
  lines(times, rowSums(obs_inc[,grepl("years old",names(obs_inc))][age_gr])/N[age_gr],type="l",col="blue",lwd=2)
  lines(times, opt_scale*rowSums(out_data[,grepl("I",names(out_data))][age_gr])/N[age_gr],type="l",col="red",lwd=2)
  # lines(times, rowSums(opt_scale_lower*out_data_lower[,grepl("I",names(out_data_lower))][age_gr]),type="l",col="grey",lwd=2)
  # lines(times, rowSums(opt_scale_upper*out_data_upper[,grepl("I",names(out_data_upper))][age_gr]),type="l",col="grey",lwd=2)
  mtext(side = 3, line = 0.2, main_tag,cex=0.7)
  text(30,y_max-y_max/10, obs_tag, cex=0.8, col=c("blue"), lty=c(1.5,1.5),lwd = c(2,2))
  text(30,y_max-y_max/6, fitted_tag, cex=0.8, col=c("red"), lty=c(1.5,1.5),lwd = c(2,2))
  text(47,y_max-y_max/10, ratio_tag, cex=0.8, col=c("black"), lty=c(1.5,1.5),lwd = c(2,2))
  text(47,y_max-y_max/6, mae_tag, cex=0.8, col=c("black"), lty=c(1.5,1.5),lwd = c(2,2))
  # text(29,y_max, WLS_tag, cex=0.8, col=c("black"), lty=c(1.5,1.5),lwd = c(2,2))
}


####Multiple plots####
windows(height=12/2.54,width=15/2.54)
par(mar=c(3, 1.8,2, 1), mfrow=c(2,3),
    oma = c(0, 2, 1, 0),tck=-0.03,mgp=c(1,0.1,0))
##total population
y_max<-max(rowSums(obs_inc[,grepl("years old",names(obs_inc))])/sum(N),
           rev(quantiles[,3])/sum(N)) 
plot(times,rowSums(obs_inc[,grepl("years old",names(obs_inc))]),type="l",col="blue",
     ylab = "Incidence rate",xlab="week",ylim=c(0,y_max))
polygon(c(rev(times), times), c(rev(quantiles[,3])/sum(N), quantiles[,1]/sum(N)), col = 'grey88', border = NA)
lines(times, rowSums(obs_inc[,grepl("years old",names(obs_inc))])/sum(N),type="l",col="blue",lwd=2)
lines(times, rowSums(opt_scale*out_data[,grepl("I",names(out_data))])/sum(N),type="l",col="red",lwd=2)
mtext(side = 3, line = 0.2, main_tag[1],cex=0.7)
text(30,y_max-y_max/10, obs_tag[1], cex=0.8, col=c("blue"), lty=c(1.5,1.5),lwd = c(2,2))
text(30,y_max-y_max/6, fitted_tag[1], cex=0.8, col=c("red"), lty=c(1.5,1.5),lwd = c(2,2))
text(47,y_max-y_max/10, ratio_tag[1], cex=0.8, col=c("black"), lty=c(1.5,1.5),lwd = c(2,2))
text(47,y_max-y_max/6, mae_tag[1], cex=0.8, col=c("black"), lty=c(1.5,1.5),lwd = c(2,2))
# text(29,y_max, WLS_tag[1], cex=0.8, col=c("black"), lty=c(1.5,1.5),lwd = c(2,2))

for(i in 2:6){
  plot_func(age_gr[i-1],main_tag[i],obs_tag[i], fitted_tag[i],ratio_tag[i], mae_tag[i],WLS_tag[i])
}




