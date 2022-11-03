func_fit_suitable_contact<-function(cij,tij,initial_value){
  
  # cij<-get(load("contact_matrices\\contact_all_2.Rdata"))[,-1]
  # tij <-get(load("MI_time_use\\aij_general_MI1.Rdata"))[1:90,1:90]
  
  mij<-func_convert_matrices(cij)
  tij<-func_convert_matrices(tij)
  
  
  
  # ###Load population data
  # pop<-read.csv("data\\pop_2011.txt",head=T,sep="\t")
  # int.breaks<-c(0,5,15,20,65,99)
  # N=tapply(pop[1:(99+1),2],c(rep(c(1:(length(int.breaks)-1)),diff(int.breaks),each=T),length(int.breaks)-1),sum)
  # 
  # ###Load observed incidence rate
  # #note: already took account for population coverage of GP:1.75%
  # obs_inc<-read.csv("data\\observed_weekly_cases_2010_2011.csv",head=T,sep=",")
  # names(obs_inc)[grepl("years.old",names(obs_inc))]<-c("[0-4] years old",
  #                                                      "[5-14] years old",
  #                                                      "[15-19] years old",
  #                                                      "[20-64] years old",
  #                                                      "65+ years old")
  # 
  
  ###SEIR model
  SEIR_model<-function(t,state,parms){
    beta=parms[1:5]
    q2=parms[6]
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
        dS[i]<- -beta[i]*S[i]*sum(mij[i,]*(1-exp(-q2*tij[i,]/mij[i,]))*I/N)
        dE[i] <- beta[i]*S[i]*sum(mij[i,]*(1-exp(-q2*tij[i,]/mij[i,]))*I/N) - (7)*E[i] ###latent period: 1 day
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
  
  #####Fit model to the data####
  
  model<- function(pars){
    out <- data.frame(ode(y = state, times = times, func = SEIR_model, parms = pars[1:6]))
    pop_matrix<-matrix(rep(N,52),ncol=5,byrow=T)
    ssr<-sum((pars[7]*out[,grepl("I",names(out))]/pop_matrix-obs_inc[,grepl("years old",names(obs_inc))]/pop_matrix)^2)
    return(ssr)
  }
  
  # initial_value<-  rep(0.3,7)
  parms<-initial_value
  
  ##use Nelder Mead
  require(pracma)
  require(lme4)
  start<-Sys.time()
  Nel_Me<-Nelder_Mead(model,parms, lower = rep.int(0, 7), upper = rep.int(Inf, 7),
                      control = list())
  Sys.time()-start
  
  
  # Nel_Me$fval
  # Nel_Me$par
  suitable_matrix<-matrix(rep(Nel_Me$par[1:5],5),ncol=5,byrow = T)*mij*(1-exp(-Nel_Me$par[6]*tij/mij))
  R0<-max(eigen(suitable_matrix)$value)*3.8/7
  results<-c(Nel_Me$par,Nel_Me$fval,R0)
  names(results)<-c(paste0("beta",1:5),"q2","scale","LS","R0")
  
  #use nlm()
  # result<-nlm(model,parms,hessian = TRUE)
  # AIC<- 2*(length(parms)-result$minimum)
  
  # start<-Sys.time()
  # Opt <- optim(parms,
  #              model,
  #              method = "L-BFGS-B",lower = rep(0,7),hessian = TRUE,
  #              control = list(parscale = rep(10^-4,7),
  #                             factr = 1))
  # Sys.time()-start
  # 
  # Opt$value
  # AIC<-2*(length(parms)+Opt$value)
  # Opt$par
  # R0<-max(eigen(mij*(1-exp(-Opt$par[2]*tij/mij)))$value)*Opt$par[1]*3.8/7
  # results<-c(Opt$par,AIC,R0)
  # names(results)<-c("beta","q2","scale","AIC","R0")
  return(results)
}