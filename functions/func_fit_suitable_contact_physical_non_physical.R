func_fit_suitable_contact_physical_non_physical<-function(cij,cij_physical,cij_non_physical,tij,initial_value){
  
  mij<-func_convert_matrices(cij)
  mij1<-func_convert_matrices(cij_physical)
  mij2<-func_convert_matrices(cij_non_physical)
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
    beta=parms[1]
    q2=parms[2]
    q3=parms[3]
    q4=parms[4]
    
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
        dS[i]<- -beta*S[i]*sum((q3*mij1[i,]+q4*mij2[i,])*(1-exp(-q2*tij[i,]/mij[i,]))*I/N)
        dE[i] <- beta*S[i]*sum((q3*mij1[i,]+q4*mij2[i,])*(1-exp(-q2*tij[i,]/mij[i,]))*I/N) - (7)*E[i] ###latent period: 1 day
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
    out <- data.frame(ode(y = state, times = times, func = SEIR_model, parms = pars[1:4]))
    pop_matrix<-matrix(rep(N,52),ncol=5,byrow=T)
    ssr<-sum((pars[5]*out[,grepl("I",names(out))]/pop_matrix-obs_inc[,grepl("years old",names(obs_inc))]/pop_matrix)^2)
    return(ssr)
  }
  
  # initial_value<-  c(0.4,0.3,0.8,0.9,0.27)
  parms<-initial_value
  
  start<-Sys.time()
  Opt <- optim(parms,
               model,
               method = "L-BFGS-B",lower = c(0, 0,0,0,0),hessian = TRUE,
               control = list(parscale = c(10^-4,10^-4,10^-4,10^-4,10^-4),
                              factr = 1))
  Sys.time()-start
  
  Opt$value
  Opt$par
  R0<-max(eigen((Opt$par[3]*mij1+Opt$par[4]*mij2) *(1-exp(-Opt$par[5]*tij/mij)))$value)*Opt$par[1]*3.8/7
  results<-c(Opt$par,Opt$value,R0)
  names(results)<-c("beta","q2","q3","q4","scale","ls_value","R0")
  return(results)
}