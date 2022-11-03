
func_convert_matrices<-function(cij){
  pop<-read.csv("data\\pop_2011.txt",head=T,sep="\t")
  wj<-pop[,2]
  L=89
  # Choice of age categories [(x),(x+1)[ for the discrete matrix, the max right border is L
  int.left=c(0, 3, 6, 12,18,25,30,35,40,45,50,55,60,65,70,75,80); nbins=length(int.left)
  int.breaks=c(int.left,L)
  
  ## tapply(Summary Variable, Group Variable, Function)
  wj.int=tapply(wj[1:(L+1)],c(rep(c(1:(length(int.breaks)-1)),diff(int.breaks),each=T),length(int.breaks)-1),sum)
  
  int.lengths=diff(int.breaks); int.lengths[length(int.lengths)]=int.lengths[length(int.lengths)]+1
  wj.cat=rep(wj.int/int.lengths,int.lengths,each=T)
  Wj<-t(matrix(rep(wj.cat,L+1),ncol=L+1))
  
  mij_90<-cij*Wj
  
  func_age_classes<-function(x){
    int.breaks<-c(0,5,15,20,65,89)
    age_group<-tapply(x,c(rep(c(1:(length(int.breaks)-1)),diff(int.breaks),each=T),length(int.breaks)-1),sum)
    return(age_group)
  }
  
  age_contact<-as.matrix(t(apply(mij_90,1,func_age_classes)))
  age_part<-as.matrix(t(apply(age_contact,2,func_age_classes)))/diff(c(0,5,15,20,65,89))
  
    ##calculate cij
  int.breaks<-c(0,5,15,20,65,89)
  wj.int=tapply(wj[1:(L+1)],c(rep(c(1:(length(int.breaks)-1)),diff(int.breaks),each=T),length(int.breaks)-1),sum)
  # 
  Wj<-t(matrix(rep(wj.int,5),ncol=5))
  mij<-((age_part*t(Wj)+t(age_part)*Wj)/2)/t(Wj)
  # cij<-mij/Wj
  
  return(mij)
}