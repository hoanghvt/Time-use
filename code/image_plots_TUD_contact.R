
setwd("C:\\Users\\lucp9032\\Desktop\\TIME USE FOLDER\\CODE_MI_BOOSTRAP\\THANG\\output\\MI_exposure_time")

TU_all_matrix<-as.matrix(get(load("aij_general_MI1.RDATA")))
TU_home_matrix<-as.matrix(get(load("aij_home_MI1.RDATA")))
cont_matrix<-as.matrix(get(load("contact_all_2.RDATA"))[,-1])


popcount2011 <- read.delim("C:\\Users\\lucp9032\\Desktop\\TIME USE FOLDER\\CODE_MI_BOOSTRAP\\THANG\\data\\2011_pop_ages_FLBR.txt")
wj2010 <- popcount2011$popFLBR[1:(89+1)] 
int.breaks2010<-c(0, 3, 6, 12,18,25,30,35,40,45,50,55,60,65,70,75,80,89)
int.lengths=diff(int.breaks2010)
wj.int2010 <-tapply(wj2010[1:(89+1)],c(rep(c(1:(length(int.breaks2010)-1)),diff(int.breaks2010),each=T),length(int.breaks2010)-1),sum)
wj.cat=rep(wj.int2010,1,each=T)
Wj<-t(matrix(rep(wj.cat,17),ncol=17))


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

Wj_100<-func(Wj)

tij_all<-TU_all_matrix*Wj_100
table<-tij_all[c(0,3,6,12,18,25,30,35,40,45,50,55,60,65,70,75,80)+1,c(0,3,6,12,18,25,30,35,40,45,50,55,60,65,70,75,80)+1]
write.table(table, "clipboard", sep="\t", row.names=FALSE)

tij_home<-TU_home_matrix*Wj_100
cont_mij<-cont_matrix*Wj_100

mij<-cont_mij[c(1, 4, 7, 13,19,26,31,36,41,46,51,56,61,66,71,76,81),c(1, 4, 7, 13,19,26,31,36,41,46,51,56,61,66,71,76,81)]

Q_index<-function(matrix){
  a<-t(matrix) / colSums(matrix)
  Q<-(tr(a)-1)/16
  return(Q)
}

library(psych)
Q_index(mij)

######PLOTS######
library(colorRamps)
library(grDevices)
library(npsp)
library(RColorBrewer)

####plot continuous age
redc <- rev(heat.colors(100))
library(grDevices)
windows(width=18, height=9)
par(mfrow=c(1,2), mar=c(4.3, 6, 2, 2), oma=c(2,0.1,2,4), cex.axis=1.5, cex.lab=2,
    cex.main=2, cex.sub=1.25)

simage(c(0:90),c(0:90), tij_all, xlab="age of participant",
       ylab="age of contact", legend.width=3,
       slim=c(min(c(tij_all,cont_mij)), max(c(cont_mij,tij_all))), cex=1.5,
       col=redc, main="a)")

simage(c(0:90),c(0:90), cont_mij, xlab="age of participant",
       ylab=" ", legend.width=3,
       slim=c(min(c(cont_mij,tij_all)), max(c(cont_mij,tij_all))), cex=1.5,
       col=redc, main="b)")


simage(c(0:90),c(0:90), tij_home, xlab="",
       ylab="age of contact", legend.width=3,
       slim=c(min(c(tij_all,tij_home)), max(c(tij_all,tij_home))), cex=1.5,
       col=redc, main="Time use at home")


####plot age groups
cont_matrix1<-cont_matrix[c(1,4,7,13,19,26,31,36,41,46,51,56,61,66,71,76,81),c(1,4,7,13,19,26,31,36,41,46,51,56,61,66,71,76,81)]
tij_all_gr<-tij_all[c(1,4,7,13,19,26,31,36,41,46,51,56,61,66,71,76,81),c(1,4,7,13,19,26,31,36,41,46,51,56,61,66,71,76,81)]
tij_home_gr<-tij_home[c(1,4,7,13,19,26,31,36,41,46,51,56,61,66,71,76,81),c(1,4,7,13,19,26,31,36,41,46,51,56,61,66,71,76,81)]
cont_mij_gr<-cont_mij[c(1,4,7,13,19,26,31,36,41,46,51,56,61,66,71,76,81),c(1,4,7,13,19,26,31,36,41,46,51,56,61,66,71,76,81)]

L=16
Lv<-L+1

setwd("C:\\Users\\lucp9032\\Desktop\\TIME USE FOLDER\\CODE_MI_BOOSTRAP\\THANG")

devtools::install_github("lwillem/simid_rtools",force=F,quiet=T)
#devtools::uninstall(simid.rtools)
library('simid.rtools')

packages_list <- c('car',  
                   'VIM',   
                   'devtools')   

# input/output folder
data_folder    <- smd_file_path('data','survey_flanders2010')
output_folder <- smd_file_path('output','Figures')
part <- read.table(file.path(data_folder,"individualsurvey_participants_full.txt"), sep=",",header = TRUE)
# Exclude people living in an elderly/nursing home + 6 people aged 90+: to be consistent with the sample size used in 
# the Flemish social contact paper
part$residential_status<-as.character(part$residential_status)
part$residential_status[is.na(part$residential_status)]<-"missing"
part<-subset(part,residential_status!="elderly_home" & residential_status!="nursing_home")
part<-subset(part,participant_age<90)
dim(part) #1707

ref_agecat <- c(0, 3, 6, 12,18,25,30,35,40,45,50,55,60,65,70,75,80,90)
part$age_cat<-cut(part$participant_age, breaks=ref_agecat,right = FALSE)
levels(part$age_cat)

windows(width=18, height=9)
par(mfrow=c(1,2), mar=c(4.3, 6, 2, 2), oma=c(2,0.1,2,4), cex.axis=1.5, cex.lab=2,
    cex.main=2, cex.sub=1.25)
simage(c(0:L),c(0:L),tij_all_gr[1:Lv,1:Lv],xlab="age group",ylab="age group",
       col=rev(heat.colors(100)),cex.axis=0.1,cex.lab=1.5,legend.width=1,
       slim=c(min(c(tij_all,tij_home)), max(c(tij_all,tij_home))))
lablist<-as.vector(c(levels(part$age_cat)))
axis(2, at=seq(0, 16, by=1), labels = lablist,cex.axis=1.2)
axis(1, at=seq(0, 16, by=1), labels = lablist,cex.axis=1.2)

# simage(c(0:L),c(0:L),tij_home_gr[1:Lv,1:Lv],xlab="age group",ylab="age group",
#        col=rev(heat.colors(100)),cex.axis=0.1,cex.lab=1.5,legend.width=1,
#        slim=c(min(c(tij_all,tij_home)), max(c(tij_all,tij_home))))
# lablist<-as.vector(c(levels(part$age_cat)))
# axis(2, at=seq(0, 16, by=1), labels = lablist,cex.axis=1.1)
# axis(1, at=seq(0, 16, by=1), labels = lablist,cex.axis=1.1)

simage(c(0:L),c(0:L),cont_mij_gr[1:Lv,1:Lv],xlab="age participant",ylab="age contact",
       col=rev(heat.colors(100)),cex.axis=0.1,cex.lab=1.5,legend.width=1,
       slim=c(min(c(cont_mij_gr)), max(c(cont_mij_gr))))
lablist<-as.vector(c(levels(part$age_cat)))
axis(2, at=seq(0, 16, by=1), labels = lablist,cex.axis=1.1)
axis(1, at=seq(0, 16, by=1), labels = lablist,cex.axis=1.1)



















