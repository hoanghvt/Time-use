#===============================================================================
#>>>> Diary weights function
#===============================================================================
DiaryWeightFun <- function(PopAgeHS, PartData, Var1, Var2){
#===============================================================================
FBPopHSAge <- PopAgeHS # Both regions 
PopSize <- as.vector(colSums(FBPopHSAge))# Pop sizes across each age groups

# Creating categories of age and household size
PartData$agecat2 <- cut(PartData[,Var1], breaks=seq(0,100,5),
                       right=FALSE, include.lowest=TRUE)
agelev <- levels(PartData$agecat2)

PartData$HScat <- cut(PartData[,Var2], breaks=c(seq(1,11,1), 20),
                       right=FALSE, include.lowest=TRUE)
HSlev <- levels(PartData$HScat)

# Creating HHsize by Age table of the participant data
HSAgeSamp <- table(PartData$HScat, PartData$agecat2)
SampSize <- as.vector(colSums(HSAgeSamp)) # Size acrosss age groups

# pfraction = Cell counts of FBPopHSAge/total population size
for(j in 1:dim(FBPopHSAge)[2]){
  for(k in 1:dim(FBPopHSAge)[1]){
    PartData$pfraction[PartData$agecat2==agelev[j] & 
                          PartData$HScat==HSlev[k]] <-
      FBPopHSAge[k,j]/sum(PopSize)
  }
}

# sfraction= cell counts of HSAgeSamp/total sample size
for(s in 1:dim(HSAgeSamp)[2]) {
  for(t in 1:dim(HSAgeSamp)[1]) {
    PartData$sfraction[PartData$agecat2==agelev[s] &
                          PartData$HScat==HSlev[t]] <-
      HSAgeSamp[t,s]/sum(SampSize)
  }
}

# Assigning crude weights=pop density/sample density except HSize=NA
PartData$diary_weight <- PartData$pfraction/PartData$sfraction
# Assigning crude weights = pop density across age groups / sample density across age groups for Hsize=NA 
mar_weight <- (PopSize/sum(PopSize))/(SampSize/sum(SampSize))
for(j in 1:dim(HSAgeSamp)[2]){
  PartData$diary_weight[PartData$agecat2==agelev[j] &
                           is.na(PartData$HScat)] <- mar_weight[j]
}
# Standardizing diary weights
PartData$diary_weight <- (PartData$diary_weight*length(PartData$diary_weight))/sum(PartData$diary_weight)

#=================================================================================
#>>>>> Assessing weights before and after truncation 2006
#=================================================================================
## Checking weights above 3 before truncation
#### Truncating weights 
PartData$diary_weight[PartData$diary_weight >=3] = 3 

#### Standardizing diary weights
PartData$diary_weight <- (PartData$diary_weight*length(PartData$diary_weight))/sum(PartData$diary_weight)

#=================================================================================
#>>>>> Variables to be excluded
#=================================================================================
remo <- c("agecat2", "HScat", "pfraction", "sfraction")# Vars to exclude 
PartData <- PartData[, !(colnames(PartData) %in% remo)]

return(PartData)
}

