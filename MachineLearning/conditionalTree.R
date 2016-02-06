

source("../start.R")

library(party)
library(tidyr)
 
load("../res.final.RData")

## reduce to relevant variables, drop unused factor levels
res2 <- res.final %>% droplevels()

# reshape long format to wide format
res.wide <- dcast(res2, id + condition + k + delta + qrpEnv + selProp + tau + method ~ variable, value.var="value")
head(res.wide)

# final data set in wide format:
save(res.wide, file="res.wide.RData")



 
#make the wide data frame for the tree
ID = unique(res.wide$id)
N = length(ID)
dat = matrix(NA,N,11)
dat = data.frame(dat)
colnames(dat)=c('delta','RE','TF','kFilled','PET','PEESE','pcurve','tau','k','kPosSig','FAT')
 
for (i in 1:N){
  dat$delta[i] = res.wide$delta[res.wide$id==ID[i]][1]   
  dat$RE[i] = res.wide$d[res.wide$id==ID[i] & res.wide$method=='RE']
  dat$TF[i] = res.wide$d[res.wide$id==ID[i] & res.wide$method=='TF']
  dat$kFilled[i] = res.wide$kFilled[res.wide$id==ID[i] & res.wide$method=='fill']
  dat$PET[i] = res.wide$d[res.wide$id==ID[i] & res.wide$method=='PET']
  dat$PEESE[i] = res.wide$d[res.wide$id==ID[i] & res.wide$method=='PEESE']
  dat$pcurve[i] = res.wide$d[res.wide$id==ID[i] & res.wide$method=='pcurve']
  dat$tau[i] = res.wide$tauEst[res.wide$id==ID[i] & res.wide$method=='Tau']
  dat$k[i] = res.wide$k[res.wide$id==ID[i]][1]
  dat$kPosSig[i] = res.wide$sig.studies[res.wide$id==ID[i] & res.wide$method=='pcurve']
  dat$FAT[i] = res.wide$p.value[res.wide$id==ID[i] & res.wide$method=='FAT']   
}

#run the tree
meTree = ctree(delta ~ ., data=dat) 
plot(meTree)
 
