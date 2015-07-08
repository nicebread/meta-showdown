#VERSION NOTES
#Data production no longer uses a "raw data approach" unless QRPs are requested.
#No longer records the number of studies file drawered. 


library(MASS)
library(pwr)
library(compiler)
library(truncnorm)
library(truncdist)
######################################################################

##########
# sample #
##########


#For biased sampling. Produces four sample sizes for four groups.
#The logic here is that the biased data collector has included a 
#categorical moderator and assigned half of the sample to this 
#group. 
sampB = function(minN,meanN,sdN){
  #get total sample size, N
  N=round(rtruncnorm(1, a=minN, mean = meanN, sd = sdN),0)
  #divide N into groups
  n1=ceiling(N/4); n2=ceiling((N/2)-n1)
  n3=ceiling((N-n1-n2)/2); n4=N-n1-n2-n3
  #get Ns, a vector of sample sizes for each group 
  Ns=c(n1,n2,n3,n4)
}


#Just produces two sample sizes for two groups. 
sampU = function(minN,meanN,sdN){
  #get total sample size, N
  N=round(rtruncnorm(1, a=minN, mean = meanN, sd = sdN),0)
  #divide N into *two* groups
  n1=ceiling(N/2); n2=N-n1
  #get Ns, a vector of sample sizes for each group 
  Ns=c(n1,n2)
}


sampB = cmpfun(sampB)
sampU = cmpfun(sampU)

######################################################################
#Experimental Data (Biased and Unbiased)
#Create a list where k entries contain (both) vector(s) of g1:g4

#For p-hack-able (bias) data. Note that it doesn't take sample size.
#Instead, it gives a maximum amount of values. The values taken
#are based on the sampB/sampU functions and opt. stopping. 
#Produces four matricies that contain two columns each for both DVs. 
expDataB = function(meanD,sigma,cbdv,maxN){
  D=rnorm(1,meanD,sigma)
  G = array(c(g1=mvrnorm(maxN,rep(D,2),matrix(c(1,cbdv,cbdv,1),2,2)),
              g2=mvrnorm(maxN,rep(0,2),matrix(c(1,cbdv,cbdv,1),2,2)),
              g3=mvrnorm(maxN,rep(D,2),matrix(c(1,cbdv,cbdv,1),2,2)),
              g4=mvrnorm(maxN,rep(0,2),matrix(c(1,cbdv,cbdv,1),2,2))),dim=c(maxN,2,4))
  }

expDataB = cmpfun(expDataB)



######################################################################

###########
# outlier #
###########

#Evaluates a number, x, relative to the mean and sd of the vector that
#x is a part of. 

outlier=function(x,mean,sd){
  out=if(abs((x-mean)/sd)<2){0}else{1}
}

outlier = cmpfun(outlier)


######################################################################

##########
# testIt #
##########

#For use with p-hack-able data. Determines d,p,t,N,n1,n2 based on
#a given lvl (i.e., main effect [0], first lvl of mod [1] or second [2]), a 
#dataset for a given DV (i.e., 1 or 2), and whether outliers are to be 
#included. 

testIt=function(DV,lvl,out){ 
  
  #a set of conditionals that determine the data to be analyzed.
  # no subsetting by the moderator, no exclusion of outliers
  if(lvl==0 & out==0){  
    Y = DV[,1]
    X = DV[,2]  
  }
  # subsetting by lvl 1 of the moderator, no exclusion of outliers
  if(lvl==1 & out==0){  
    Y = subset(DV[,1], DV[,3]==1)
    X = subset(DV[,2], DV[,3]==1) 
  }
  # subsetting by lvl 2 of the moderator, no exclusion of outliers
  if(lvl==2 & out==0){
    Y = subset(DV[,1], DV[,3]==2)
    X = subset(DV[,2], DV[,3]==2) 
  }
  # no subsetting by the moderator, exclusion of outliers
  if(lvl==0 & out==1){
    Y = DV[,1]
    X = DV[,2]  
  }
  # subsetting by lvl 1 of the moderator, exclusion of outliers
  if(lvl==1 & out==1){  
    Y = subset(DV[,1], DV[,3]==1 & DV[,4] < 1)
    X = subset(DV[,2], DV[,3]==1 & DV[,4] < 1) 
  }
  # subsetting by lvl 2 of the moderator, exclusion of outliers
  if(lvl==2 & out==1){
    Y = subset(DV[,1], DV[,3]==2 & DV[,4] < 1)
    X = subset(DV[,2], DV[,3]==2 & DV[,4] < 1) 
  }
  
  #the output based on the above conditions
  test = t.test(Y~X,var.equal=T)
  n1 = length(subset(Y,X==1))
  n2 = length(subset(Y,X==2))
  v1 = var(subset(Y,X==1))
  v2 = var(subset(Y,X==2))
  N  = n1+n2
  t  = as.numeric(test[1])             
  p  = test$p.value
  m1 = as.numeric(test$estimate[1])
  m2 = as.numeric(test$estimate[2])
  d  = (m1-m2)/(sqrt(((n1-1)*v1+(n2-1)*v2)/(n1+n2-2)))
  out= c(d,p,t,N,n1,n2) 
  
}

testIt = cmpfun(testIt)

######################################################################

###################################
#        analyB and analyU        #
###################################

#Produces a vector of results using QRPs, including
#optional moderator, outlier removal, and multiple DVs. 
#Takes groups (g1:g4). Gives a vector of 
#(d,p,t,N,v,se,power,n1,n2)

analyB <- function(g1,g2,g3,g4,multDV,out,mod){
  
  #Create combo groups  
  G1=rbind(g1,g3); G2=rbind(g2,g4)
  
  #create X codes
  X1.1=replicate(length(G1[,1]),1); X1.2=replicate(length(G1[,1]),1) 
  X2.1=replicate(length(G2[,1]),2); X2.2=replicate(length(G2[,1]),2)
  
  #Create M codes
  m1.1=replicate(length(g1[,1]),1); m1.2=replicate(length(g1[,2]),1)
  m2.1=replicate(length(g2[,1]),1); m2.2=replicate(length(g2[,2]),1)
  m3.1=replicate(length(g3[,1]),2); m3.2=replicate(length(g3[,2]),2)
  m4.1=replicate(length(g4[,1]),2); m4.2=replicate(length(g4[,2]),2)
  M1.1=c(m1.1,m3.1); M1.2=c(m1.2,m3.2)
  M2.1=c(m2.1,m4.1);M2.2=c(m2.2,m4.2)
  
  #Create outlier codes
  o1.1=mapply(outlier,G1[,1],mean(G1[,1]),sd(G1[,1]))
  o1.2=mapply(outlier,G1[,2],mean(G1[,2]),sd(G1[,2]))
  o2.1=mapply(outlier,G2[,1],mean(G2[,1]),sd(G2[,1]))
  o2.2=mapply(outlier,G2[,2],mean(G2[,2]),sd(G2[,2]))
  
  #combine codes with outcome values
  c1=cbind(G1[,1],X1.1,M1.1,o1.1); c2=cbind(G1[,2],X1.2,M1.2,o1.2)
  c3=cbind(G2[,1],X2.1,M2.1,o2.1); c4=cbind(G2[,2],X2.2,M2.2,o2.2)
  
  #make "datasets"
  DV1=rbind(c1,c3)
  DV2=rbind(c2,c4)
  
  #Save p values for interaction effects
  A1=aov(DV1[,1]~DV1[,2]*DV1[,3])
  A2=aov(DV2[,1]~DV2[,2]*DV2[,3])
  A1.o=aov(DV1[,1]~DV1[,2]*DV1[,3],subset=DV1[,4]<1)
  A2.o=aov(DV2[,1]~DV2[,2]*DV2[,3],subset=DV2[,4]<1)
  intA1P=summary(A1)[[1]][["Pr(>F)"]][3]
  intA2P=summary(A2)[[1]][["Pr(>F)"]][3]
  intA1P.o=summary(A1.o)[[1]][["Pr(>F)"]][3]
  intA2P.o=summary(A2.o)[[1]][["Pr(>F)"]][3]
  
  #test in each possible way
  #  first DV
  t100=testIt(DV1,0,0) 
  t101=testIt(DV1,0,1)
  t110=testIt(DV1,1,0) 
  t111=testIt(DV1,1,1) 
  t120=testIt(DV1,2,0) 
  t121=testIt(DV1,2,1)
  #  second DV
  t200=testIt(DV2,0,0) 
  t201=testIt(DV2,0,1)
  t210=testIt(DV2,1,0) 
  t211=testIt(DV2,1,1) 
  t220=testIt(DV2,2,0) 
  t221=testIt(DV2,2,1)

  #pull the best result given options
  #  start looking without moderator
  pub = 
  if(t100[2]<.05 & t100[1]>0){t100}else{                      #DV1 and no outlier removal
  if(out==1 & t101[2]<.05 & t101[1]>0){t101}else{             #DV1 with outlier removal
  if(multDV==1 & t200[2]<.05 & t200[1]>0){t200}else{          #DV2 and no outlier removal
  if(multDV==1 & out==1 & t201[2]<.05 & t201[1]>0){t201}else{ #DV2 with outlier removal
  
  #  start chopping on the moderator (lvl 1)
  if(mod == 1 & t110[2]<.05 & t110[1]>0 & intA1P  < .05){t110}else{
  if(out==1 & mod == 1 & t111[2]<.05 & t111[1]>0 & intA1P.o< .05){t111}else{
  if(multDV==1 & mod == 1 & t210[2]<.05 & t210[1]>0 & intA1P  < .05){t210}else{
  if(multDV==1 & out==1 & mod == 1 & t211[2]<.05 & t211[1]>0 & intA1P.o< .05){t211}else{
    
  #  lvl 2
  if(mod == 1 & t120[2]<.05 & t120[1]>0 & intA2P  < .05){t120}else{
  if(out==1 & mod == 1 & t121[2]<.05 & t121[1]>0 & intA2P.o< .05){t121}else{
  if(multDV==1 & mod == 1 & t220[2]<.05 & t220[1]>0 & intA2P  < .05){t220}else{
  if(multDV==1 & out==1 & mod == 1 & t221[2]<.05 & t221[1]>0 & intA2P.o< .05){t221}else{
    t100}}}}}}}}}}}}
  
  #get additional info
  pub_v=((pub[4]/(pub[5]*pub[6]))+(pub[1]^2)/(2*pub[4]))
  pub_se=sqrt(pub_v)
  pow=pwr.t2n.test(pub[1], n1=pub[5], n2=pub[6])
  pub_pwr=pow$power
  #"publish" a final result
  publish=c(pub[1],pub[2],pub[3],pub[4],pub_v,pub_se,pub_pwr,pub[5],pub[6])
}

analyB = cmpfun(analyB)


######################################################################

###################################
#        expFinB and expFinU      #
###################################

#Produces results from an experiment that has been p-hacked.
#This adds optional stopping to the QRPs intro'd by analyB.

expFinB = function(meanD,sigma,cbdv,maxN,  #arg for expDataB and expDataU
                   minN,meanN,sdN,         #arg for sampB and sampU
                   multDV,out,mod,         #arg for analyB
                   colLim,add,QRP){        #arg specific to expDataB 
  
  if (QRP==1){
    #get data for a study using QRPs
    G = expDataB(meanD,sigma,cbdv,maxN)
    s = sampB(minN,meanN,sdN)
    a = analyB(G[,,1][1:s[1],],G[,,2][1:s[2],],G[,,3][1:s[3],],G[,,4][1:s[4],],multDV,out,mod)  
    for (i in 0:colLim){
      a = if(i == colLim || a[1] > 0 & a[2] < .05){a}else{
        s[1]=s[1]+add
        s[2]=s[2]+add
        s[3]=s[3]+add
        s[4]=s[4]+add
        a=analyB(G[,,1][1:s[1],],G[,,2][1:s[2],],G[,,3][1:s[3],],G[,,4][1:s[4],],multDV,out,mod)
      }
      return(a)}    
  }else{
    
    #get data for a study using only publication selection
    #get the sample
    samp = sampU(minN,meanN,sdN)
    n1 = samp[1]; n2 = samp[2]
    N = n1+n2
    df = N - 2
    #set tcrit (default is selction based on two-tail p-value and positive effect size)
    tcrit = qt(.975, df)
    #based on meanD, get a t value
    t = rtrunc(1, "t", a = tcrit, b = Inf,
               df = df, ncp = sqrt(n1/2)*meanD) 
    #convert to effect size etc.
    d=2*t/sqrt(2*n1)
    d_v = ( (2*n1 / (n1^2)) + d^2 / 2*df ) * (2*n1 / df)
    d_se = sqrt(d_v)
    p=2*pt(t, df=n1 + n2 - 2, lower.tail = F)
    pow=pwr.t2n.test(d, n1 = n1, n2 = n2)
    pwr=pow$power 
    out = c(d,p,t,N,d_v,d_se,pwr,n1,n2) 
    return(out)
  }    
} 
  
#Produces results from an experiment without QRPs or pubbias. 
expFinU = function(meanD,             #arg for expDataU
                   minN,meanN,sdN){   #arg for sampU
  
  #get the sample
  samp = sampU(minN,meanN,sdN)
  n1 = samp[1]; n2 = samp[2]
  N = n1+n2
  #based on meanD, get a t value
  t = rtrunc(1, "t", a = -Inf, b = Inf,
             df = N - 2,
             ncp = sqrt(n1/2)*meanD) 
  #convert to effect size etc.
  d=2*t/sqrt(2*n1)
  d_v = ( (2*n1 / (n1^2)) + d^2 / 2*df ) * (2*n1 / df)
  d_se = sqrt(d_v)
  p=2*pt(t, df=n1 + n2 - 2, lower.tail = F)
  pow=pwr.t2n.test(d, n1 = n1, n2 = n2)
  pwr=pow$power 
  out = c(d,p,t,N,d_v,d_se,pwr,n1,n2) 

}

expFinB = cmpfun(expFinB)
expFinU = cmpfun(expFinU)



######################################################################

##############
#   dataMA   #
##############

#Produces a dataset for meta-analysis. Applies both QRP
#and selection at a proportion specified by propB if 
#sel and QRP are 1 not 0. 

#' @param k the number of studies in the MA
#' @param QRP 1 if QRP/p-hacks are available, 0 otherwise
#' @param sel 1 if publication bias selection exists, 0 otherwise
#' @param propB the proportion of the sample affected by bias
#' @param meanD the true effect (or the average of the true effects if heterogeneity exists)
#' @param sigma the SD around the true effect
#' @param cbdv the correlation between the multiple DVs
#' @param maxN the max possible group size that could be created *this needs to be set higher than what can actually be generated--it doesn't mean you get bigger samples
#' @param minN the min of the truncated normal for sample size
#' @param meanN the mean of the truncated normal for sample size
#' @param sdN the SD of the truncated normal for sample size
#' @param multDV 1 if multiple DVs as a hack, 0 otherwise
#' @param out 1 if optional outlier removal as a hack, 0 otherwise
#' @param mod 1 if optional moderator as a hack, 0 otherwise
#' @param colLim number of times to try collecting more data
#' @param add number to add to each group when collecting more data


dataMA = function(k,QRP,sel,propB,       #arg specific to dataMA (QRP is used for expFinB, too)
                  meanD,sigma,cbdv,maxN, #arg for expDataB 
                  minN,meanN,sdN,        #arg for sampB and sampU
                  multDV,out,mod,        #arg in analyB and expFinB
                  colLim,add){           #arg for expFinB 
  
  #get the number of studies produced with bias (and those without)
  kB = round(k*propB)
  kU = k-kB
  
  #return warnings if needed
  if(QRP==0){
    if(multDV==1){print('multDV is only used if QRP = 1')} 
    if(out==1){print('out is only used if QRP = 1')} 
    if(mod==1){print('mod is only used if QRP = 1')} 
    if(colLim>0){print('colLim is only used if QRP = 1')} 
    if(add>0){print('add is only used if QRP = 1')}
    if (sel == 0 & kB > 0){print("propB cannot be > 0 with no sel or QRP")}   
  }
  
  #Produce unbiased data, if any
  if (kU > 0){
    rU = matrix(NA,kU,9)
    for (i in 1: kU){
      rU[i,] = expFinU(meanD,minN,meanN,sdN)}    
  }
 
  #Produce biased data based on QRP AND Sel
  if (QRP == 1 & sel == 1 & kB > 0){
    rB = matrix(NA,kB,9)
    i = 1
    for (i in 1:kB){
      rB[i,] = expFinB(meanD,sigma,cbdv,maxN,minN,meanN,sdN,multDV,out,mod,colLim,add,QRP)    
      repeat {if (rB[i,1]>0 & rB[i,2]<.05) break else{
        rB[i,] = expFinB(meanD,sigma,cbdv,maxN,minN,meanN,sdN,multDV,out,mod,colLim,add,QRP)}
      }
    }    
  }
  
  #Produce biased data based on Sel alone.
  if (QRP == 0 & sel == 1 & kB > 0){
    rB = matrix(NA,kB,9)
    for (i in 1:kB){
      rB[i,] = expFinB(meanD,sigma,cbdv,maxN,minN,meanN,sdN,multDV,out,mod,colLim,add,QRP)}
  }
  
  #Produce biased data based on QRP alone. 
  if (QRP == 1 & sel == 0 & kB > 0){
    rB = matrix(NA,kB,9)
    for (i in 1:kB){
      rB[i,] = expFinB(meanD,sigma,cbdv,maxN,minN,meanN,sdN,multDV,out,mod,colLim,add,QRP)}  
  }
  
  if (kU > 0 & kB > 0){outMat = rbind(rU,rB)}
  if (kU > 0 & kB == 0){outMat = rU}
  if (kU == 0 & kB > 0){outMat = rB}
  
  #change outMat to a data.frame and name the columns
  outMat = data.frame(d = outMat[,1],       # effect size, d
                      p = outMat[,2],       # p value for the two group comparison
                      t = outMat[,3],       # t value for the two group comparison
                      N = outMat[,4],       # total N
                      v = outMat[,5],       # variance for the effect size
                      se = outMat[,6],      # standard error for the effect size
                      pow = outMat[,7],     # power given the true effect for the two group comparison
                      n1 = outMat[,8],      # experimental group sample size
                      n2 = outMat[,9])      # control group sample size

  return(outMat)
}

dataMA = cmpfun(dataMA)
 




