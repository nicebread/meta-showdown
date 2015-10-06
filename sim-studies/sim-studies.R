#VERSION NOTES
#Totally re-did the data generation functions
#Now it's all raw data approach
#also now recording number of studies file-drawered to get the resulting observation

######################################################################

#==============
#   Outlier   #
#==============

# evaluate whether a number, x, 
# is an outlier as defined by 
# being beyond 2 SDs from the mean
# of x's vector

outlier=function(x,mean,sd){
  out=if(abs((x-mean)/sd)<2){0}else{1}
}


#============
# expDataB  #
#============

# produce data for other functions to bias
# delta is the true effect
# tau is for heterogeneity
# cbdv is the correlation between the two outcomes
# output is 4 vectors of length maxN
# This is called within expFinB

expDataB = function(delta,tau,
                    cbdv,maxN){                    
  
  #calculate the treatement effect as a function of the 
  #true effect, delta, and heterogeneity (defined as tau)
  Te = delta + tau*rnorm(1,0,1)
  
  #generate four matricies of maxN rows and 2 columns
  #each matrix represents results from maxN subjects experiencing
  #one of the 4 unique combinations of the experimental manipulation
  #and the moderator (it's a 2*2 design).
  #each column represents the results on a DV because each 
  #participant has responded on two DVs
  #results on the DVs are correlated at r = cbdv
  #the performance of the groups (i.e., the matricies) are
  #not correlated
  #responses are normally distributed with a mean of zero 
  #and a SD of 1
  #the treatment effect is added to each observation in the
  #experimental group
  #there is no effect for the moderator
  g1 = mvrnorm(maxN,rep(0,2),matrix(c(1,cbdv,cbdv,1),2,2)) + Te
  g2 = mvrnorm(maxN,rep(0,2),matrix(c(1,cbdv,cbdv,1),2,2))
  g3 = mvrnorm(maxN,rep(0,2),matrix(c(1,cbdv,cbdv,1),2,2)) + Te
  g4 = mvrnorm(maxN,rep(0,2),matrix(c(1,cbdv,cbdv,1),2,2))
  
  #build the output array
  G = array(c(g1,g2,g3,g4),dim=c(maxN,2,4))
  
  return(G)
  
}


#==========
# testIt  #
#==========

# For use with p-hack-able data from expDataB. 
# Determines d,p,t,N,n1,n2 based on a given lvl 
# (i.e., main effect [0], first lvl of mod [1] 
# or second [2]), a dataset for a given DV 
# (i.e., 1 or 2), and whether outliers are to be 
# included. This is called within analyB

testIt=function(DV,lvl,out){ 
  
  # a set of conditionals that determine the data to be analyzed.
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
  df = N - 2
  
  #get pooled variance
  S = sqrt( ((n1 - 1)*v1 + (n2 - 1)*v2) / df )
  
  #calculate d and the variance of d
  d  = (m1-m2)/S
  
  #this only returns the info needed to tell
  #whether the data will be hacked again
  #things like power and variance will get calculated
  #if the result is kept. 
  out= c(d,p,t,N,n1,n2)
  
  return(out)
  
}



#===============
#    analyB    # 
#===============

# Produces a vector of results using QRPs, including
# optional moderator, outlier removal, and multiple DVs. 
# Takes groups (g1:g4) from expDataB. Gives a vector of 
# (d,p,t,N,v,se,power,n1,n2).

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
  best = 
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
  
  #get additional info for the best results
  d = best[1]
  p = best[2]
  t = best[3]
  N = best[4]
  n1 = best[5]
  n2 = best[6]
  df = N - 2
  d_v= (n1 + n2)/(n1 * n2) + (d^2 / (2 *df)) * (n1 + n2) / df
  d_se = sqrt(d_v)
  pow = pwr.t2n.test(d, n1=n1, n2=n2)
  pwr = pow$power
  
  #return the best result
  out=c(d,p,t,N,d_v,d_se,pwr,n1,n2)
  
  return(out)
}


#==================
#     expFinB     #     
#==================

# Produces results from a p-hacked experiment.
# It is the biased parallel to expFinU.

expFinB = function(delta,tau,cbdv,maxN,    #arg for expDataB
                   multDV,out,mod,         #arg for analyB
                   colLim,add,empN,minN,meanN){            #new args for expFinB
  
  #get data for a study using QRPs
  G = expDataB(delta,tau,cbdv,maxN)
  #determine the starting per-group sample size
  #using either a specified distribution OR the empirical distribition
  if (empN = T){
    s = sample(perGrp$x,1)
  }else{
    s = rtrunc(n=1, spec="nbinom", a=minN, b=Inf, size=2.3, mu=meanN)
  }
  
  #if the data are generated with optional moderators
  #what is typically the per-group sample size is divided in 2
  #since one half experiences each level of the mod
  if (mod == 1){
    s = ceiling(s/2)  
  }
  #run the first analysis with some QRPs applied
  a = analyB(g1 = G[,,1][1:s,], #group one, 1:the current sample size
             g2 = G[,,2][1:s,],
             g3 = G[,,3][1:s,],
             g4 = G[,,4][1:s,],
             multDV,out,mod)
  #see if you can benefit from optional stopping
  for (i in 0:colLim){
    #continue adding more data and p-hacking until either collection
    #limit is reached (colLim) or the p-value and the sign of d are 
    #significant and positive
    a = if(i == colLim || a[1] > 0 & a[2] < .05){a}else{
      #if p-value and sign of d aren't sig/pos, define the new sample
      #sizes
      s=s+add
      a = analyB(g1 = G[,,1][1:s,], #group one, 1:the current sample size
                 g2 = G[,,2][1:s,],
                 g3 = G[,,3][1:s,],
                 g4 = G[,,4][1:s,],
                 multDV,out,mod)}
  }
  
  #return the p-hacked result
  return(a)    
  
}    


#==============
#   expFinU   #
#==============

# generate the results from an unbiased experiment
# delta is the true effect
# tau indicated heterogeneity
# minN and meanN are fed to a negative binomial for 
# sample size

# results from an unbiased experiment
expFinU = function(delta,tau,
                   empN,minN,meanN){
  
  #get the per-group sample size 
  if (empN = T){
    n = sample(perGrp$x,1)
  }else{
    n = rtrunc(n=1, spec="nbinom", a=minN, b=Inf, size=2.3, mu=meanN)
  }
  
  #generate two independent vectors of raw data 
  #the mean is zero and error is randomly distributed
  #and equal between groups
  Xe = rnorm(n,0,1)
  Xc = rnorm(n,0,1)       
  
  #calculate the treatement effect as a function of the 
  #true effect, delta, and tau
  Te = delta + tau * rnorm(1,0,1)
  
  #add the treatment effect to the experimental group
  Ye = Te + Xe 
  Yc =      Xc 
  
  #get the summary stats
  m1 = mean(Ye)
  v1 = var(Ye)
  m2 = mean(Yc)
  v2 = var(Yc)
  n1 = n
  n2 = n
  df = 2*n-2
  
  #get the pooled variance
  S = sqrt( ((n1 - 1)*v1 + (n2 - 1)*v2) / df )
  
  #compare the two distributions
  test = t.test(Ye,Yc)
  
  #calculate d, the variance of d, the p-value, the t-stat, and n.
  d = (m1 - m2)/S
  d_v = (n1 + n2)/(n1 * n2) + (d^2 / (2 *df)) * (n1 + n2) / df
  d_se = sqrt(d_v)
  p = test$p.value
  t = as.numeric(test$statistic)
  N = n1+n2
  
  #get power
  pow = pwr.t2n.test(d, n1 = n1, n2 = n2)
  pwr = pow$power 
  
  #output 
  out = c(d,p,t,N,d_v,d_se,pwr,n1,n2)
  
}

#=============
#   dataMA   #
#=============

# Produces a dataset for meta-analysis. Applies both QRP
# and selection at a proportion specified by propB if 
# sel and QRP are 1 not 0. 

#' @param k the number of studies in the MA
#' @param QRP 1 if QRP/p-hacks are available, 0 otherwise
#' @param sel 1 if publication bias selection exists, 0 otherwise
#' @param propB the proportion of the sample affected by bias
#' @param delta the true effect (or the average of the true effects if heterogeneity exists)
#' @param tau the SD around the true effect
#' @param cbdv the correlation between the multiple DVs
#' @param empN a logical, whether to use the empirical per-group N distribution
#' @param maxN the max possible group size that could be created *this needs to be set higher than what can actually be generated--it doesn't mean you get bigger samples
#' @param minN the min of the truncated normal for sample size
#' @param meanN the mean of the truncated normal for sample size
#' @param multDV 1 if multiple DVs as a hack, 0 otherwise
#' @param out 1 if optional outlier removal as a hack, 0 otherwise
#' @param mod 1 if optional moderator as a hack, 0 otherwise
#' @param colLim number of times to try collecting more data
#' @param add number to add to each group when collecting more data
#' @param verbose Should informations be printed?

dataMA <- function(k, delta=0.5, tau=0, maxN=500,
                   empN = F, minN=10, meanN=30,                    
                   # Parameters for publication bias
                   sel=0, propB=0,                   
                   # parameters for QRP
                   QRP=0, cbdv=0.5, multDV=0, out=0, mod=0, colLim=0, add=0, verbose=TRUE) {
  
  #get the number of studies produced with bias (and those without)
  kB = round(k*propB)
  kU = k-kB
  
  #return warnings if needed
  if(QRP==0 & verbose==TRUE){
    if(multDV==1){print('multDV is only used if QRP = 1')} 
    if(out==1){print('out is only used if QRP = 1')} 
    if(mod==1){print('mod is only used if QRP = 1')} 
    if(colLim>0){print('colLim is only used if QRP = 1')} 
    if(add>0){print('add is only used if QRP = 1')}
    if(sel == 0 & kB > 0){print("propB cannot be > 0 with no sel or QRP")}   
  }
  
  #Produce unbiased data, if any
  if (kU > 0){
    rU = matrix(NA,kU,10) 
    for (i in 1: kU){
      rU[i,1:9] = expFinU(delta,tau,empN,minN,meanN)
      rU[i,10] = 0
    }    
  }
  
  
  #Produce biased data based on QRP AND Sel
  if (QRP == 1 & sel == 1 & kB > 0){
    rB = matrix(NA,kB,10)
    i = 1
    for (i in 1:kB){
      rB[i,1:9] = expFinB(delta,tau,cbdv,maxN,multDV,out,mod,colLim,add,empN,minN,meanN) 
      rB[i,10] = 0 #number of file drawered studes
      repeat {if (rB[i,1]>0 & rB[i,2]<.05) break else{
        rB[i,1:9] = expFinB(delta,tau,cbdv,maxN,multDV,out,mod,colLim,add,empN,minN,meanN)
        rB[i,10] = rB[i,10] + 1} #count file-drawered studies
      }
    }    
  }
  
  #Produce biased data based on Sel alone.
  if (QRP == 0 & sel == 1 & kB > 0){
    rB = matrix(NA,kB,10)                    #made it 10 columns to record file drawered
    for (i in 1:kB){
      #note that it is produced using expFinU and
      #then acted on by selection bias
      rB[i,1:9] = expFinU(delta,tau,empN,minN,meanN)
      rB[i,10] = 0 #number of file drawered studies
      repeat {if (rB[i,1]>0 & rB[i,2]<.05) break else{
        rB[i,1:9] = expFinU(delta,tau,empN,minN,meanN)
        rB[i,10] = rB[i,10] + 1} #count the file-drawered studies
      }
    }    
  }
  
  #Produce biased data based on QRP alone. 
  if (QRP == 1 & sel == 0 & kB > 0){
    rB = matrix(NA,kB,10)
    for (i in 1:kB){
      rB[i,1:9] = expFinB(delta,tau,cbdv,maxN,multDV,out,mod,colLim,add,empN,minN,meanN)
      rB[i,10] = 0 #studies are never file-drawered, always takes a zero
    }  
  }
  
  #bind together the output
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
                      n2 = outMat[,9],      # control group sample size
                      kFD = outMat[,10])    # the number of studies file drawered to generate the observed results 
  
  return(outMat)
}

