



#=========================
# A new function that defines and allows application of arbitrary 
# censor functions that define how selection works
# pubProb is the probability that a given result (p,d) is published
# function simply needs to transale a result to a probability

publicationProb = function(censorFunction, pObs, dObs){
  
  #standard--what we used in the intial sims
  if(censorFunction=="posSig"){
    pubProb = if(pObs<.05 & dObs > 0){1}else{0}
  }
  
  
  #goofy step function that likes anything with p < .05 and is OK with
  #results in the right direction with .05 <= p < .1
  if(censorFunction=="step_1"){
    pubProb = if(pObs<.05){1}else{
      if(pObs < .1 & pObs >= .05 & dObs > 0){.5}else{0}
    }
  }
  
  #continuous function that prefers anything with p <.05 100%
  #and anything else as a steeply decreasing hyperbolic function
  if(censorFunction=="hyp_1"){
    pubProb = 1/(1+30*(pObs-.05))
  }
  
  return(pubProb)
}

#=========================






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


#==============
#   expFinU   #
#==============

# generate the results from an unbiased experiment
# delta is the true effect
# tau indicated heterogeneity
# minN and meanN are fed to a negative binomial for 
# sample size

# results from an unbiased experiment 
expFinU = function(delta, tau, empN, meanN, minN, empN.boost=0){
  
  #get the per-group sample size 
  if (empN==TRUE){
    n <- sample(perGrp$x,1) + empN.boost
  } else {
    n <- rtrunc(n=1, spec="nbinom", a=minN, b=Inf, size=2.3, mu=meanN)
  }
  
  #generate two independent vectors of raw data 
  #the mean is zero and error is randomly distributed
  #and equal between groups
  Xe = rnorm(n,0,1)
  Xc = rnorm(n,0,1)       
  
  #calculate the treatement effect as a function of the 
  #true effect, delta, and tau
  Te = delta + tau * rnorm(1,0,1)
  
  #store the true effect for the study
  D = Te
  
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
  out = c(d,p,t,N,d_v,d_se,pwr,n1,n2,D)
  
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
  
  #store the true effect for the study
  D = matrix(Te,2,maxN)
  
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
  G = array(c(g1,g2,g3,g4,D),dim=c(maxN,2,5))
  
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
    Y = subset(DV[,1], DV[,4] < 1)
    X = subset(DV[,2], DV[,4] < 1)   
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

analyB <- function(g1,g2,g3,g4,D,multDV,out,mod,censorFunction){
  
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
  t100=testIt(DV1,0,0) #(dv,lvl,outliers)
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
  
  #determine which effect are publishable relative to the 
  #censorFunction in use
  testOutput = t(data.frame(t100,t101,t110,
                            t111,t120,t121,
                            t200,t201,t210,
                            t211,t220,t221))
  pubOutput = rep(NA,12)
  for(iTest in 1:12){
    pPr = publicationProb(censorFunction, 
                          testOutput[iTest,2], 
                          testOutput[iTest,1])
    pubOutput[iTest] = rbinom(1,size=1,prob = pPr)
  }
  pubOutput = pubOutput==1
  
  #determine which interaction effects are publishable--a 
  #requirement for the simple effect to be published. 
  #NOTE: Doing this by taking the p-value for the interaction 
  #and the direction for the simple effect. This really
  #might be wrong. 
  #intOutput = t(data.frame(c(d,intA1P),    #AOV with DV1, no outliers
  #                         c(d,intA2P),    #AOV with DV2, no outliers
  #                         c(d,intA1P.o),  #AOV with DV1, outliers
  #                         c(d,intA2P.o))) #AOV with DV2, outliers
  
  
  
  
  #pull the best result given options
  #  start looking without moderator
  if(pubOutput[1]){
    best = t100                         #DV1 and no outlier removal (default)
  } else if (out==1 & pubOutput[2]){                   
    best = t101                         #DV1 with outlier removal
  } else if (multDV==1 & pubOutput[3]){
    best = t200                         #DV2 and no outlier removal
  } else if (multDV==1 & out==1 & pubOutput[4]){
    best = t201                         #DV2 with outlier removal
    
    #  start chopping on the moderator (lvl 1)
  } else if (mod == 1 & pubOutput[5]){
    best = t110
  } else if (out==1 & mod == 1 & pubOutput[6]){
    best = t111
  } else if (multDV==1 & mod == 1 & pubOutput[7]){
    best = t210
  } else if (multDV==1 & out==1 & mod == 1 & pubOutput[8]){
    best = t211
    
    #  lvl 2
  } else if(mod == 1 & pubOutput[9]){
    best = t120
  } else if (out==1 & mod == 1 & pubOutput[10]){
    best = t121
  } else if (multDV==1 & mod == 1 &pubOutput[11]){
    best = t220
  } else if (multDV==1 & out==1 & mod == 1 & pubOutput[12]){
    best = t221
  } else {
    best = t100
  }
  
  
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
  out=c(d,p,t,N,d_v,d_se,pwr,n1,n2,D)
  
  return(out)
}


#==================
#     expFinB     #     
#==================

# Produces results, a, from a p-hacked experiment.
expFinB = function(delta, tau, empN, maxN, meanN, minN, strat, empN.boost=empN.boost, censorFunction){
  
  #correlation between multiple DVs is set to 0.50 as default
  cbdv = 0.5
  
  # if QRP strategy is NONE
  if (strat=='none'){
    a = expFinU(delta, tau, empN, meanN, minN, empN.boost=empN.boost)
  } else if (strat=='mod'){   #if QRP strategy is MODERATE
    
    #get data for a study using QRPs
    G = expDataB(delta,tau,cbdv,maxN)
    
    #determine the starting per-group sample size
    #using either a specified distribution OR the empirical distribition
    if (empN == TRUE){
      s <- sample(perGrp$x,1) + empN.boost
    }else{
      s <- rtrunc(n=1, spec="nbinom", a=minN, b=Inf, size=2.3, mu=meanN)
    }
    
    s = round(s/2)
    
    #run the first analysis with some QRPs applied
    a = analyB(g1 = G[,,1][1:s,], #group one, 1:the current sample size
               g2 = G[,,2][1:s,],
               g3 = G[,,3][1:s,],
               g4 = G[,,4][1:s,],
               D = G[,,5][1,1],           # the study-lvl true effect
               multDV=1,out=0,mod=0,censorFunction) # MODERATE 
    
    #define optional stopping parameters for MODERATE strategy
    colLim = 3 
    add = 3
    
    #see if you can benefit from optional stopping
    for (i in 1:colLim){
      #continue adding more data and p-hacking until either collection
      #limit is reached (colLim) or the p-value and the sign of d are 
      #significant and positive
      pPr = publicationProb(censorFunction, a[2], a[1])
      pubOutput = rbinom(1,size=1,prob = pPr)==1
      if(pubOutput){break}
      #if p-value and sign of d aren't sig/pos, define the new sample sizes
      s=s+add
      a = analyB(g1 = G[,,1][1:s,], #group one, 1:the current sample size
                 g2 = G[,,2][1:s,],
                 g3 = G[,,3][1:s,],
                 g4 = G[,,4][1:s,],
                 D = G[,,5][1,1],      # the study-lvl true effect
                 multDV=1,out=0,mod=0,censorFunction) # MODERATE
    }
    
  } else if (strat=='agg'){ #if QRP strategy is AGGRESSIVE
    
    #get data for a study using QRPs
    G = expDataB(delta,tau,cbdv,maxN)
    
    #determine the starting per-group sample size
    #using either a specified distribution OR the empirical distribition
    if (empN == TRUE){
      s <- sample(perGrp$x,1) + empN.boost
    }else{
      s <- rtrunc(n=1, spec="nbinom", a=minN, b=Inf, size=2.3, mu=meanN)
    }
    
    s = round(s/2)
    
    #run the first analysis with some QRPs applied
    a = analyB(g1 = G[,,1][1:s,], #group one, 1:the current sample size
               g2 = G[,,2][1:s,],
               g3 = G[,,3][1:s,],
               g4 = G[,,4][1:s,],
               D = G[,,5][1,1],      # the study-lvl true effect
               multDV=1,out=1,mod=1,censorFunction) # AGGRESIVE 
    
    #define optional stopping parameters for AGGRESSIVE strategy
    colLim = 5 
    add = 3
    
    #see if you can benefit from optional stopping
    for (i in 1:colLim){
      #continue adding more data and p-hacking until either collection
      #limit is reached (colLim) or the p-value and the sign of d are 
      #significant and positive
      pPr = publicationProb(censorFunction, a[2], a[1])
      pubOutput = rbinom(1,size=1,prob = pPr)==1
      if(pubOutput){break}
      #if p-value and sign of d aren't sig/pos, define the new sample
      #sizes
      s=s+add
      a = analyB(g1 = G[,,1][1:s,], #group one, 1:the current sample size
                 g2 = G[,,2][1:s,],
                 g3 = G[,,3][1:s,],
                 g4 = G[,,4][1:s,],
                 D = G[,,5][1,1],      # the study-lvl true effect
                 multDV=1,out=1,mod=1,censorFunction) # AGGRESSIVE
    }
    
  } else {print('ERROR: define QRP strategy')}
  
  #return the result
  return(a)    
  
}    



#=============
#   dataMA   #
#=============

# Produces a dataset for meta-analysis. Applies both QRP
# and selection at a proportion specified by propB if 
# sel and QRP are 1 not 0. 

#' @param k the number of studies in the MA
#' @param delta the true effect (or the average of the true effects if heterogeneity exists)
#' @param tau the SD around the true effect
#' @param empN a logical, whether to use the empirical per-group N distribution
#' @param maxN the max possible group size that could be created *this needs to be set higher than what can actually be generated--it doesn't mean you get bigger samples
#' @param minN the min of the truncated normal for sample size
#' @param meanN the average of the truncated normal for sample size
#' @param selProp the proportion of the sample affected by bias
#' @param qrpEnv the qrp environment that produced the literature: 'none', 'low', 'med', 'high'
#' @param empN.boost A constant that is added to the empirical effect sizes

dataMA <- function(k, delta, tau,
                   empN, maxN, meanN, minN,
                   selProp, qrpEnv, empN.boost = 0, censorFunction) {  
  
  #get the number of studies exposed to publication selection bias (and those not exposed)
  kB = round(k*selProp)
  kU = k-kB
  
  #get the proportions of studies produced under each strategy
  if(qrpEnv=='none'){
    noneP = 1; modP = 0; aggP = 0
  }else if(qrpEnv=='low'){
    noneP = 0.50; modP = 0.40; aggP = 0.10
  }else if(qrpEnv=='med'){
    noneP = 0.30; modP = 0.50; aggP = 0.20
  }else if(qrpEnv=='high'){
    noneP = 0.10; modP = 0.40; aggP = 0.50
  }else{
    print('ERROR: qrpEnv must be none, low, med, or high')}
  
  #get number of to-be observed studies for all cases
  kU_None = round(kU*noneP) 
  kU_Mod = round(kU*modP)
  kU_Agg = kU - kU_None - kU_Mod
  #
  kB_None = round(kB*noneP)  
  kB_Mod = round(kB*modP) 
  kB_Agg = kB - kB_None - kB_Mod 
  
  #initialize results matricies for all of the above (makes it simpler, 
  #the NAs will be trimmed later)
  rU_None = matrix(NA,k,13)
  rU_Mod = matrix(NA,k,13)
  rU_Agg = matrix(NA,k,13)
  rB_None = matrix(NA,k,13)
  rB_Mod = matrix(NA,k,13)
  rB_Agg = matrix(NA,k,13)
  
  #Produce data *unaffected* by publication selection bias or from QRP (strat = none)
  if (kU_None > 0){
    for (i in 1: kU_None){
      rU_None[i,1:10] = expFinB(delta, tau, empN, maxN, meanN, minN, strat='none', empN.boost=empN.boost,censorFunction) 
      rU_None[i,11] = 0 #number file drawered
      rU_None[i,12] = 0 #no sel
      rU_None[i,13] = 0 #no QRP
    }    
  }
  
  #Produce data *unaffected* by publication selection bias but affected by QRP strat = mod
  if (kU_Mod > 0){
    for (i in 1: kU_Mod){
      rU_Mod[i,1:10] = expFinB(delta, tau, empN, maxN, meanN, minN, strat='mod', empN.boost=empN.boost,censorFunction) 
      rU_Mod[i,11] = 0 #number file drawered
      rU_Mod[i,12] = 0 #no sel
      rU_Mod[i,13] = 1 #mod qrp
    }    
  }
  
  #Produce data *unaffected* by publication selection bias and from QRP strat = agg
  if (kU_Agg > 0){
    for (i in 1: kU_Agg){
      rU_Agg[i,1:10] = expFinB(delta, tau, empN, maxN, meanN, minN, strat='agg', empN.boost=empN.boost,censorFunction) 
      rU_Agg[i,11] = 0 #number file drawered
      rU_Agg[i,12] = 0 #no sel
      rU_Agg[i,13] = 2 #agg qrp
    }    
  }
  
  #Produce data *affected* by publication selection bias and by QRP strat = none
  if (kB_None > 0){
    for (i in 1:kB_None){
      rB_None[i,1:10] = expFinB(delta, tau, empN, maxN, meanN, minN, strat='none', empN.boost=empN.boost,censorFunction) 
      rB_None[i,11] = 0 #number of file drawered studes
      rB_None[i,12] = 1 #selection
      rB_None[i,13] = 0 #no QRP
      
      pPr = publicationProb(censorFunction, rB_None[i,2], rB_None[i,1]) #check result against censor function 
      repeat {if (rbinom(1,size=1,prob = pPr)==1) break else{
        rB_None[i,1:10] = expFinB(delta, tau, empN, maxN, meanN, minN, strat='none', empN.boost=empN.boost,censorFunction) 
        rB_None[i,11] = rB_None[i,11] + 1} #count file-drawered studies
        rB_None[i,12] = 1 #sel
        rB_None[i,13] = 0 #no QRP
        pPr = publicationProb(censorFunction, rB_None[i,2], rB_None[i,1]) #check result against censor function 
      }
    }    
  }
  
  #Produce data *affected* by publication selection bias and from QRP strat = mod
  if (kB_Mod > 0){
    for (i in 1:kB_Mod){
      rB_Mod[i,1:10] = expFinB(delta, tau, empN, maxN, meanN, minN, strat='mod', empN.boost=empN.boost,censorFunction) 
      rB_Mod[i,11] = 0 #number of file drawered studes
      rB_Mod[i,12] = 1 #sel
      rB_Mod[i,13] = 1 #mod QRP
      
      pPr = publicationProb(censorFunction, rB_Mod[i,2], rB_Mod[i,1]) #check result against censor function 
      repeat {if (rbinom(1,size=1,prob = pPr)==1) break else{
        rB_Mod[i,1:10] = expFinB(delta, tau, empN, maxN, meanN, minN, strat='mod', empN.boost=empN.boost,censorFunction) 
        rB_Mod[i,11] = rB_Mod[i,11] + 1} #count file-drawered studies
        rB_Mod[i,12] = 1 #sel
        rB_Mod[i,13] = 1 #mod QRP
        pPr = publicationProb(censorFunction, rB_Mod[i,2], rB_Mod[i,1]) #check result against censor function 
      }
    }    
  }
  
  #Produce data *affected* by publication selection bias and from QRP strat = agg
  if (kB_Agg > 0){
    for (i in 1:kB_Agg){
      rB_Agg[i,1:10] = expFinB(delta, tau, empN, maxN, meanN, minN, strat='agg', empN.boost=empN.boost,censorFunction) 
      rB_Agg[i,11] = 0 #number of file drawered studes
      rB_Agg[i,12] = 1 #sel
      rB_Agg[i,13] = 2 #Agg QRP
      
      pPr = publicationProb(censorFunction, rB_Agg[i,2], rB_Agg[i,1]) #check result against censor function 
      repeat {if (rbinom(1,size=1,prob = pPr)==1) break else{
        rB_Agg[i,1:10] = expFinB(delta, tau, empN, maxN, meanN, minN, strat='agg', empN.boost=empN.boost,censorFunction) 
        rB_Agg[i,11] = rB_Agg[i,11] + 1} #count file-drawered studies
        rB_Agg[i,12] = 1 #sel
        rB_Agg[i,13] = 2 #Agg QRP
        pPr = publicationProb(censorFunction, rB_Agg[i,2], rB_Agg[i,1]) #check result against censor function 
      }
    }    
  }
  
  #bind together the output and trim NAs
  outMat = rbind(rU_None,rU_Mod,rU_Agg,rB_None,rB_Mod,rB_Agg)
  outMat = subset(outMat,!is.na(outMat[,1]))
  
  #name columnes
  colnames(outMat) = c('d',       # effect size, d
                       'p',       # p value for the two group comparison
                       't',       # t value for the two group comparison
                       'N',       # total N
                       'v',       # variance for the effect size
                       'se',      # standard error for the effect size
                       'pow',     # power given the true effect for the two group comparison
                       'n1',      # experimental group sample size
                       'n2',      # control group sample size
                       'D',       # the study-level true effect
                       'kFD',     # the number of studies file drawered to generate the observed results 
                       'sel',     # 0 = no selection, 1 = selection
                       'qrp')     # 0 = 'none', 1 = 'mod', 2 = 'agg'
  
  return(outMat)
}
