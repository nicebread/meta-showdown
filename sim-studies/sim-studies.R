# MAIN FUNCTION to use: simMA
# (see from line 440)

# source("sim-studies.R")

# load censoring function
source("censorFunc.R")

# get a simulated per-group sample size that follows the distribution of empirical sample sizes 
# (see folder "Empirical sample size distributions")
# min = minimum sample size, max = maximum sample size
# max = 1905 corresponds to the largest observed per-group sample size in Marszalek et al.
getN <- function(k=1, min.n = 5, max.n = 1905, shape=1.15326986, scale=0.04622745) {
	library(invgamma)
	ns <- round(rtrunc(n=k, spec="invgamma", a=min.n, b=max.n, shape=shape, scale=scale))
}

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
#   simData.noQRP   #
#==============

# generate the results from an unbiased experiment
# delta is the true effect
# tau indicated heterogeneity
# minN and meanN are fed to a negative binomial for 
# sample size

# results from an unbiased experiment 
simData.noQRP <- function(delta, tau){
  
  #get the per-group sample size 
  n <- getN(k=1)
  
  #calculate the treatement effect as a function of the 
  #true effect, delta, and tau
  delta_i = delta + rnorm(1, 0, tau)
  
  #generate two independent vectors of raw data 
  #the mean is zero and error is randomly distributed
  #and equal between groups
  Ye = rnorm(n, delta_i, 1)
  Yc = rnorm(n, 0, 1)   
  
  #get the summary stats
  m1 = mean(Ye)
  v1 = var(Ye)
  m2 = mean(Yc)
  v2 = var(Yc)
  n1 = n
  n2 = n
  df = 2*n-2
  
  #get the pooled variance (formula from Hedge's g as)
  S = sqrt( ((n1 - 1)*v1 + (n2 - 1)*v2) / df )
  
  #compare the two distributions
  test = t.test(Ye, Yc)
  
  #calculate d, the variance of d, the p-value, the t-stat, and n.
  d = (m1 - m2)/S
	
  d_v = (n1 + n2)/(n1 * n2) + (d^2 / (2*(n1+n2)))
  d_se = sqrt(d_v)
	
  p = test$p.value
  t = as.numeric(test$statistic)
  
  #get power
  pow = pwr.t2n.test(d, n1 = n1, n2 = n2)
  pwr = pow$power 
  
  #output 
  out = c(d, p, t, n1+n2, d_v, d_se, pwr, n1, n2, delta_i)  
}



#============
# expDataB  #
#============

# produce data for other functions to bias
# delta is the true effect
# tau is for heterogeneity
# cbdv is the correlation between the two outcomes
# output is 4 vectors of length maxN
# This is called within simData.QRP

expDataB <- function(delta, tau, cbdv, maxN = 3000){
  
  # sample the treatment effect as a function of the 
  # true effect, delta, and heterogeneity (defined as tau)
  delta_i = delta + rnorm(1, 0, tau)
  
  # store the true effect for the study
  D = matrix(delta_i, 2, maxN)
  
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
  g1 = mvrnorm(maxN, rep(0,2), matrix(c(1,cbdv,cbdv,1),2,2)) + delta_i
  g2 = mvrnorm(maxN, rep(0,2), matrix(c(1,cbdv,cbdv,1),2,2))
  g3 = mvrnorm(maxN, rep(0,2), matrix(c(1,cbdv,cbdv,1),2,2)) + delta_i
  g4 = mvrnorm(maxN, rep(0,2), matrix(c(1,cbdv,cbdv,1),2,2))
  
  #build the output array
  G = array(c(g1,g2,g3,g4,D),dim=c(maxN, 2, 5))
  
  return(G)
  
}

#w <- expDataB(0.5, .1, .2)


#==========
# testIt  #
#==========

# For use with p-hack-able data from expDataB. 
# Determines d,p,t,N,n1,n2 based on a given lvl 
# (i.e., main effect [0], first lvl of mod [1] 
# or second [2]), a dataset for a given DV 
# (i.e., 1 or 2), and whether outliers are to be 
# included. This is called within analyB

testIt=function(DV, lvl, out){
  
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
  out <- c(d,p,t,N,n1,n2)
  
  return(out)
  
}



#===============
#    analyB    # 
#===============

# Produces a vector of results using QRPs, including
# optional moderator, outlier removal, and multiple DVs. 
# Takes groups (g1:g4) from expDataB. Gives a vector of 
# (d,p,t,N,v,se,power,n1,n2).

analyB <- function(g1, g2, g3, g4, D, multDV, out, mod){
  
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
  best = if(t100[2]<.05 & t100[1]>0){t100}                     #DV1 and no outlier removal
  else if(out==1 & t101[2]<.05 & t101[1]>0){t101}             #DV1 with outlier removal
  else if(multDV==1 & t200[2]<.05 & t200[1]>0){t200}         #DV2 and no outlier removal
  else if(multDV==1 & out==1 & t201[2]<.05 & t201[1]>0){t201} #DV2 with outlier removal
  
  #  start chopping on the moderator (lvl 1)
  else if(mod == 1 & t110[2]<.05 & t110[1]>0 & intA1P  < .05){t110}
  else if(out==1 & mod == 1 & t111[2]<.05 & t111[1]>0 & intA1P.o< .05){t111}
  else if(multDV==1 & mod == 1 & t210[2]<.05 & t210[1]>0 & intA1P  < .05){t210}
  else if(multDV==1 & out==1 & mod == 1 & t211[2]<.05 & t211[1]>0 & intA1P.o< .05){t211}
  
  #  lvl 2
  else if(mod == 1 & t120[2]<.05 & t120[1]>0 & intA2P  < .05){t120}
  else if(out==1 & mod == 1 & t121[2]<.05 & t121[1]>0 & intA2P.o< .05){t121}
  else if(multDV==1 & mod == 1 & t220[2]<.05 & t220[1]>0 & intA2P  < .05){t220}
  else if(multDV==1 & out==1 & mod == 1 & t221[2]<.05 & t221[1]>0 & intA2P.o< .05){t221}
  else{t100}
  
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
  out=c(d, p, t, N, d_v, d_se, pwr, n1, n2, D)
  
  return(out)
}


#==================
#     simData.QRP     #     
#==================

# Produces results, a, from a p-hacked experiment.
simData.QRP <- function(delta, tau, QRP.strategy, maxN = 3000){
  
  #correlation between multiple DVs is set to 0.20 as default
  cbdv = 0.2
  
  # if QRP strategy is NONE
  if (QRP.strategy=='none'){
    a = simData.noQRP(delta, tau)
  }
  
  #if QRP strategy is MODERATE
  else if (QRP.strategy=='mod'){
    
    #get data for a study using QRPs
    G <- expDataB(delta, tau, cbdv)
    
    #determine the starting per-group sample size
    s <- getN(k=1)
    
		# Divide sample size by 2: the idea is that the main factor of interest defined the two group sizes. A moderator factor is then added to create a 2*2, but because the moderator is not the main focus, the empirical sample sizes should only be used for the two groups formed by the main factor--not the four groups formed by the 2*2 split.
    s <- round(s/2)
    
    #run the first analysis with some QRPs applied
    a = analyB(g1 = G[,,1][1:s,], #group one, 1:the current sample size
               g2 = G[,,2][1:s,],
               g3 = G[,,3][1:s,],
               g4 = G[,,4][1:s,],
               D = G[,,5][1,1],      # the study-lvl true effect
               multDV=1, out=0, mod=0) # MODERATE 
    
    #define optional stopping parameters for MODERATE strategy
    colLim = 3 
    add = 3
    
    #see if you can benefit from optional stopping
    for (i in 1:colLim){
      #continue adding more data and p-hacking until either collection
      #limit is reached (colLim) or the p-value and the sign of d are 
      #significant and positive
      if(a[1] > 0 & a[2] < .05){break}
      #if p-value and sign of d aren't sig/pos, define the new sample
      #sizes
      s=s+add
      a = analyB(g1 = G[,,1][1:s,], #group one, 1:the current sample size
                 g2 = G[,,2][1:s,],
                 g3 = G[,,3][1:s,],
                 g4 = G[,,4][1:s,],
                 D = G[,,5][1,1],      # the study-lvl true effect
                 multDV=1,out=0,mod=0) # MODERATE
    }
  
  }
  
  #if QRP strategy is AGGRESSIVE
  else if (QRP.strategy=='agg'){
    
    #get data for a study using QRPs
    G = expDataB(delta,tau,cbdv,maxN)
    
    #determine the starting per-group sample size
    s <- getN(k=1)
    
		# Divide sample size by 2: the idea is that the main factor of interest defined the two group sizes. A moderator factor is then added to create a 2*2, but because the moderator is not the main focus, the empirical sample sizes should only be used for the two groups formed by the main factor--not the four groups formed by the 2*2 split.
    s <- round(s/2)
    
    #run the first analysis with some QRPs applied
    a = analyB(g1 = G[,,1][1:s,], #group one, 1:the current sample size
               g2 = G[,,2][1:s,],
               g3 = G[,,3][1:s,],
               g4 = G[,,4][1:s,],
               D = G[,,5][1,1],      # the study-lvl true effect
               multDV=1,out=1,mod=1) # AGGRESIVE 
    
    #define optional stopping parameters for AGGRESSIVE strategy
    colLim = 5 
    add = 3
    
    #see if you can benefit from optional stopping
    for (i in 1:colLim){
      #continue adding more data and p-hacking until either collection
      #limit is reached (colLim) or the p-value and the sign of d are 
      #significant and positive
      if(a[1] > 0 & a[2] < .05){break}
      #if p-value and sign of d aren't sig/pos, define the new sample
      #sizes
      s=s+add
      a = analyB(g1 = G[,,1][1:s,], #group one, 1:the current sample size
                 g2 = G[,,2][1:s,],
                 g3 = G[,,3][1:s,],
                 g4 = G[,,4][1:s,],
                 D = G[,,5][1,1],      # the study-lvl true effect
                 multDV=1,out=1,mod=1) # AGGRESSIVE
    }
    
  }
  
  else{print('ERROR: define QRP strategy')}
  
  #return the result
  return(a)    
  
}    




## ======================================================================
## New implementation with new censoring functions
## ======================================================================


# Produces a dataset for meta-analysis. Applies both QRP
# and selection at a proportion specified by propB if 
# sel and QRP are 1 not 0. 

#' @param k the number of studies in the MA
#' @param delta the true effect (or the average of the true effects if heterogeneity exists)
#' @param tau the SD around the true effect
#' @param censor The censoring function - either "none", "med" (medium publication bias), "high" (high publication bias), or a vector of 3 values for the censoring function (posSign_NS_baseRate, negSign_NS_baseRate, counterSig_rate)
#' @param qrpEnv the qrp environment that produced the literature: 'none', 'low', 'med', 'high'
#' @param empN.boost A constant that is added to the empirical effect sizes: WARNING: NOT CAREFULLY TESTED YET!!

# k=10;delta=.3;tau=.1;qrpEnv="med";censorFunc="A"; empN=TRUE; maxN = 1000; meanN = NA; minN = 0; empN.boost = 0
simMA <- function(k, delta, tau, qrpEnv= c("none", "low", "medium", "high"), censorFunc = c("none", "medium", "high"), verbose=FALSE) {  
    
	# validate parameters
	if (length(censorFunc) == 1) {
		censorFunc <- match.arg(censorFunc, c("none", "medium", "high"))
	}
	qrpEnv <- match.arg(qrpEnv, c("none", "low", "medium", "high"))
		
  # Define the QRP environments:
	# get the proportions of studies produced under each QRP strategy
  if (qrpEnv == 'none'){
    noneP = 1; modP = 0; aggP = 0
  } else if (qrpEnv == 'low'){
    noneP = 0.50; modP = 0.40; aggP = 0.10
  } else if (qrpEnv == 'medium'){
		noneP = 0.30; modP = 0.50; aggP = 0.20
  } else if (qrpEnv == 'high'){
		noneP = 0.10; modP = 0.40; aggP = 0.50
  } else {
		print('ERROR: qrpEnv must be none, low, medium, or high')
	}
  
	
	datMA <- data.frame()
	
	# repeatedly add a new study from that environment until the desired number of k is achieved
	repeat {		
		thisStudiesHackingStyle <- sample(x = c("none", "mod", "agg"), size=1, replace=TRUE, prob = c(noneP, modP, aggP))
		
		if (thisStudiesHackingStyle == "none") {
      res <- simData.noQRP(delta=delta, tau=tau)			
      res[11] = 0 #QRP style
		} else if (thisStudiesHackingStyle == "mod") {
      res <- simData.QRP(delta=delta, tau=tau, QRP.strategy="mod")			
      res[11] = 1 #QRP style
		} else if (thisStudiesHackingStyle == "agg") {
      res <- simData.QRP(delta=delta, tau=tau, QRP.strategy="agg")			
      res[11] = 2 #QRP style
		}
		
		# inflict publication bias via the censoring function
		if (is.character(censorFunc) && censorFunc == "none") {
			publish <- 1
		} else if (is.character(censorFunc) && censorFunc == "medium") {
			# predefined censor function for "medium publication bias"
			publish <- rbinom(n=1, size=1, prob=censorMedium(pObs = res[2], direction = sign(res[1])))
		} else if (is.character(censorFunc) && censorFunc == "high") {
			# predefined censor function for "strong publication bias"
			publish <- rbinom(n=1, size=1, prob=censorHigh(pObs = res[2], direction = sign(res[1])))
		} else if (is.vector(censorFunc) && length(censorFunc)==3) {
			publish <- rbinom(n=1, size=1, prob=censor(res[2], direction = sign(res[1]), posSign_NS_baseRate = censorFunc[1], negSign_NS_baseRate = censorFunc[2], counterSig_rate = censorFunc[3]))
		} else {
			stop("Wrong specification of censor function!")
		}
				
		if (publish == 1) {
			datMA <- rbind(datMA, res)
			if (verbose==TRUE) print(nrow(datMA))
		}
		
		if (nrow(datMA) >= k) {break}
		
	} # of repeat
  
  #name columnes
  colnames(datMA) = c( 'd',       # effect size, d
                       'p',       # p value for the two group comparison
                       't',       # t value for the two group comparison
                       'N',       # total N
                       'v',       # variance for the effect size
                       'se',      # standard error for the effect size
                       'pow',     # power given the true effect for the two group comparison
                       'n1',      # experimental group sample size
                       'n2',      # control group sample size
                       'delta_i', # the study-level true effect
                       'qrp')     # 0 = 'none', 1 = 'mod', 2 = 'agg'
											 
											 
 	# Add Hedge's correction factor
	df = datMA$n1 + datMA$n2 - 2
 	J = 1- 3/(4*df - 1)
 	datMA$g = datMA$d*J
 	datMA$g_v = datMA$v*J^2
 	datMA$g_se = sqrt(datMA$g_v)											 
  
  return(datMA)
}


#s1 <- simMA(100, delta=0, tau=0, qrpEnv= "medium", censorFunc = "none", verbose=FALSE)
#s2 <- simMA(100, delta=0, tau=0, qrpEnv= "high", censorFunc = "none", verbose=FALSE)

# sanity check: does n1 and n2 add up to N?
# s1 <- simMA(10, delta=0, tau=0, qrpEnv= "none", censorFunc = "none", verbose=FALSE); s1; all(s1$N == s1$n1 + s1$n2)
# s1 <- simMA(10, delta=0, tau=0, qrpEnv= "medium", censorFunc = "none", verbose=FALSE); s1; all(s1$N == s1$n1 + s1$n2)
# s1 <- simMA(10, delta=0, tau=0, qrpEnv= "high", censorFunc = "none", verbose=FALSE); s1; all(s1$N == s1$n1 + s1$n2)
#
# s1 <- simMA(10, delta=0, tau=0, qrpEnv= "none", censorFunc = "medium", verbose=FALSE); s1; all(s1$N == s1$n1 + s1$n2)
# s1 <- simMA(10, delta=0, tau=0, qrpEnv= "medium", censorFunc = "medium", verbose=FALSE); s1; all(s1$N == s1$n1 + s1$n2)
# s1 <- simMA(10, delta=0, tau=0, qrpEnv= "high", censorFunc = "medium", verbose=FALSE); s1; all(s1$N == s1$n1 + s1$n2)
#
# s1 <- simMA(10, delta=0, tau=0, qrpEnv= "none", censorFunc = "high", verbose=FALSE); s1; all(s1$N == s1$n1 + s1$n2)
# s1 <- simMA(10, delta=0, tau=0, qrpEnv= "medium", censorFunc = "high", verbose=FALSE); s1; all(s1$N == s1$n1 + s1$n2)
# s1 <- simMA(10, delta=0, tau=0, qrpEnv= "high", censorFunc = "high", verbose=FALSE); s1; all(s1$N == s1$n1 + s1$n2)