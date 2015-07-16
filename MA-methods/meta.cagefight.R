library(pwr)
library(truncnorm)
library(psych)
library(grid)
library(ggplot2)
library(reshape2)
library(metafor)



# build a function to run a meta-analysis and store some values. You can tell it whether or not you want publication bias (dropping all nonsignificant studies), and whether or not you want heterogeneity in effects

#### function to run a meta-analysis using pet-peese, peters test, pcurve, random effects, and trim-and fill ####

meta <- function(k, pubbias, het, efx) { ## this is an example for publication bias and homogenous ES
  nobs <- numeric(k)  # set up some empty bins to store outputs
  tobs <- numeric(k)
  pobs <- numeric(k)
  dfobs <- numeric(k)
  d <- numeric(k)
  dobs <- numeric(k)
  sedobs <- numeric(k)
  vardobs <- numeric(k)
  nNeed <- numeric(k)
  mx <- numeric(k)
  my <- numeric(k)
  sdx <- numeric(k)
  sdy <- numeric(k)
  nx <-nobs
 
  #simulate data for each experiment
  
  for(i in 1:k){ #for each simulated experiment
    dtrue <-   if (efx==TRUE) {
      if (het==TRUE) {
      rtruncnorm(1, a=.2, b=.4, mean = .2, sd = .12) #under heterogeneity it picks realistic values between .2 and .4
    } else {
      runif(1,.27,.28) # under homogeneity, it sticks close to the mean of the het. distribution
    }  
      } else {
        0 #d = zero if efx==FALSE
      }
    nreq <-  if (efx==TRUE) {
      round(power.t.test(d=dtrue, n=NULL, power=.8)$n, 0) #pick n for .8 power
      } else {
        NA
      }
    nused <- round(rtruncnorm(1, a=20, b=200, mean = 30, sd = 50),0) # actual N, again picked at random. I'm using a truncated normal so that n peaks around 30 and tapers from there. max 200, min 20
    x <- rnorm(n = nused, mean = 0, sd = 1) # create data
    y <- rnorm(n = nused, mean = dtrue, sd = 1)
    nobs[i] <- nused  # record n
    nNeed[i] <- nreq
    d[i] <- dtrue #record true d
    mx[i] <- mean(x) # store the stuff needed to calculate d and se(d)
    my[i] <- mean(y)
    sdx[i] <- sd(x)
    sdy[i] <- sd(x)
    test <- t.test(y,x) # run a t test
    tobs[i] <- test$statistic # grab t
    pobs[i] <- test$p.value # grab the p-value
    dfobs[i] <- test$parameter
  }
  
  cor(nobs, nNeed)
  
  meandiff = my-mx # again, just use standard formulae to calculate things
  sdpool = sqrt(((sdx^2+sdy^2)/2))
  varpool = sdpool^2
  dobs <- meandiff/sdpool # here's cohen's d
  vardobs <- (2*nobs/nobs^2)*varpool # calculate the variance from standard formula
  sedobs <- sqrt(vardobs) #ditto
  
  

  ### publication bias. Toss out nonsignificant studies. This section is ommitted in the conditions with no publication bias
  

  simdat <- data.frame(cbind(dobs, sedobs, vardobs, nNeed, nobs, pobs, d)) #gather required inputs
  
  if (pubbias==TRUE) {
	  simdat.selected <- subset(simdat, pobs <=.05 & dobs > 0) # if T, toss all nonsignificant data. Harsh.
  } else {
  	simdat.selected <- simdat # if F, use the full set
  }
  
  
  
  # observed mean ES store this
  
  mean.obs = mean(simdat.selected$dobs)
  
  # real mean ES from ALL studies run (regardless of pub. bias). store this
 
  mean.real = mean(simdat$d)
  
  # return how many studies actually made the cut
  
  kper <- nrow(simdat.selected)
  
  # calculate correlation between actual N and required N among selected studies
  
  nCorr <- if (efx ==T) {
    cor(simdat.selected$nobs, simdat.selected$nNeed)
  } else {
    NA
  }
  
  # give full pet-peese estimates. basically tell it to use pet if can't reject nil, use peese if can
  
  ### run PET-PEESE
  
  pet <- lm(dobs ~ sedobs, data=simdat.selected, weights= 1/vardobs) #run pet, store values
  pet.d <- summary(pet)$coefficients[1]
  pet.ld <- confint(pet)[1,1]
  pet.ud <- confint(pet)[1,2]
  pet.bias <- pet.d-mean.real
  pet.prec <- pet.ud - pet.ld
  pet.sqer = pet.bias^2
  pet.coverage = ifelse(pet.ld <= mean.real & pet.ud >= mean.real, 1, 0)
  pet.zero = ifelse(pet.ld <=0 & pet.ud >= 0, 1, 0)
  
  peese <- lm(dobs ~ vardobs, data=simdat.selected, weights = 1/vardobs)  #run peese, store values
  peese.d <- summary(peese)$coefficients[1]
  peese.ld <- confint(peese)[1,1]
  peese.ud <- confint(peese)[1,2]
  peese.bias <- peese.d-mean.real
  peese.prec <- peese.ud - peese.ld
  peese.sqer = peese.bias^2
  peese.coverage = ifelse(peese.ld <= mean.real & peese.ud >= mean.real, 1, 0)
  peese.zero = ifelse(peese.ld <=0 & peese.ud >= 0, 1, 0)
  
  
  pp.d <- ifelse(pet.ld < 0 & pet.ud > 0, pet.d, peese.d)
  pp.ld <- ifelse(pet.ld < 0 & pet.ud > 0, pet.ld, peese.ld)
  pp.ud <- ifelse(pet.ld < 0 & pet.ud > 0, pet.ud, peese.ud)
  
  pp.bias <- pp.d-mean.real
  pp.prec <- pp.ud - pp.ld
  pp.sqer = pp.bias^2
  pp.coverage = ifelse(pp.ld <= mean.real & pp.ud >= mean.real, 1, 0)
  pp.zero = ifelse(pp.ld <=0 & pp.ud >= 0, 1, 0)

  
  
  ## Peters method
  
  simdat.selected$pred <- 1/(2*simdat.selected$nobs)
  simdat.selected$wt <- simdat.selected$pred^-1
  
  peters <- lm(dobs ~ pred, data=simdat.selected, weights = wt)
  peters.d <- summary(peters)$coefficients[1]
  peters.ld <- confint(peters)[1,1]
  peters.ud <- confint(peters)[1,2]
  
  peters.bias =  peters.d - mean.real
  peters.prec <- peters.ud - peters.ld
  peters.sqer = peters.bias^2
  peters.coverage = ifelse(peters.ld <= mean.real & peters.ud >= mean.real, 1, 0)
  peters.zero = ifelse(peters.ld <=0 & peters.ud >= 0, 1, 0)
  
  ## add in p curve

  pcurve.d <- pcurve_estimate_d(tobs=tobs,dfobs=dfobs,dmin=-.2,dmax=2)
  pcurve.bias <- pcurve.d - mean.real
  pcurve.sqer = pcurve.bias^2
  
  
  
  ### random effects MA
  
  re<-rma(dobs, sei=vardobs, data=simdat.selected)
  re.d <- re$b[1]
  re.ld <- re$ci.lb
  re.ud <- re$ci.ub
  
  re.bias <- re.d-mean.real
  re.prec <- re.ud-re.ld
  re.sqer = re.bias^2
  re.coverage = ifelse(re.ld <= mean.real & re.ud >= mean.real, 1, 0)
  re.zero = ifelse(re.ld <=0 & re.ud >= 0, 1, 0)
  
  ## trimfill
  rtf<-trimfill(re)
  
  tf.d <- rtf$b[1]
  tf.ld <- rtf$ci.lb
  tf.ud <- rtf$ci.ub
  
  tf.bias <- tf.d - mean.real
  tf.prec <- tf.ud - tf.ld
  tf.sqer = tf.bias^2
  tf.coverage = ifelse(tf.ld <= mean.real & tf.ud >= mean.real, 1, 0)
  tf.zero = ifelse(tf.ld <=0 & tf.ud >= 0, 1, 0)
  
  return(data.frame(mean.real, mean.obs,kper, nCorr, pet.d, peese.d, pp.d, peters.d, pcurve.d, re.d, tf.d,pet.bias, peese.bias, pp.bias, peters.bias, pcurve.bias, re.bias, tf.bias, pet.prec, peese.prec, pp.prec, peters.prec, re.prec, tf.prec, pet.sqer, peese.sqer, pp.sqer, peters.sqer, pcurve.sqer, re.sqer, tf.sqer,pet.coverage, peese.coverage, pp.coverage, peters.coverage, re.coverage, tf.coverage, pet.zero, peese.zero, pp.zero, peters.zero, re.zero, tf.zero)) # this tells it what to spit out for each meta-analysis
}


### so, that was creating a function for generating 100 studies and using pet-peese to meta-analyze them. 

# to test it, you can just run the function once
k = 100 # number of studies per meta
pubbias = T #do you include publication bias?
het =  T# do you want heterogenous data?
efx = T #do you want a real effect?
meta(k, pubbias, het, efx) # run a meta-analysis without publication bias






#### simulate a bunch of meta-analyses under different conditions ####

set.seed(9999) # this should ensure similar results

nMeta <- 500 # how many meta-analyses do you want?

phet <- data.frame() #this is the meta-meta simulation for publication bias + heterogeneity
for (j in 1:nMeta) {
	print(paste0("Running ", j, "/", nMeta))
	phet <- rbind(phet, meta(k=100, pubbias=T, het=T, efx=T))
}
summary(phet)


phom <- data.frame() #this is the meta-meta simulation for publication bias + homogeneity. it also makes me want to eat pho
for (j in 1:nMeta) {
  print(paste0("Running ", j, "/", nMeta))
  phom <- rbind(phom, meta(k=100, pubbias=T, het=F, efx=T))
}
summary(phom)


nhet <- data.frame() #this is the meta-meta simulation for NO publication bias + heterogeneity
for (j in 1:nMeta) {
  print(paste0("Running ", j, "/", nMeta))
  nhet <- rbind(nhet, meta(k=100, pubbias=F, het=T, efx=T))
}
summary(nhet)


nhom <- data.frame() #this is the meta-meta simulation for publication bias + homogeneity
for (j in 1:nMeta) {
  print(paste0("Running ", j, "/", nMeta))
  nhom <- rbind(nhom, meta(k=1000, pubbias=F, het=F, efx=T))
}
summary(nhom)


nil <- data.frame() #this is the meta-meta simulation for nil effects, publication bias
for (j in 1:nMeta) {
  print(paste0("Running ", j, "/", nMeta))
  nil <- rbind(nil, meta(k=1000, pubbias=T, het=F, efx=F))
}
summary(nil)




