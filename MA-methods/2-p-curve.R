#### functions for p-curve ES. Code adapted from Uri Simonsohn ####

#LOSS FUNCTION
pcurve_loss <- function(t.sig, df.sig, dobs) {
  #################################################################################################
  #SYNTAX:
  #1. t.sig is a vector with observed significant t-values, 
  #2. df.sig vector with degrees of freedom associated with each t-value
  #3. dobs is the effect size on which fitted p-curve is based and the measure of loss computed
  
  # Note: Only provide significant t values!
  #################################################################################################
  
  # suppress warnings for KS test and pt precision
  options(warn=-1)
  
  #4.Compute non-centrality parameter implied by dobs and df.sig
  #df+2 is total N. 
  #Becuase the noncentrality parameter for the student distribution is ncp=sqrt(n/2)*d, 
  #we add 2 to d.f. to get N,  divide by 2 to get n, and by 2 again for ncp, so -->df+2/4
  ncp_est <- sqrt((df.sig+2)/4)*dobs                          
  
  #5.Find critical t-value for p=.05 (two-sided)
  #this is used below to compute power, it is a vector as different tests have different dfs 
  #and hence different critical values
  tc <- qt(.975, df.sig)                     
  
  #4.Find power for ncp given tc, again, this is a vector of implied power, for ncp_est,  for each test
  power_est <- 1-pt(tc, df.sig, ncp_est)
  
  #5.Compute pp-values
  #5.1 First get the overall probability of a t>tobs, given ncp
  p_larger <- pt(t.sig,df=df.sig,ncp=ncp_est)
  
  #5.2 Now, condition on p<.05
  ppr <- (p_larger-(1-power_est))/power_est  #this is the pp-value for right-skew
  ppr <- ppr[!is.na(ppr)]
  
  # Are there enough observations?
  if (length(ppr) == 0) {
	  KSD <- 99999999 	# return a large number (instead of NA) because this is a loss function
  } else {
	  #6. Compute the gap between the distribution of observed pp-values and a uniform distribution 0,1 
	  #this is the D statistic outputted by the KS test against uniform
	  KSD <- ks.test(ppr, punif)$statistic        
  }
  
  options(warn=0)
  return(KSD)          
}



#Function 2: Estimate d
pcurve_estimate_d <- function(tobs, dfobs, dmin=-1, dmax=4, dstart=NA)
{
  #################################################################################################
  #SYNTAX:
  #tobs  : vector with observed t-values 
  #dfobs : vector with degrees of freedom associated with each t-value
  #dmin   : smallest  effect size to consider 
  #dnax   : largest   effect size to consider 
  #e.g., dmin=-1, dmax=1 would look for the best fitting effect size in the d>=-1 and d<=1 range
  #################################################################################################
  
  loss.test=c()
  
  #1.Convert all ts to the same sign (for justification see Supplement 5) 
  tobs <- abs(tobs)
  
  #2 Compute p-values
  pobs <- 2*(1-pt(tobs, df=dfobs))
  
  #3 Keep significant t-values and corresponding df.
  t.sig <- subset(tobs, pobs<.05)
  df.sig <- subset(dfobs, pobs<.05)
  
  if (length(t.sig) <= 1) return(NA)
  
  # If no starting value is provided: Compute a good starting value for the optimizer
  if (is.na(dstart)) {
	  dtest.set <- seq(dmin, dmax, length.out=20)
	  options(warn=-1)               #turn off warning becuase R does not like its own pt() function!
	  for (dtest in dtest.set) {    
	    loss.test <- c(loss.test, pcurve_loss(t.sig=t.sig, df.sig=df.sig, dobs=dtest))
	  }
	  options(warn=0)                #turn warnings back on
  
	  #find the effect leading to smallest loss in the test set; use as starting value in optim
	  dstart <- dtest.set[which.min(loss.test)]
  }
  
  #optimize around the global minimum
  dhat <- optimize(pcurve_loss, c(dstart-.2, dstart+.2), df.sig=df.sig, t.sig=t.sig)
  return(list(d=dhat$minimum, nStudies=length(t.sig)))
}



#Function 3: Compute bootstrapped CI for p-curve estimate
pcurve_estimate_d_CI <- function(tobs, dfobs, dmin=0, dmax=4, B=1000, progress=TRUE) {
	
	d.boot <- c()
	dstart <- pcurve_estimate_d(tobs, dfobs, dmin=dmin, dmax=dmax)$d
	
	if (progress==TRUE) {
		library(progress)
		pb <- progress_bar$new(format="Bootstrapping [:bar] :percent ETA: :eta", total=B, clear=FALSE)
	}
	
	for (i in 1:B) {
		if (progress==TRUE) pb$tick()
		
		# resample:
		t.resample <- sample(tobs, length(tobs), replace=TRUE)
		df.resample <- sample(dfobs, length(dfobs), replace=TRUE)
		d.boot <- c(d.boot, pcurve_estimate_d(t.resample, df.resample, dmin=dmin, dmax=dmax, dstart=dstart)$d)
	}

	return(d.boot)
}

#' @param t A vector of t values
#' @param df A vector of associated dfs
#' @param CI Should the CI be computed? (Needs bootstrapping; takes long)
#' @param level The coverage of the CI (default: 95%)
#' @param B Number of bootstrap samples for CI
#' @param progress Should a progress bar be displayed for the CI bootstrapping?
#' @param long Should the results be returned in long format?
pcurveEst <- function(t, df, CI=TRUE, level=.95, B=1000, progress=TRUE, long=TRUE) {
	out <- matrix(NA, 1, 4)
	colnames(out) <- c("dPcurve","lbPcurve","ubPcurve", "nstudiesPcurve")
	
	est <- pcurve_estimate_d(tobs=t, dfobs=df)
	
	if (!is.na(est)) {
		out[1, 1] <- est$d
		out[1, 4] <- est$nStudies
		if (CI==TRUE) {
			d.boot <- pcurve_estimate_d_CI(t, df, dmin=-2, dmax=4, B=B, progress=progress)
			CI.est <- quantile(d.boot, prob=c((1-level)/2, 1-(1-level)/2))
			out[1, 2:3] <- CI.est
		}
	} else {
		out[1, 4] <- 0
	}
	
    if (long==FALSE) {
    	return(out)
    } else {
  	  outlong <- data.frame(method="pcurve", variable=c("d", "lb", "ub", "nStudies"), value=out[1, ])
  	  rownames(outlong) <- NULL
  	  return(outlong)
    }
}


# # test: Unbiased set of studies
# dat <- dataMA(50, meanD=0.5, sigma=0, sel=0, propB=0)
# system.time({
# 	pcurveEst(dat$t, dat$N-2, CI=FALSE)
# })

## Test wide format
#pcurveEst(dat$t, dat$N-2, CI=TRUE, long=FALSE)

# # test: biased set of studies
# dat2 <- dataMA(500, meanD=0.3, sel=1, propB=0.8)
# system.time({
# 	pcurveEst(dat2$t, dat2$N-2, CI=FALSE)
# })