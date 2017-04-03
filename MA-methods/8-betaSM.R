# ---------------------------------------------------------------------
# These functions are from Citkowicz, M., & Vevea, J. L. (2017). A parsimonious weight function for modeling publication bias. Psychological Methods, 22(1), 28–41. http://doi.org/10.1037/met0000119

# we only run random effects
source("betaSM_functions/RE.models.R")
source("betaSM_functions/SERE.functions.R")
source("betaSM_functions/SEFE.adjusted.R")
source("betaSM_functions/SERE.adjusted.R")

# full: compute the full range of output? (Slower)
betaSM.est <- function(d, v, long=TRUE, full=FALSE) {

	y <- d
	#Set starting values.
	a <- 1
	b <- 1
	vc <- 0 #Set to zero if estimating a fixed-effects model.
	beta0 <- .5

	#Estimate the p-values.
	#NOTE: This includes the p-value adjustment that allows the adjusted model to run.
	p <- 1-pnorm(y/sqrt(v))

	for (l in 1:length(p)) {
		p[l] <- if (p[l] < .00001) .00001
	  else if (p[l] > .99999) .99999
	  else p[l] 
	}

	#Fit the adjusted beta density weight-function model.
	# This version does account for pub bias
	adjustedREpars <- c(a, b, vc, beta0)
	
	REadjustedest <- c(NA, NA, NA, NA)
	tryCatch({
	  REadjusted <- nlminb(start=adjustedREpars, y=y, v=v, p=p, objective=neglikeadjustedRE, lower=c(0,0,0,-Inf), control=list(eval.max=1000, iter.max=1000, abs.tol=10e-5, rel.tol=10e-5))
	  REadjustedest <- REadjusted$par
	}, error=function(e) {warning("betaSM optimizer did not converge.")})
	

	#Estimate the standard errors of the parameter estimates for both models.
	SEFEadjustedest <- c(REadjustedest[1], REadjustedest[2], REadjustedest[4])
	SEREadjustedest <- c(REadjustedest[1], REadjustedest[2], REadjustedest[3], REadjustedest[4])
	
	FEadjustedSEs <- NA
	FEadjustedcovmat <- NA	  
	REadjustedcovmat <- NA
	REadjustedSEs <- NA
	AdjustedRESEs <- NA
	
	if (!is.na(REadjustedest[3])) {
	  if(REadjustedest[3]==0) {
	    # compute fixed effect (even if intended to be random: if var==0, go for fixed computation)
	    tryCatch({
	      hess <- SEFE.hessian(SEFEadjustedest, y, v, p)
	      covmat <- solve(-hess)
	      
	      SEa <- sqrt(covmat[1,1])
	      SEb <- sqrt(covmat[2,2])
	      SEvc <- NA
	      SEb0 <- sqrt(covmat[3,3])
	      
	      FEadjustedSEs <- c(SEa,SEb,SEvc,SEb0)
	      FEadjustedcovmat <- covmat
	    }, error = function(e) {
	    })
	    
	    
	  } else {
	    # random effects
	    tryCatch({
	      hess <- SERE.hessian(SEREadjustedest, y, v, p)
	      covmat <- solve(-hess)
	      
	      SEa <- sqrt(covmat[1,1])
	      SEb <- sqrt(covmat[2,2])
	      SEvc <- sqrt(covmat[3,3])
	      SEb0 <- sqrt(covmat[4,4])
	      
	      REadjustedSEs <- c(SEa,SEb,SEvc,SEb0)
	      REadjustedcovmat <- covmat
	    }, error = function(e) {
	    })
	  }
	  
	  if(REadjustedest[3]==0) AdjustedRESEs <- FEadjustedSEs else AdjustedRESEs <- REadjustedSEs
	  if(REadjustedest[3]==0) AdjustedREcovmat <- FEadjustedcovmat else AdjustedREcovmat <- REadjustedcovmat
	}

	


	if (full==TRUE) {
		## Fit the unadjusted standard meta-analytic model.
		# This is a model that does not account for pubBias and should be equivalent to standard reMA.
		# WE DO NOT RUN THE UNADJUSTED MODEL
	
		unadjustedREpars <- c(vc, beta0)
		REunadjusted <- nlminb(start=unadjustedREpars, y=y, v=v, objective=neglikeunadjustedRE, lower=c(0,-Inf), control=list(eval.max=1000, iter.max=1000, abs.tol=10e-5, rel.tol=10e-5))
		REunadjustedest <- REunadjusted$par
		
	  print("Unadjusted parameter estimates:"); print("vc beta0"); print(REunadjustedest)
		
		#Estimate the likelihood-ratio test for publication bias.
		# This test fixes a and b from the beta weight function to 1 (i.e., "no publication bias").
		# Sign. p-value means: there is significant publication bias
	
		# WE SKIP THIS TEST, AS WE ONLY COMPUTE THE ADJUSTED MODEL
		LRRE <- -2*(REadjusted$obj - REunadjusted$obj)
		dfLRRE <- length(adjustedREpars) - length(unadjustedREpars)
		pLRRE <- 1-pchisq(LRRE,dfLRRE)
		print("Likelihood-ratio test:"); print("Chi-squared estimate, degrees of freedom, and p-value"); print(c(LRRE,dfLRRE,pLRRE))
		print("Assess convergence:"); print("Unadjusted model Adjusted model"); print(c(REunadjusted$conv,REadjusted$conv))
	}

	#Estimate the data variability using Q-within and I-squared.
	# source("variability.calculation.R")
	#
	# print("Assess convergence:"); print("Unadjusted model Adjusted model"); print(c(REunadjusted$conv,REadjusted$conv))
	#
	# print("Unadjusted parameter estimates:"); print("vc beta0"); print(REunadjustedest)
	# print("Standard errors:"); print(UnadjustedRESEs)
	# print("Covariance matrix:"); print(UnadjustedREcovmat)
	#
	# print("Adjusted parameter estimates:"); print("a b vc beta0"); print(REadjustedest)
	# print("Standard errors:"); print(AdjustedRESEs)
	# print("Covariance matrix:"); print(AdjustedREcovmat)
	#
	
	#
	# print("Q-within estimate, degrees of freedom, and p-value:"); print(c(Qw,dfQw,pQw))
	#
	# print("I-squared estimate:"); print(I2)
	#

	alpha <- 0.05
	est <- REadjustedest[4]
	SE <- AdjustedRESEs[4]
	res.wide <- data.frame(
		method = "betaSM",
		term = c("b0"),
		estimate = est,
		std.error = SE,
		statistic = abs(REadjustedest[4]) / SE,
		p.value = pnorm(abs(REadjustedest[4]) / SE, lower.tail=FALSE)*2,
		conf.low = est + qnorm(alpha/2)*SE,
		conf.high = est + qnorm(1-alpha/2)*SE
	)

	returnRes(res.wide, long)
}


# set.seed(5)
# dat <- dataMA(k = 20, delta = 0.41, tau = 0.10, empN = TRUE, maxN=500, minN=0, meanN=0, selProp = 0.3, qrpEnv = "none")
# dat <- data.frame(dat)
# TPSM.est(t=dat$t, n1=dat$n1, n2=dat$n2)
# betaSM.est(d=dat$d, v=dat$v)
# RMA.est(d=dat$d, v=dat$v)
#
# # check Citkowicz own data set
#
# d <- c(1.01, 0.82, 0.59, 0.44, 0.84, 0.73, 1.12, 0.04, 0.24, 0.32, 1.04, 1.31, 0.59, 0.66, 0.62, 0.47, 1.08, 0.98, 0.26, 0.39, 0.60, 0.94, 0.11)
# v <- c(0.2704, 0.2116, 0.0529, 0.0324, 0.0841, 0.0841, 0.1296, 0.1369, 0.0225, 0.1600, 0.1024, 0.3249, 0.0841, 0.0361, 0.0961, 0.0729, 0.1024, 0.1024, 0.0324, 0.0324, 0.0961, 0.2809, 0.0729)
#
# RMA.est(d=d, v=v)
# betaSM.est(d=d, v=v, full=TRUE)
# betaSM.est(d=d, v=v, full=FALSE)