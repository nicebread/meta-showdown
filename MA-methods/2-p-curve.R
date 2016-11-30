#### functions for p-curve ES. Code adapted from Uri Simonsohn ####

######################################
# pcurve_loss function
# this code mirrors the functionality of the original loss function
# written by Simonsohn et al.
pcurve_loss <- function(pc_data, dobs) {
  options(warn=-1)
  t.sig <- pc_data$t_obs
  df.sig <- pc_data$df_obs
  ncp_est <- sqrt((df.sig+2)/4)*dobs                          
  tc <- qt(.975, df.sig)                     
  power_est <- 1-pt(tc, df.sig, ncp_est)
  p_larger <- pt(t.sig,df=df.sig,ncp=ncp_est)
  ppr <- (p_larger-(1-power_est))/power_est
  
  # Problem: ks.test gives an error if the number of test statistics is small and
  # bootstrapping selects a weird sample. In case of errors, return a large loss value
 KSD <- tryCatch({
      ks.test(ppr, punif)$statistic
  }, error = function(e) {
	  return(1e10) # return a large loss function value
  })
  
  # print progression of loss function
  #cat(paste0("dobs=", round(dobs, 3), "; loss=", round(KSD, 3), "\n"))
  
  options(warn=0)
  return(KSD)          
}

##########################
# pcurveEst is the function that should be called to provide p-curve
# estimates of effect size, in addition to bootstrapped confidence intervals
# (if desired). See parameters below for details.
#
#' @param t t-values
#' @param df degrees of freedom
#' @param CI Should the CI be computed? (Needs bootstrapping; takes long)
#' @param level The coverage of the CI (default: 95%)
#' @param B Number of bootstrap samples for CI
#' @param progress Should a progress bar be displayed for the CI bootstrapping?
#' @param long Should the results be returned in long format?
pcurveEst <- function(t, df, CI=TRUE, level=.95, B=1000, progress=TRUE, long=TRUE) {
  require(dplyr) # dplyr is needed for filter functions below
  out <- matrix(NA, 1, 3)
  colnames(out) <- c("dPcurve","lbPcurve","ubPcurve")

  # define dmin and dmax (the range of parameter search)
  dmin <- -2
  dmax <- 4
  
  # pcurve_prep is called first to sort the data into a frame and verify it is compatible
  pc_data = pcurve_prep(t_obs = t, df_obs = df)
  # next we check to make sure we have more than 0 rows (at least 1 study); if not, return a null
  if(nrow(pc_data) == 0){
	outlong <- data.frame(method="pcurve", term="b0", variable=c("estimate"), value=NA)
    return(outlong)
  }
  
  # now let's get the pcurve ES estimate
  out[1, 1] <- optim(par=0, fn=pcurve_loss, pc_data = pc_data, method="BFGS")$par
  
  if (CI==TRUE) {
	  warning("CI not properly implemented.")
    #d.boot <- pcurve_estimate_d_CI(pc_data = pc_data, dmin=dmin, dmax=dmax, B=B, progress=progress)
    #CI.est <- quantile(d.boot, prob=c((1-level)/2, 1-(1-level)/2))
    #out[1, 2:3] <- CI.est
  }
  
  if (long==FALSE) {
    return(out)
  } else {
    outlong <- data.frame(method="pcurve", term="b0", variable=c("estimate", "conf.low", "conf.high"), value=out[1, ])
	
	outlong <- plyr::rbind.fill(outlong, data.frame(
		method="pcurve",
		term="kSig",
		variable="estimate",
		value=nrow(pc_data)
	))
	
    rownames(outlong) <- NULL
    return(outlong)
  }
}


##########################
# pcurve_prep takes in vectors of t-values and associated degs. freedom
# packages everything into a data.frame
# strips out anything with p > .05
# also strips out any negative t-values
# all pre-processing and validation should go in here
pcurve_prep <- function(t_obs, df_obs){
  # first, calculate p-values and d-values for all studies
  d_vals = (t_obs*2)/sqrt(df_obs)
  p_vals = (1 - pt(t_obs, df_obs)) * 2
  # then, shove everything into a data.frame to keep things organized
  unfiltered_data = data.frame(t_obs, df_obs, p_vals, d_vals)
  # now for the checks. 
  # strip out anything that is NS, or if t < 0.
  # Note that we're not throwing any warnings out here.
  unfiltered_data = filter(unfiltered_data, t_obs > 0)
  clean_data = filter(unfiltered_data, p_vals < .05)
  # all done!
  return(clean_data)
}



######################################
# pcurve_estimate_d_CI obtains bootstrapped resamples of the provided dataset
# and estimates the ES of every resample using pcurve. 
pcurve_estimate_d_CI <- function(pc_data, dmin, dmax, B, progress=TRUE) {
	d.boot <- c()
	if (progress==TRUE) {
		require(progress)
		pb <- progress_bar$new(format="Bootstrapping [:bar] :percent ETA: :eta", total=B, clear=FALSE)
	}
	
	for (i in 1:B) {
		if (progress==TRUE) pb$tick()
	  # get a random resample, with replacement
	  # note that sample() doesn't work here, necessary to use sample_n()
		resample_data = sample_n(pc_data, length(pc_data$t_obs), replace=TRUE)
		#print(resample_data)
		#
		d.boot <- c(d.boot, optimize(pcurve_loss, c(dmin, dmax), pc_data = resample_data)$minimum)
	}

	return(d.boot)
}

# set.seed(1)
#dat <- dataMA(k = 10, delta = -0.7, tau = 0, empN = TRUE, maxN=500, minN=0, meanN=0, selProp = 0, qrpEnv = "none")
#dat <- data.frame(dat)


# # test: biased set of studies
# dat <- data.frame(dataMA(k = 40, delta = 0.15, tau = 0.05, empN = TRUE, maxN=500, minN=0, meanN=0, selProp = 0.9, qrpEnv = "low"))
# 	pcurveEst(dat$t, dat$N-2, CI=FALSE)
#	pc_skew(dat$t, dat$N-2)

# Compare with p-curve.com
# cat(paste(paste0("t(", dat$N-2, ")=", round(dat$t, 4)), collapse="\n"))

#TPSM.est(dat$t, dat$n1, dat$n2)
