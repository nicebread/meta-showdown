#### functions for p-curve ES. Code adapted from Uri Simonsohn ####

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
  # here let's define dmin and dmax? 
  dmin = -2
  dmax = 4
  # pcurve_prep is called first to sort the data into a frame and verify it is compatible
  pc_data = pcurve_prep(t_obs = t, df_obs = df)
  # now let's get the pcurve ES estimate
  out[1, 1] <- optimize(pcurve_loss, c(dmin, dmax), pc_data = pc_data)$minimum
  if (CI==TRUE) {
    d.boot <- pcurve_estimate_d_CI(pc_data = pc_data, dmin=dmin, dmax=dmax, B=B, progress=progress)
    CI.est <- quantile(d.boot, prob=c((1-level)/2, 1-(1-level)/2))
    out[1, 2:3] <- CI.est
  }
  
  if (long==FALSE) {
    return(out)
  } else {
    outlong <- data.frame(method="pcurve", variable=c("d", "lb", "ub"), value=out[1, ])
    rownames(outlong) <- NULL
    return(outlong)
  }
}


##########################
# pcurve_prep takes in vectors of t-values and associated degs. freedom
# packages everything into a data.frame
# strips out anything with p > .05
# and warns the user if any negative t-values have been provided (pcurve is unsigned)
# all pre-processing and validation should go in here
pcurve_prep <- function(t_obs, df_obs){
  # first, calculate p-values and d-values for all studies
  d_vals = (t_obs*2)/sqrt(df_obs)
  p_vals = (1 - pt(t_obs, df_obs)) * 2
  # then, shove everything into a data.frame to keep things organized
  unfiltered_data = data.frame(t_obs, df_obs, p_vals, d_vals)
  # now for the checks. 
  # first ensure that all t-values are positive. pcurve cannot accommodate negative values.
  # if negative t-values are present, halt execution. 
  # next, if any p-values are greater than .05, remove them and warn
  # the user that they were present/are now removed.
  # finally, if less than 4 good studies are left, abort: k=3 is a joke
  neg_tvalues = filter(unfiltered_data, t_obs < 0)$t_obs
  NS_studies = filter(unfiltered_data, p_vals > .05)$p_vals
  clean_data = filter(unfiltered_data, p_vals < .05)
  if (length(NS_studies) > 0){
    msg = paste0(length(NS_studies), " NS studies were removed from dataset")
    warning(msg)
  }
  if (length(neg_tvalues) > 0){
    msg = "Negative t-values present in dataset, aborting"
    stop(msg)
  }
  if (length(clean_data$p_vals) < 4){
    msg = "Less than 4 pcurvable studies available, aborting"
    stop(msg)
  }
  # all done!
  return(clean_data)
}


######################################
# pcurve_loss function
# this code mirrors the functionality of the original loss function
# written by Simonsohn et al.
pcurve_loss <- function(pc_data, dobs) {
  options(warn=-1)
  t.sig = pc_data$t_obs
  df.sig = pc_data$df_obs
  ncp_est <- sqrt((df.sig+2)/4)*dobs                          
  tc <- qt(.975, df.sig)                     
  power_est <- 1-pt(tc, df.sig, ncp_est)
  p_larger <- pt(t.sig,df=df.sig,ncp=ncp_est)
  ppr <- (p_larger-(1-power_est))/power_est 
  KSD <- ks.test(ppr, punif)$statistic
  options(warn=0)
  return(KSD)          
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
		resample_data = sample_n(pc_data, length(pc_data), replace=TRUE)
		#
		d.boot <- c(d.boot, optimize(pcurve_loss, c(dmin, dmax), pc_data = resample_data)$minimum)
	}

	return(d.boot)
}



# # test: Unbiased set of studies
# dat <- dataMA(50, meanD=0.5, sigma=0, sel=0, propB=0)
# system.time({
# 	pcurveEst(dat$t, dat$N-2, CI=FALSE)
# })

## Test long format
#pcurveEst(dat$t, dat$N-2, CI=TRUE, long=TRUE)

# # test: biased set of studies
# dat2 <- dataMA(500, meanD=0.3, sel=1, propB=0.8)
# system.time({
# 	pcurveEst(dat2$t, dat2$N-2, CI=TRUE)
# })
