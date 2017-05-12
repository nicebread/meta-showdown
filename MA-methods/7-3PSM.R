# ---------------------------------------------------------------------
#  Estimate the three-parameter selection model (3PSM) implemented in McShane et al. 2016
# For estimation functions, see 7b-selection.meta.functions.R
#' @param long Should the results be returned in long format?

TPSM.est <- function(t, n1, n2, long=TRUE) {	

	# Three-parameter selection model
	# MBH Implmentation: init.value gives an initial guess for the effect size, heterogeneity, and relative
	# likelihood of reporting a study that is not statistically significant and directionally consistent.
	
	# use very general starting values for the parameters (not tuned to our specific simulations)
	theta.init <- c(expected.d = 0.3, max.tau= 0.5, p.report = 0.99)
	alpha <- 0.05

	mm <- NULL
	try(mm <- estimate.onestep.selection.heterogeneous(t, n1, n2, alpha/2, theta.init), silent=FALSE)

	res.wide <- NULL
	if(!is.null(mm) & all(complete.cases(mm[[2]]))){
		SE <- sqrt(diag(mm[[2]]))
		res.wide <- data.frame(
			method = "3PSM",
			term = c("b0", "max.tau", "p.report"),
			estimate = mm[[1]],
			std.error = SE,
			statistic = NA,
			p.value = pnorm(abs(mm[[1]]) / SE, lower.tail=FALSE)*2,
			conf.low = mm[[1]] + qnorm(alpha/2)*SE,
			conf.high = mm[[1]] + qnorm(1-alpha/2)*SE
		)
	}
	
	returnRes(res.wide)
}

# set.seed(5)
# dat <- dataMA(k = 12, delta = 0.41, tau = 0.01, empN = TRUE, maxN=500, minN=0, meanN=0, selProp = 0, qrpEnv = "none")
# dat <- data.frame(dat)
# TPSM.est(t=dat$t, n1=dat$n1, n2=dat$n2)
