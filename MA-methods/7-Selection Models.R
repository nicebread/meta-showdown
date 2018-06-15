# ---------------------------------------------------------------------
# Estimate the 1PSM with the weightr package, as an alternative ML implementation of pcurve/puniform
# the single cut point is at .025 (one-sided testing)

#' @param min.pvalues How many p-values must be present in each bin that the function returns an estimate?

onePSM.McShane.est <- function(t.obs, n1, n2, long=TRUE) {	
	
	alpha <- .05
	
	# define weights to inflict 100% publication bias for non-sig results
	w1 <- tryCatch(
		#weightfunct(d, v, steps = c(0.025, 1), mods = NULL, weights = c(1, 0), fe = TRUE, table = TRUE),
		estimate.extreme.selection.homogeneous(t.obs, n1, n2, alpha = alpha/2, interval = c(0, 2)),
		error = function(e) NULL
	)
	
  # initialize as empty df
  res.wide <- data.frame(
    method = "1PSM",
    term = "b0",
    estimate = NA,
    std.error = NA,
    statistic = NA,
    p.value = NA,
    conf.low = NA,
    conf.high = NA
  )
  if(!is.null(w1)) {
    if (all(complete.cases(w1))) {
      SE <- w1[2]
      res.wide <- data.frame(
        method = "1PSM",
        term = c("b0"),
        estimate = w1[1],
        std.error = SE,
        statistic = NA,
        p.value = pnorm(abs(w1[1]) / SE, lower.tail=FALSE)*2,
        conf.low = w1[1] + qnorm(alpha/2)*SE,
        conf.high = w1[1] + qnorm(1-alpha/2)*SE
      )
    }
  }
  
  returnRes(res.wide)
}

# ---------------------------------------------------------------------
# Estimate the 3PSM (a single step-cutpoint) with the weightr package (should be equivalent to the McShane implementation)
# the single cut point is at .025 (one-sided testing)
# The authors suggest to have >= 4 p-values in each interval. 

#' @param min.pvalues How many p-values must be present in each bin that the function returns an estimate?

threePSM.est <- function(d, v, min.pvalues=0, long=TRUE) {	
	
	w1 <- tryCatch(
		weightfunct(d, v, steps = c(0.025, 1), mods = NULL, weights = NULL, fe = FALSE),
		error = function(e) NULL
	)
	
  res.NA <- data.frame(
    method = "3PSM",
    term = c("tau2", "b0", "pr.nonsig"),
    estimate = NA,
    std.error = NA,
    statistic = NA,
    p.value = NA,
    conf.low = NA,
    conf.high = NA
  )
	
	if (is.null(w1)) return(returnRes(res.NA))
	
	# if <= min.pvalues p-values in an interval: return NA
	p.table <- table(cut(w1$p, breaks=c(0, .025, 1)))
	if (any(p.table < min.pvalues)) {
	  return(returnRes(res.NA))
	} else {
		est <- w1[[2]]$par
	
		# compute standard errors from hessian
		std.err <- sqrt(abs(diag(solve(w1[[2]]$hessian))))

	
	  res.wide <- data.frame(
	    method = "3PSM",
	    term = c("tau2", "b0", "pr.nonsig"),
	    estimate = est,
	    std.error = std.err,
	    statistic = est/std.err,
	    p.value = pnorm(est/std.err, lower.tail=FALSE)*2,
	    conf.low = est + qnorm(.025)*std.err,
	    conf.high = est + qnorm(1-.025)*std.err
	  )
	}
	
	 return(returnRes(res.wide))
}



# ---------------------------------------------------------------------
# Estimate the 4PSM (two step-cutpoints) with the weightr package
# The authors suggest to have >= 4 p-values in each interval. If that is not provided, return NA.
# optionally fallback to 3PSM if not enough p.values in each interval

#' @param min.pvalues How many p-values must be present in each bin that the function returns an estimate?
#' @param fallback If 4PSM has not enough p-values: Should the function try to estimate a 3PSM instead?

fourPSM.est <- function(d, v, min.pvalues=0, long=TRUE, fallback = FALSE) {	
	w1 <- tryCatch(
		weightfunct(d, v, steps = c(0.025, 0.5, 1), mods = NULL, weights = NULL, fe = FALSE, table = TRUE),
		error = function(e) NULL
	)
	
  res.NA <- data.frame(
    method = "4PSM",
    term = c("tau2", "b0", "pr.nonsig", "pr.opposite"),
    estimate = NA,
    std.error = NA,
    statistic = NA,
    p.value = NA,
    conf.low = NA,
    conf.high = NA
  )
	
	if (is.null(w1)) return(returnRes(res.NA))
	
	# if <= min.pvalues p-values in an interval: return NA
	p.table <- table(cut(w1$p, breaks=c(0, .025, 0.5, 1)))
	if (any(p.table < min.pvalues)) {
		if (fallback==TRUE) {
			return(threePSM.est(d, v, min.pvalues=min.pvalues, long=long))
		} else {
		   return(returnRes(res.NA))
		}	  
	} else {
		est <- w1[[2]]$par
	
		# compute standard errors from hessian
		std.err <- sqrt(abs(diag(solve(w1[[2]]$hessian))))

	  res.wide <- data.frame(
	    method = "4PSM",
	    term = c("tau2", "b0", "pr.nonsig", "pr.opposite"),
	    estimate = est,
	    std.error = std.err,
	    statistic = est/std.err,
	    p.value = pnorm(est/std.err, lower.tail=FALSE)*2,
	    conf.low = est + qnorm(.025)*std.err,
	    conf.high = est + qnorm(1-.025)*std.err
	  )
	}
	
	 return(returnRes(res.wide))
}


# ---------------------------------------------------------------------
#  Estimate the three-parameter selection model (3PSM) implemented in McShane et al. 2016
# For estimation functions, see 7b-selection.meta.functions.R
#' @param long Should the results be returned in long format?

TPSM.McShane.est <- function(t, n1, n2, long=TRUE) {	
  
  # Three-parameter selection model
  # McShane et al. implmentation: init.value gives an initial guess for the effect size, heterogeneity, and relative
  # likelihood of reporting a study that is not statistically significant and directionally consistent.
  
  # use very general starting values for the parameters (not tuned to our specific simulations)
  theta.init <- c(expected.d = 0.3, max.tau= 0.5, p.report = 0.99)
  alpha <- 0.05
  
  mm <- NULL
  try(mm <- estimate.onestep.selection.heterogeneous(t, n1, n2, alpha/2, theta.init), silent=FALSE)
  
  # initialize as empty df
  res.wide <- data.frame(
    method = "3PSM",
    term = "b0",
    estimate = NA,
    std.error = NA,
    statistic = NA,
    p.value = NA,
    conf.low = NA,
    conf.high = NA
  )
  if(!is.null(mm)) {
    if (all(complete.cases(mm[[2]]))) {
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
  }
  
  returnRes(res.wide)
}

# set.seed(5)
# dat <- simMA(k = 30, delta = 0.2, tau = 0.1, empN = TRUE, maxN=500, minN=0, meanN=0, censor="B", qrpEnv = "med")
# TPSM.McShane.est(t=dat$t, n1=dat$n1, n2=dat$n2)
# threePSM.est(d=dat$d, v=dat$v)
# fourPSM.est(d=dat$d, v=dat$v)
# pcurveEst(t=dat$t, df=dat$n1 + dat$n2 - 2, progress=FALSE, long=TRUE, CI=FALSE)
# puniformEst(t.value=dat$t, n1=dat$n1, n2=dat$n2, skipBarelySignificant=TRUE)
# onePSM.McShane.est(t.obs=dat$t, n1=dat$n1, n2=dat$n2)

# d = dat$d
# v=dat$v
# t.obs = dat$t
# n1 = dat$n1
# n2 = dat$n2
