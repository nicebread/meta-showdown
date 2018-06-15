# comment codes:
# 0 = regular converged estimate
# 1 = "No significant studies on the specified side": return NA
# 2 = "set to zero if avg. p-value > .025": return 0
# 3 = "Not converged for other reasons": return NA

puniformEst <- function(t.value, n1, n2, skipBarelySignificant=TRUE, long=TRUE) {
	
	p.values <- pt(t.value, n1+n2-2, lower.tail=FALSE)*2	# two-tailed
	kSig <- sum(p.values<.05)
	
	# initialize empty results object
	res <- data.frame(
		method = "puniform",
		term = "b0",
		estimate = NA,
		std.error = NA,
		statistic = NA,
		p.value = NA,	# one-tailed p-value of p-uniform's test of null-hypothesis of no effect
		conf.low = NA,
		conf.high = NA,
		comment = NA
	)
	
	# catch two boundary cases
	
	returnSpecial <- FALSE
	
	# 1. check if there are >= 1 studies in the correct significant direction
	if (kSig < 1) {
		returnSpecial <- TRUE
		res$estimate <- NA
		res$comment <- 1
	}
	
	# 2. "Set the effect-size estimate of p-uniform or p-curve equal to zero if the average p value of the statistically significant studies is larger than .025"
	# see van Aert, R. C. M., Wicherts, J. M., & van Assen, M. A. L. M. (2016). Conducting Meta-Analyses Based on pValues. Perspectives on Psychological Science, 11(5), 713–729. http://doi.org/10.1177/1745691616650874, Table 1, recommendation 4
	if (skipBarelySignificant == TRUE & kSig >= 1 & mean(p.values[p.values < .05]) > .025) {
		returnSpecial <- TRUE
		res$estimate <- 0
		res$comment <- 2
	}
	
	if (returnSpecial == TRUE) {
		res <- plyr::rbind.fill(res, data.frame(
			method="puniform",
			term="kSig",
			value=kSig
		))
		
		return(returnRes(res, long))		
	}
	
	# No special case? Then proceed with the puniform function
	# If the standard "P" method fails, fallback to "KS"
	# by using side="right", the p-uniform already does a one-sided selection of p-values
	
	PU <- tryCatch(
		puniform(tobs=t.value, n1i=n1, n2i=n2, alpha = 0.05, side="right", method="P", plot = FALSE), 
		error = function(e) {
			warning("P-uniform method 'P' failed.")
			res$comment <- 3
			return(return(returnRes(res, long)))
		}
	)
	
	
	res <- data.frame(
		method = "puniform",
		term = "b0",
		estimate = PU$est,
		std.error = NA,
		statistic = PU$L.0,
		p.value = PU$pval.0*2,	# one-tailed p-value of p-uniform's test of null-hypothesis of no effect.
		# We double the p-value here to get two-sided p-values (all other methods return two-sided values, and we want to have it comparable)
		conf.low = PU$ci.lb,
		conf.high = PU$ci.ub,
		comment = 0
	)
	res <- plyr::rbind.fill(res, data.frame(
		method="puniform",
		term="kSig",
		value=PU$ksig
	))
	res <- plyr::rbind.fill(res, data.frame(
		method="puniform",
		term="PB.test",
		statistic=PU$L.pb,
		p.value=PU$pval.pb
	))
	
    returnRes(res, long)
}


# t.value <- c(1.1, 1.2, 1.1)
# n1 <- n2 <- c(20, 23, 24)
# pt(t.value, n1+n2-2, lower.tail=FALSE)*2
# puniformEst(t.value, n1, n2, skipBarelySignificant=TRUE)