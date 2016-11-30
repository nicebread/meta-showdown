puniformEst <- function(t.value, n1, n2, long=TRUE) {
	
	# check if there are at least 1 studies in the correct significant direction
	if (sum(pt(t.value, n1+n2-2, lower.tail=FALSE)<.025) < 1) {
		res <- data.frame(
			method = "puniform",
			term = "b0",
			estimate = NA,
			std.error = NA,
			statistic = NA,
			p.value = NA,	# one-tailed p-value of p-uniform's test of null-hypothesis of no effect
			conf.low = NA,
			conf.high = NA
		)
		
		return(returnRes(res, long))
	}
	
	# by using side="right", the p-uniform already does a one-sided selection
	PU <- puniform(tobs=t.value, n1i=n1, n2i=n2, alpha = 0.05, side="right", method="P", plot = FALSE)
	
	res <- data.frame(
		method = "puniform",
		term = "b0",
		estimate = PU$est,
		std.error = NA,
		statistic = PU$L.0,
		p.value = PU$pval.0,	# one-tailed p-value of p-uniform's test of null-hypothesis of no effect
		conf.low = PU$ci.lb,
		conf.high = PU$ci.ub
	)
	res <- plyr::rbind.fill(res, data.frame(
		method="puniform",
		term="kSig",
		estimate=PU$ksig
	))
	res <- plyr::rbind.fill(res, data.frame(
		method="puniform",
		term="PB.test",
		statistic=PU$L.pb,
		p.value=PU$pval.pb
	))
	
    returnRes(res, long)
}