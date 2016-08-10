puniformEst <- function(t.value, n1, n2, long=TRUE) {
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
	
    if (long==FALSE) {
  	  # return wide format
  	  return(res)
    } else {
  	  # transform to long format
  	  long <- melt(res, id.vars=c("method", "term"))
  	  long <- long %>% filter(!is.na(value)) %>% arrange(method, term, variable)
  	  return(long)
    }
}