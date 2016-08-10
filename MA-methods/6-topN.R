topN <- function(d, v, n1, n2, est=c("fixed", "rma", "PEESE"), fixed.effect=0.3, adequate.power=0.80, long=TRUE) {
	
	if (est=="fixed") {
		eff <- fixed.effect
	}
	if (est == "rma") {
		RE <- reEst(d, v, long=FALSE)
		eff <- RE[RE$method=="reMA" & RE$term=="b0", "estimate"]
	}
	if (est == "PEESE") {
		PEESE <- lmVarEst(d, v, long=FALSE)
		eff <- PEESE[PEESE$method=="PEESE.rma" & PEESE$term=="b0", "estimate"]
	}
	
	# top-N: only select studies that are adequatly powered (in a one-sided test)
	power <- pwr.t2n.test(n1 = n1, n2= n2, d = eff, sig.level = 0.05, alternative = c("greater"))$power
	
	if (sum(power >= adequate.power) > 1) {
		reMA.topN <- rma(d[power >= adequate.power], v[power >= adequate.power], method="DL")
		
	    res <- data.frame(method=paste0("topN.", est), tidyRMA(reMA.topN))
	    res <- plyr::rbind.fill(res, data.frame(
	  	  method=paste0("topN.", est),
	  	  term="tau2",
	  	  estimate=reMA$tau2,
	  	  std.error=reMA$se.tau2
	  	))
	  res <- plyr::rbind.fill(res, data.frame(
		  method=paste0("topN.", est),
		  term="kSelected",
		  estimate=sum(power >= adequate.power)
		))
  	  res <- plyr::rbind.fill(res, data.frame(
  		  method=paste0("topN.", est),
  		  term="criticalES",
  		  estimate=eff
  		))
		
	} else {
		res <- data.frame(
			method=paste0("topN.", est),
				  term = "b0",
				  estimate = NA,
				  std.error = NA,
				  statistic = NA,
				  p.value = NA,
				  conf.low = NA,
				  conf.high = NA
				)
	}
    
  
	
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