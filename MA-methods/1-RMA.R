#' @param long Should the results be returned in long format?
reEst <- function(d, v, long=TRUE) {
  
  #analyzes MA data set using standard RE model estimators
  #produces estimate of true effect, CI around estimate,
  #and estimate of tau (could get CIs if wanted)

  reMA <- rma(d, v, method="DL")
  
  # assign NULL to tfMA if an error is raised
  tfMA <- tryCatch({
	  tfMA = trimfill(reMA)
  }, error=function(e) NULL)  
  
  res <- data.frame(method="reMA", tidyRMA(reMA))
  res <- plyr::rbind.fill(res, data.frame(
	  method="reMA",
	  term="tau2",
	  estimate=reMA$tau2,
	  std.error=reMA$se.tau2
	))		
	  
    
  if (!is.null(tfMA)) {	  
	  res <- rbind(res, data.frame(method="TF", tidyRMA(tfMA)))
	  res <- plyr::rbind.fill(res, data.frame(
		  method="TF",
		  term="tau2",
		  estimate=tfMA$tau2,
		  std.error=tfMA$se.tau2
		))
  	  res <- plyr::rbind.fill(res, data.frame(
  		  method="TF",
  		  term="kFilled",
  		  estimate=tfMA$k0
  		))
  }
    
  returnRes(res, long)
}
