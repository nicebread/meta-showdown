#' @param long Should the results be returned in long format?
RMA.est <- function(d, v, long=TRUE) {
  
  #analyzes MA data set using standard RE model estimators
  #produces estimate of true effect, CI around estimate,
  #and estimate of tau (could get CIs if wanted)

	# adjust stepadj (make it smaller by x0.5) and increase maxiter from 100 to 500 to prevent convergence problems
	#reMA <- rma(d, v, method="REML")
  reMA <- rma(d, v, method="REML", control = list(stepadj = .5, maxiter=500))
  
  # assign NULL to tfMA if an error is raised
	tfMA <- NULL
  try({
	  tfMA <- trimfill(reMA, side="left", maxiter=500)
  }, silent=TRUE)  
  
  res <- data.frame(method="reMA", tidyRMA(reMA))
  res <- plyr::rbind.fill(res, data.frame(
	  method="reMA",
	  term=c("tau2", "I2", "Q"),
	  estimate=c(reMA$tau2, reMA$I2, reMA$QE),
	  std.error=c(reMA$se.tau2, NA, NA)
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
