#' @param long Should the results be returned in long format?
reEst <- function(d, v, long=TRUE) {
  
  #analyzes MA data set using standard RE model estimators
  #produces estimate of true effect, CI around estimate,
  #and estimate of tau (could get CIs if wanted)
 
  
  out = matrix(NA,1,8)
  colnames(out) = c("dRE","lbRE","ubRE","tauRE","dTF","lbTF","ubTF","filledTF")
  
  reMA = rma(d, v, method="DL")
  tfMA = trimfill(reMA)
  
  out[1,1] = as.numeric(reMA$b[,1])
  out[1,2] = reMA$ci.lb
  out[1,3] = reMA$ci.ub
  
  out[1,4] = as.numeric(sqrt(reMA$tau2))
    
  out[1,5] = as.numeric(tfMA$b[,1])
  out[1,6] = tfMA$ci.lb
  out[1,7] = tfMA$ci.ub
  
  out[1,8] = tfMA$k0

  if (long==FALSE) {
  	return(out)
  } else {
        outlong <- data.frame(method=c(rep("RE", 3), "Tau", rep("TF", 3), "fill"), 
                              variable=c(c("d", "lb", "ub"), "tauEst", c("d", "lb", "ub"),"kFilled"), 
                              value=out[1, ])
	  rownames(outlong) <- NULL
	  return(outlong)
  }
}
