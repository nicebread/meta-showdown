#' @param long Should the results be returned in long format?
lmVarEst <- function(d, v, long=TRUE) {
  
  #analyzes MA data set using two lm() models
  #produces estimates of true effect (and CIs) for
  #PET, PEESE, and PET-PEESE
  #also produces FAT p-value
  
  out = matrix(NA,1,10)
  colnames(out) = c("dPT","lbPT","ubPT","dPE","lbPE","ubPE","dPP","lbPP","ubPP","fatP")
  
  PET = lm(d~sqrt(v),weights=1/v)
  PEESE = lm(d~v,weights=1/v)
  ciPET = confint(PET)
  ciPEESE = confint(PEESE)
  
  #the one-tail version that Stanley privately advocated. Not mentioned in publications.
  #usePET = ifelse(summary(PET)$coefficients[7] < .10 & as.numeric(PET$coefficients[1]) > 0, 0, 1)
  
  #the two-tail version. Note the change in the conditional argument. 
  usePET = ifelse(summary(PET)$coefficients[7] > .05, 1, 0)
  
  #the estimates of delta
  out[,1] = as.numeric(PET$coefficients[1])     #dPT
  out[,4] = as.numeric(PEESE$coefficients[1])   #dPE
  out[,7] = if(usePET==1){out[,1]}else{out[,4]} #dPP
  
  #PET CI limits
  out[,2] = ciPET[1]
  out[,3] = ciPET[3]
  
  #PEESE CI limits
  out[,5] = ciPEESE[1]
  out[,6] = ciPEESE[3]
  
  #PET-PEESE CI limits
  out[,8] = if(usePET==1){out[,2]}else{out[,5]}
  out[,9] = if(usePET==1){out[,3]}else{out[,6]}
  
  out[,10] = summary(PET)$coefficients[8]

  if (long==FALSE) {
  	return(out)
  } else {
	  outlong <- data.frame(method=c(rep("PET", 3), rep("PEESE", 3), rep("PET-PEESE", 3), "FAT"), variable=c(rep(c("d", "lb", "ub"), 3), "p.value"), value=out[1, ])
	  rownames(outlong) <- NULL
	  return(outlong)
  }
}
