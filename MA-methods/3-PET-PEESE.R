#' @param long Should the results be returned in long format?
lmVarEst <- function(d, v, long=TRUE) {
  
  #analyzes MA data set using two lm() models
  #produces estimates of true effect (and CIs) for
  #PET, PEESE, and PET-PEESE
  #also produces FAT p-value
  
  PET.lm <- lm(d~sqrt(v), weights=1/v)
  PEESE.lm <- lm(d~v, weights=1/v)
  PET.rma <- rma(d, v, mods=sqrt(v), method="REML")
  PEESE.rma <- rma(d, v, mods=v, method="REML")  
  
  res <- rbind(
		data.frame(method="PET.lm", tidyLM(PET.lm)),
		data.frame(method="PEESE.lm", tidyLM(PEESE.lm)),
		data.frame(method="PET.rma", tidyRMA(PET.rma)),
		data.frame(method="PEESE.rma", tidyRMA(PEESE.rma))
	  )
  
 
  # conditional PET/PEESE estimator
  #the one-tail version that Stanley privately advocated. Not mentioned in publications. Use two-tailed (below) instead.
  #usePET = ifelse(summary(PET)$coefficients[7] < .10 & as.numeric(PET$coefficients[1]) > 0, 0, 1)
  
  #the two-tail version. Note the change in the conditional argument. 
  usePET.lm <- ifelse(res %>% filter(method == "PET.lm", term == "b0") %>% .[["p.value"]] > .05, TRUE, FALSE)
  usePET.rma <- ifelse(res %>% filter(method == "PET.rma", term == "b0") %>% .[["p.value"]] > .05, TRUE, FALSE)
  
  res <- rbind(res, 
		data.frame(method="PETPEESE.lm", if (usePET.lm == TRUE) {tidyLM(PET.lm)} else {tidyLM(PEESE.lm)}),
		data.frame(method="PETPEESE.rma", if (usePET.rma == TRUE) {tidyRMA(PET.rma)} else {tidyRMA(PEESE.rma)})
	  )

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
