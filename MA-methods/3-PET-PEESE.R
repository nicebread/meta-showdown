#' @param long Should the results be returned in long format?
PETPEESE.est <- function(d, v, PP.test = c("two-sided", "one-sided"), long=TRUE) {
  
  PP.test <- match.arg(PP.test, PP.test)
  
  PET.lm <- lm(d~sqrt(v), weights=1/v)
  PEESE.lm <- lm(d~v, weights=1/v)
  PET.rma <- rma(yi = d, vi = v, mods=sqrt(v), method="DL")
  PEESE.rma <- rma(yi = d, vi = v, mods=v, method="DL")  
  
  res <- rbind(
		data.frame(method="PET.lm", tidyLM(PET.lm)),
		data.frame(method="PEESE.lm", tidyLM(PEESE.lm)),
		data.frame(method="PET.rma", tidyRMA(PET.rma)),
		data.frame(method="PEESE.rma", tidyRMA(PEESE.rma))
	  )
  
 
  # conditional PET/PEESE estimator

  #the one-tail version that Stanley privately advocated. Not mentioned in publications. 
  #usePET = ifelse(summary(PET)$coefficients[7] < .10 & as.numeric(PET$coefficients[1]) > 0, 0, 1)
  if (PP.test == "one-sided") {
	  coef.lm <- summary(PET.lm)$coefficients
	  coef.rma <- summary(PET.rma)$coefficients
	  usePET.lm <- !(coef.lm["(Intercept)", "Pr(>|t|)"] < .10 & coef.lm["(Intercept)", "Estimate"] > 0)
	  usePET.rma <- !(coef.rma["(Intercept)", "Pr(>|t|)"] < .10 & coef.rma["(Intercept)", "Estimate"] > 0)
  }
  
  #the two-tail version.
  if (PP.test == "two-sided") {
	  usePET.lm <- ifelse(res %>% filter(method == "PET.lm", term == "b0") %>% .[["p.value"]] > .05, TRUE, FALSE)
	  usePET.rma <- ifelse(res %>% filter(method == "PET.rma", term == "b0") %>% .[["p.value"]] > .05, TRUE, FALSE)
  }
    
  res <- rbind(res, 
		data.frame(method="PETPEESE.lm", if (usePET.lm == TRUE) {tidyLM(PET.lm)} else {tidyLM(PEESE.lm)}),
		data.frame(method="PETPEESE.rma", if (usePET.rma == TRUE) {tidyRMA(PET.rma)} else {tidyRMA(PEESE.rma)})
	  )

  returnRes(res, long)
}
