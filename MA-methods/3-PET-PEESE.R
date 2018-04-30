#' @param long Should the results be returned in long format?
#' @param PP.test "one-sided" or "two-sided" (Stanley's default = one-sided)

# estimator_type: 1 = WAAP; 2 = WLS; 3 = PET; 4 = PEESE

PETPEESE.est <- function(d, v, PP.test = "one-sided", runRMA=FALSE, long=TRUE) {
  
  PP.test <- match.arg(PP.test, PP.test)
  
  PET.lm <- lm(d~sqrt(v), weights=1/v)
  PEESE.lm <- lm(d~v, weights=1/v)
  
  res <- rbind(
		data.frame(method="PET.lm", tidyLM(PET.lm)),
		data.frame(method="PEESE.lm", tidyLM(PEESE.lm))
	)  
 
  # conditional PET/PEESE estimator	
	lm.p.value  <- res %>% filter(method == "PET.lm", term == "b0") %>% .[["p.value"]]
	lm.est  <- res %>% filter(method == "PET.lm", term == "b0") %>% .[["estimate"]]


  # "For the purpose of deciding which meta-regression accommodation for selective reporting bias to employ, we recommend testing H0:b0 < 0 at the 10% significance level."
	# From: Stanley, T. D. (2016). Limitations of PET-PEESE and other meta-analysis methods. Abgerufen von https://www.hendrix.edu/uploadedFiles/Departments_and_Programs/Business_and_Economics/AMAES/Limitations%20of%20PET-PEESE.pdf


  if (PP.test == "one-sided") {
	  p.crit <- .10
  } else if (PP.test == "two-sided") {
		p.crit <- .05
  }
	
	# the condition always uses a directional hypothesis (a reversed slope should not happen), but different critical levels (.05 vs .10)
  usePEESE.lm <- ifelse(lm.p.value < p.crit & lm.est > 0, TRUE, FALSE)
    	
  res <- rbind(res,
		data.frame(method="PETPEESE.lm", if (usePEESE.lm == TRUE) {tidyLM(PEESE.lm)} else {tidyLM(PET.lm)})
	)
		
	
	if (runRMA == TRUE) {
		PET.rma <- tryCatch(
	  	rma(yi = d, vi = v, mods=sqrt(v), method="REML", control = list(stepadj = .5, maxiter=500)),
			error = function(e) rma(yi = d, vi = v, mods=sqrt(v), method="DL")
		)
	
		PEESE.rma <- tryCatch(
			rma(yi = d, vi = v, mods=v, method="REML", control = list(stepadj = .5, maxiter=500)),
			error = function(e) rma(yi = d, vi = v, mods=v, method="DL")
		)  
		
	  res <- rbind(
			res,
			data.frame(method="PET.rma", tidyRMA(PET.rma)),
			data.frame(method="PEESE.rma", tidyRMA(PEESE.rma))
		)
	
		rma.p.value <- res %>% filter(method == "PET.rma", term == "b0") %>% .[["p.value"]]	
		rma.est <- res %>% filter(method == "PET.rma", term == "b0") %>% .[["estimate"]]
	  usePEESE.rma <- ifelse(rma.p.value < p.crit & rma.est > 0, TRUE, FALSE)
	  res <- rbind(res,
			data.frame(method="PETPEESE.rma", if (usePEESE.rma == TRUE) {tidyRMA(PEESE.rma)} else {tidyRMA(PET.rma)})
		)
	}
	
	
	res <- plyr::rbind.fill(res, data.frame(method="PETPEESE.lm", term="estimator", type=ifelse(usePEESE.lm == TRUE, 4, 3)))
	
	if (runRMA == TRUE) {
		res <- plyr::rbind.fill(res, data.frame(method="PETPEESE.rma", term="estimator", type=ifelse(usePEESE.rma == TRUE, 4, 3)))	
	} else {
		res$method <- gsub(".lm", "", res$method, fixed=TRUE)
	}

  returnRes(res, long)
}
