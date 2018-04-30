ensemble.est <- function(MAdat, resample=TRUE, includeComponents=FALSE, B=10000, long=TRUE, verbose=FALSE) {
	
	if (includeComponents == TRUE) {
		# run standard estimators
		res <- rbind(
				PETPEESE.est(MAdat$d, MAdat$v, PP.test = "one-sided", long=TRUE, runRMA=FALSE),
				puniformEst(t.value=MAdat$t, n1=MAdat$n1, n2=MAdat$n2, skipBarelySignificant=TRUE),
				threePSM.est(d=MAdat$d, v=MAdat$v, min.pvalues=1, long=TRUE),
				fourPSM.est(d=MAdat$d, v=MAdat$v, min.pvalues=1, long=TRUE, fallback=FALSE),
				WAAP.est(d=MAdat$d, v=MAdat$v, long=TRUE)
			)
		} else {
			res <- data.frame()
		}
	
	if (resample==TRUE) {

		res.resample <- data.frame()
		for (i in 1:B) {
			if (verbose==TRUE) print(i)
		
			MAdat0 <- MAdat[sample(nrow(MAdat), replace=TRUE), ]
			
			res0 <- rbind(
				PETPEESE.est(MAdat0$d, MAdat0$v, PP.test = "one-sided", long=TRUE, runRMA=FALSE),
				puniformEst(t.value=MAdat0$t, n1=MAdat0$n1, n2=MAdat0$n2, skipBarelySignificant=TRUE),
				threePSM.est(d=MAdat0$d, v=MAdat0$v, min.pvalues=1, long=TRUE),
				#fourPSM.est(d=MAdat0$d, v=MAdat0$v, min.pvalues=1, long=TRUE, fallback=FALSE),
				WAAP.est(d=MAdat0$d, v=MAdat0$v, long=TRUE)
			)
	
			res1 <- res0 %>% filter(method %in% c("PETPEESE.lm", "puniform", "3PSM", "WAAP-WLS"), variable == "estimate", term=="b0") %>% mutate(B=i)
		
			res.resample <- rbind(res.resample, res1)
		}
		
	  res.ensemble <- rbind(
			data.frame(
		    method = "ensemble1",
		    term = "b0",
		    estimate = 	mean(res.resample$value),
		    conf.low = quantile(res.resample$value, prob=.025),
		    conf.high = quantile(res.resample$value, prob=.975)
		  )# ,
# 			data.frame(
# 		    method = "ensemble2",
# 		    term = "b0",
# 		    estimate = 	mean(res.resample$value[res.resample$method %in% c("")]),
# 		    conf.low = quantile(res.resample$value[res.resample$method %in% c("")], prob=.025),
# 		    conf.high = quantile(res.resample$value[res.resample$method %in% c("")], prob=.975)
# 		  )
		)
				
		res <- rbind(res, returnRes(res.ensemble))
	}
			
  return(res)
}

# ensemble.est(MAdat, resample=FALSE, includeComponents=TRUE, B=10, verbose=FALSE)
# ensemble.est(MAdat, resample=TRUE, includeComponents=TRUE, B=10, verbose=FALSE)
# ensemble.est(MAdat, resample=TRUE, includeComponents=FALSE, B=10, verbose=FALSE)

# R <- list()
# for (i in 1:1000) {
# 	print(i)
# 	MAdat <- simMA(k=20, delta=.3, tau=.2, qrpEnv="medium", censorFunc = "medium", empN = TRUE)
# 	R[[i]] <- data.frame(
# 		ensemble.est(MAdat, resample=TRUE, B=100, verbose=FALSE),
# 		replication=i
# 	)
# }
#
# R1 <- bind_rows(R)
#
# R1 %>%
# 	filter(method %in% c("PETPEESE.lm", "puniform", "3PSM", "WAAP-WLS", "ensemble"), variable == "estimate", term=="b0") %>%
# 	group_by(method) %>%
# 	summarise(mean.est = mean(value))
#
# res.wide <- dcast(R1, replication + method ~ term + variable, value.var="value")
#
# delta <- 0.3
#
# summ <- res.wide %>%
# 	group_by(method) %>%
# 		dplyr::summarise(
# 			meanEst		= mean(b0_estimate, na.rm=TRUE),
# 			ME 			= mean(b0_estimate - delta, na.rm=TRUE),
# 			RMSE		= sqrt(mean((b0_estimate - delta)^2, na.rm=TRUE)),
# 			MAD			= mean(abs(b0_estimate - delta), na.rm=TRUE), # mean absolute deviation
#
# 			perc2.5		= quantile(b0_estimate, probs=.025, na.rm=TRUE),
# 			perc97.5	= quantile(b0_estimate, probs=.975, na.rm=TRUE),
# 			CI.length = perc97.5 - perc2.5,
#
# 			coverage 	= sum(delta > b0_conf.low & delta < b0_conf.high, na.rm=TRUE) / sum(!is.na(b0_conf.high)),
# 			coverage.pos 	= sum(delta > b0_conf.low & delta < b0_conf.high & b0_estimate > 0, na.rm=TRUE) / sum(!is.na(b0_conf.high) & b0_estimate > 0)
# 		)