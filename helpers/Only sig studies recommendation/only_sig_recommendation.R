load("../../dataFiles/res.wide.red.RData")

# how many PET slopes are (significantly) negative?

# ---------------------------------------------------------------------
# Look at results with 100% sig. studies

# propagate kSig from pcurve rows to all rows
res.wide.allSig <- res.wide.red %>% group_by(id) %>% 
	mutate(kSig_all = max(kSig_estimate, na.rm=TRUE)) %>% 
	ungroup() %>% 
	filter(kSig_all == k)
	
nrow(res.wide.red)
nrow(res.wide.allSig)

summ.allSig <- res.wide.allSig %>% group_by(condition, k, k.label, delta, delta.label, qrpEnv, qrp.label, selProp, selProp.label, tau, tau.label, method) %>% 
	dplyr::summarise(
		meanEst		= mean(b0_estimate, na.rm=TRUE),
		ME 			= mean(b0_estimate - delta, na.rm=TRUE),
		RMSE		= sqrt(mean((b0_estimate - delta)^2, na.rm=TRUE)),
		MAD			= mean(abs(b0_estimate - delta), na.rm=TRUE), # mean absolute deviation
		perc2.5		= quantile(b0_estimate, probs=.025, na.rm=TRUE),
		perc97.5	= quantile(b0_estimate, probs=.975, na.rm=TRUE),
		coverage 	= sum(delta > b0_conf.low & delta < b0_conf.high)/sum(!is.na(b0_conf.high)),
		consisZero  = sum(0 > b0_conf.low & 0 < b0_conf.high)/n()
	)


	summ.allSig %>% filter(selProp > 0, !method %in% c("pcurve.evidence", "pcurve.hack", "pcurve.lack", "3PSM")) %>% 
		ggplot(aes(x=k.label, y=meanEst, ymin=perc2.5, ymax=perc97.5, color=factor(delta), shape=qrpEnv)) + 
		geom_pointrange(position=position_dodge(width=0.7)) + 
		geom_hline(aes(yintercept=delta, color=factor(delta))) + 
		coord_flip(ylim=c(-0.6, 1.1)) +
		facet_grid(selProp~tau~method) + 
		theme_bw() + ggtitle("Estimate and 95% bootstrap percentiles (for selProp = 0% and tau=0.2)")		