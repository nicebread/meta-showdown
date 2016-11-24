source("../start.R", chdir=TRUE)

load("../simParts/YsimData_condition_100.RData")

MAdat <- sim[sim$id==103100, ]
MAdat

RMA.est(d=MAdat$d, v=MAdat$v, long=TRUE)

# no SEs provided
TPSM.est(t=MAdat$t, n1=MAdat$n1, n2=MAdat$n2, long=TRUE)

estimate.onestep.selection.heterogeneous(z.obs=MAdat$t, n1=MAdat$n1, n2=MAdat$n2, alpha=0.05/2, theta.init=c(expected.d = 0.3, max.tau= 0.5, p.report = 0.99))

estimate.onestep.selection.heterogeneous(z.obs=MAdat$t, n1=MAdat$n1, n2=MAdat$n2, alpha=0.05/2, theta.init=c(expected.d = 0.3, max.tau= 0.5, p.report = 0.99))


# ---------------------------------------------------------------------
# Test: 100% sig. studies
MA100 <- MAdat[MAdat$p < .05, ][1:5,]

TPSM.est(t=MA100$t, n1=MA100$n1, n2=MA100$n2, long=TRUE)

estimate.onestep.selection.heterogeneous(z.obs=MA100$t, n1=MA100$n1, n2=MA100$n2, alpha=0.05/2, theta.init=c(expected.d = 0.3, max.tau= 0.5, p.report = 0.99))

estimate.onestep.selection.heterogeneous(z.obs=MAdat$t, n1=MAdat$n1, n2=MAdat$n2, alpha=0.05/2, theta.init=c(expected.d = 0.3, max.tau= 0.5, p.report = 0.99))



# number of sign. studies per condition

tab <- res.wide.red %>% filter(method=="puniform") %>% group_by(condition, k, delta, qrpEnv, selProp, tau) %>% dplyr::summarise(
	MIN.sig = min(kSig_estimate),
	MAX.sig = max(kSig_estimate),
	MEDIAN.sig = median(kSig_estimate)
)

print(tab, n=432)


# ---------------------------------------------------------------------
# Look at results with 100% sig. studies

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

	summ2 %>% filter(selProp==0.95, tau==0.4) %>% 
		ggplot(aes(x=k.label, y=meanEst, ymin=perc2.5, ymax=perc97.5, color=factor(delta))) + 
		geom_pointrange(position=position_dodge(width=0.7)) + 
		geom_hline(aes(yintercept=delta, color=factor(delta))) + 
		coord_flip(ylim=c(-0.6, 1.1)) +
		facet_grid(qrp.label~method) + 
		theme_bw() + ggtitle("Estimate and 95% bootstrap percentiles (for selProp = 0% and tau=0.2)")
		
		
	summ.allSig %>% filter(selProp==0.95, tau==0.4) %>% 
		ggplot(aes(x=k.label, y=meanEst, ymin=perc2.5, ymax=perc97.5, color=factor(delta))) + 
		geom_pointrange(position=position_dodge(width=0.7)) + 
		geom_hline(aes(yintercept=delta, color=factor(delta))) + 
		coord_flip(ylim=c(-0.6, 1.1)) +
		facet_grid(qrp.label~method) + 
		theme_bw() + ggtitle("Estimate and 95% bootstrap percentiles (for selProp = 0% and tau=0.2)")		