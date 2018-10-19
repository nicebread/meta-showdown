## ======================================================================
## This code checks the alternative interpretation of p-curve (a la Simonsohn)
## which states that p-curve is supposed to recover the "average true effect of studies submitted to p-curve"
## =====================================================================

library(dplyr)
library(ggplot2)
load("dataFiles/res.wide.RData")

# res.wide$qrp.label <- factor(res.wide$qrpEnv, levels=c("none", "med", "high"), labels=paste0("QRP = ", c("none", "med", "high")), ordered=TRUE)
# res.wide$delta.label <- factor(res.wide$delta, levels=c(0, 0.2, 0.5, 0.8), labels=paste0("delta = ", c(0, 0.2, 0.5, 0.8)), ordered=TRUE)
# res.wide$censor <- factor(res.wide$selProp, levels=unique(res.wide$selProp), labels=paste0("PB = ", unique(res.wide$selProp)))

# ---------------------------------------------------------------------
#  Compute summary measures across replications

# use the data set without any reductions (i.e., also keep p-curves with <=3 sign. studies)
PC <- res.wide %>% filter(method=="pcurve", !is.na(kSig_estimate) & kSig_estimate >= 1)

summ.PC <- PC %>% group_by(condition, k, k.label, delta, delta.label, qrpEnv, qrp.label, censor, censor.label, tau, tau.label) %>% 
	dplyr::summarise(
		meanEst.AEO		= mean(b0_estimate, na.rm=TRUE),
		ME.AEO 			= mean(b0_estimate - delta.included_mean, na.rm=TRUE),
		RMSE.AEO		= sqrt(mean((b0_estimate - delta.included_mean)^2, na.rm=TRUE)),		
		perc2.5.AEO		= quantile(b0_estimate, probs=.025, na.rm=TRUE),
		perc97.5.AEO	= quantile(b0_estimate, probs=.975, na.rm=TRUE),
		
		nSig = mean(kSig_estimate)
	)


# average kSig in tau=0 conditions:

summ.PC %>% filter(tau==0, delta == 0, qrpEnv=="none") %>% dplyr::select(1:8, nSig)

# ---------------------------------------------------------------------
# Plot

# order the delta.label factor alphabetically
summ.PC$delta.label2 <- factor(summ.PC$delta.label, levels=sort(levels(summ.PC$delta.label)))

summ.PC$censor.label2 <- factor(paste0("PB = ", summ.PC$censor))

# raw estimates (not posified)
summ.PC %>% 
	ggplot(aes(x=k.label, y=ME.AEO, shape=delta.label2)) + 
	geom_point(position=position_dodge(width=0.7)) + 
	geom_hline(yintercept=0) + 
	coord_flip(ylim=c(-0.4, 0.25)) +
	xlab("k") + ylab("Mean error (relative to average true effect size of studies submitted to p-curve)") +
	facet_grid(tau.label~censor.label2~qrp.label) + 
	guides(shape=guide_legend("True effect size")) + xlab("") +
	theme_bw()

ggsave("Plots/ME_AEO_raw.jpg", dpi=120)	
	
