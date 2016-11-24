#' ---
#' title: "Estimation"
#' author: "Felix Schönbrodt"
#' output: pdf_document
#' classoption: landscape
#' ---



library(ggplot2)
library(dplyr)

load("summ.RData")

# ---------------------------------------------------------------------
# Show estimate

# remove selProp = 0 (unrealistic; show in Appendix)
summ2 <- summ %>% filter(
	!method %in% c("PET.lm", "PEESE.lm", "pcurve.evidence", "pcurve.lack")
	)

#+ echo=FALSE, fig.width=11, fig.height=8
summ2 %>% filter(selProp==0, tau==0.2) %>% 
	ggplot(aes(x=qrp.label, y=meanEst, ymin=perc2.5, ymax=perc97.5, color=factor(delta))) + 
	geom_pointrange(position=position_dodge(width=0.7)) + 
	geom_hline(aes(yintercept=delta, color=factor(delta))) + 
	coord_flip(ylim=c(-0.6, 1.1)) +
	facet_grid(k.label~method) + 
	theme_bw() + ggtitle("Estimate and 95% bootstrap percentiles (for selProp = 0% and tau=0.2)")


summ2 %>% filter(selProp==0.6, tau==0.2) %>% 
	ggplot(aes(x=qrp.label, y=meanEst, ymin=perc2.5, ymax=perc97.5, color=factor(delta))) + 
	geom_pointrange(position=position_dodge(width=0.7)) + 
	geom_hline(aes(yintercept=delta, color=factor(delta))) + 
	coord_flip(ylim=c(-0.6, 1.1)) +
	facet_grid(k.label~method) + 
	theme_bw() + ggtitle("Estimate and 95% bootstrap percentiles (for selProp = 60% and tau=0.2)")

summ2 %>% filter(selProp==0.6, tau==0.4) %>% 
	ggplot(aes(x=qrp.label, y=meanEst, ymin=perc2.5, ymax=perc97.5, color=factor(delta))) + 
	geom_pointrange(position=position_dodge(width=0.7)) + 
	geom_hline(aes(yintercept=delta, color=factor(delta))) + 
	coord_flip(ylim=c(-0.6, 1.1)) +
	facet_grid(k.label~method) + 
	theme_bw() + ggtitle("Estimate and 95% bootstrap percentiles (for selProp = 60% and tau=0.4)")
	
	
summ2 %>% filter(selProp==0.90, tau==0.2) %>% 
	ggplot(aes(x=qrp.label, y=meanEst, ymin=perc2.5, ymax=perc97.5, color=factor(delta))) + 
	geom_pointrange(position=position_dodge(width=0.7)) + 
	geom_hline(aes(yintercept=delta, color=factor(delta))) + 
	coord_flip(ylim=c(-0.6, 1.1)) +
	facet_grid(k.label~method) + 
	theme_bw() + ggtitle("Estimate and 95% bootstrap percentiles (for selProp = 95% and tau=0.2)")
	
	
summ2 %>% filter(selProp==0.90, tau==0.4) %>% 
	ggplot(aes(x=qrp.label, y=meanEst, ymin=perc2.5, ymax=perc97.5, color=factor(delta))) + 
	geom_pointrange(position=position_dodge(width=0.7)) + 
	geom_hline(aes(yintercept=delta, color=factor(delta))) + 
	coord_flip(ylim=c(-0.6, 1.1)) +
	facet_grid(k.label~method) + 
	theme_bw() + ggtitle("Estimate and 95% bootstrap percentiles (for selProp = 95% and tau=0.4)")

