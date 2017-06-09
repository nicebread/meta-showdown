library(dplyr)
library(ggplot2)

load("../res.wide.RData")

PUNI <- res.wide[res.wide$method == "puniform", ]
PUNI$noCI.LB <- is.na(PUNI$b0_conf.low)
table(PUNI$noCI.LB)

# check the raw data of the failing CI in p-uniform
load("../simParts/simData_condition_149.RData")

# id = batch + replication + condition 
# --> id = 505149 --> batch==5, replication==5
critical <- sim %>% filter(batch==5, replication==5)

library(puniform)
puniform(tobs=critical$t, n1i=critical$n1, n2i=critical$n2, alpha = 0.05, side="right", method="P", plot = FALSE)

# ---------------------------------------------------------------------
# Compare estimates from puniforms with and without lower CI

posify <- function(x) {x[x<0] <- 0; return(x)}
summ.PU <- PUNI %>% group_by(condition, k, k.label, delta, qrp.label, selProp, selProp.label, tau, tau.label, noCI.LB) %>% 
	dplyr::summarise(
		meanEst		= mean(b0_estimate, na.rm=TRUE),
		ME 			= mean(b0_estimate - delta, na.rm=TRUE),
		RMSE		= sqrt(mean((b0_estimate - delta)^2, na.rm=TRUE)),
		MAD			= mean(abs(b0_estimate - delta), na.rm=TRUE), # mean absolute deviation
		perc2.5		= quantile(b0_estimate, probs=.025, na.rm=TRUE),
		perc97.5	= quantile(b0_estimate, probs=.975, na.rm=TRUE),
		coverage 	= sum(delta > b0_conf.low & delta < b0_conf.high)/sum(!is.na(b0_conf.high)),
		consisZero  = sum(0 > b0_conf.low & 0 < b0_conf.high)/n(),
		meanEst.pos = mean(posify(b0_estimate), na.rm=TRUE),
		perc2.5.pos		= quantile(posify(b0_estimate), probs=.025, na.rm=TRUE),
		perc97.5.pos	= quantile(posify(b0_estimate), probs=.975, na.rm=TRUE)
	)


summ.PU %>% filter(tau==0.4) %>% 
	ggplot(aes(x=k.label, y=meanEst.pos, ymin=perc2.5.pos, ymax=perc97.5.pos, color=factor(delta), shape = noCI.LB)) + 
	geom_pointrange(position=position_dodge(width=0.7)) + 
	geom_hline(aes(yintercept=delta, color=factor(delta))) + 
	coord_flip(ylim=c(-0.1, 1.1)) +
	facet_grid(qrp.label~selProp~tau.label) + 
	theme_bw() + ggtitle("Estimate and 95% bootstrap percentiles (for tau=0.2, k==30)")