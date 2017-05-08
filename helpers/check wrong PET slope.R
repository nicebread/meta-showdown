load("../res.wide.red.RData")

# how many PET slopes are (significantly) negative?

res.PET <- res.wide.red %>% filter(method == "PET.lm", delta %in% c(0, 0.5))
prop.table(table("Slope = positive" = res.PET$b1_estimate > 0, "p < .10" = res.PET$p.value < .10))
round(prop.table(table("Slope = positive" = res.PET$b1_estimate > 0, "p < .10" = res.PET$p.value < .10, "selProp" = res.PET$selProp), margin=c(3)), 2)

round(prop.table(table("Slope = positive" = res.PET$b1_estimate > 0, "p < .10" = res.PET$p.value < .10, "selProp" = res.PET$selProp, "H0" = res.PET$delta==0), margin=c(3, 4)), 2)



res.PET$posSlope <- res.PET$b1_estimate > 0

posify <- function(x) {x[x<0] <- 0; return(x)}
summ.PET <- res.PET %>% group_by(condition, k, k.label, delta, qrp.label, selProp, selProp.label, tau, tau.label, posSlope) %>% 
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


summ.PET %>% filter(tau==0.2, k==30) %>% 
	ggplot(aes(x=k.label, y=meanEst.pos, ymin=perc2.5.pos, ymax=perc97.5.pos, color=factor(delta))) + 
	geom_pointrange(position=position_dodge(width=0.7)) + 
	geom_hline(aes(yintercept=delta, color=factor(delta))) + 
	coord_flip(ylim=c(-0.1, 1.1)) +
	facet_grid(posSlope~qrp.label~selProp) + 
	theme_bw() + ggtitle("Estimate and 95% bootstrap percentiles (for tau=0.2, k==30)")