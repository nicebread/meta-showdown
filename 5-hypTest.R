#' The hypothesis test plot

library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)


load(file="res.hyp.RData")

# ---------------------------------------------------------------------
# Compute summary measures across replications

hyp.summ <- res.hyp %>% 
	group_by(condition, k, k.label, delta, delta.label, qrpEnv, qrp.label, selProp, selProp.label, tau, tau.label, method) %>% 
	dplyr::summarise(
		H0.reject = sum(H0.reject, na.rm=TRUE)/sum(!is.na(H0.reject)),
		n.simulations = n()
	) %>% 
	filter(method %in% c("reMA", "TF", "PET.lm", "PEESE.lm", "PETPEESE.lm", "pcurve.evidence", "puniform", "3PSM")) %>% 
	mutate(method = factor(method, levels=c("reMA", "TF", "PET.lm", "PEESE.lm", "PETPEESE.lm", "pcurve.evidence", "puniform", "3PSM"), labels=c("reMA", "TF", "PET", "PEESE", "PET-PEESE", "p-curve", "p-uniform", "3PSM")))
	

# ---------------------------------------------------------------------
# Compute rejection ratios

RR <- hyp.summ %>% ungroup() %>% select(condition, k, delta, qrp.label, selProp, tau, method, H0.reject)
	
RR$TypeI <- RR$H0.reject
RR$TypeI[RR$delta!=0] <- NA

RR$TypeII <- 1-RR$H0.reject
RR$TypeII[RR$delta==0] <- NA
RR$Power <- 1-RR$TypeII

RR$tau.label <- factor(RR$tau, levels=unique(RR$tau), labels=paste0("tau = ", unique(RR$tau)))
RR$selProp.label <- factor(RR$selProp, levels=unique(RR$selProp), labels=paste0("Publication Bias = ", unique(RR$selProp)))

save(RR, file="RR.RData")

# compare delta==0 against delta==0.5
H1 <- 0.5
RR.H0 <- RR %>% filter(delta == 0) %>% select(condition, k, qrp.label, selProp, tau.label, method, TypeI)
RR.H1 <- RR %>% filter(delta == H1) %>% select(k, qrp.label, selProp, tau.label, method, Power)

RR.wide <- inner_join(RR.H0, RR.H1)

RR.wide$rejectionRatio <- RR.wide$Power/RR.wide$TypeI
RR.wide$errorSum <- (1-RR.wide$Power) + RR.wide$TypeI

# res.wide$delta.label <- factor(res.wide$delta, levels=unique(res.wide$delta), labels=paste0("delta = ", unique(res.wide$delta)))
# res.wide$k.label <- factor(res.wide$k, levels=sort(unique(res.wide$k)), labels=paste0("k = ", sort(unique(res.wide$k))))
# res.wide$qrp.label <- factor(res.wide$qrpEnv, levels=unique(res.wide$qrpEnv), labels=paste0("QRP = ", unique(res.wide$qrpEnv)))


save(RR.wide, file="RR.wide.RData")


theme_metashowdown <- theme(
  panel.spacing =unit(0.5, "lines"),
  panel.background = element_rect(fill="white"),
  panel.border = element_rect(color="grey70",fill=NA, size = 1), #element_blank(),
  panel.grid.minor= element_blank(),
  panel.grid.major= element_blank(),#element_line(color="grey90"),
  strip.background = element_rect(colour="white", fill="white"), #"grey93"
  axis.ticks = element_line(color="lightgrey"),
  legend.position = "none", #c("bottom"),
	axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)
)


buildFacet <- function(dat, title) {
	PLOT <- dat %>%
	  ggplot(aes(x=factor(k), shape=qrp.label)) + 
	  geom_hline(yintercept=c(.05, .50, .80), linetype="dotted", color="skyblue") +
		geom_hline(yintercept=c(0, 1), linetype="solid", color="skyblue") +
	  geom_point(aes(y=TypeI), position=position_dodge(width=.7), size = 3, color="steelblue3", fill="skyblue") +	
		geom_point(aes(y=Power), position=position_dodge(width=.7), size = 1.5, color="black", fill="black") +	
	  coord_flip() +
	  facet_grid(tau.label~method) + 
	  scale_y_continuous(labels=scales::percent, limits=c(0, 1), breaks = c(.05, .5, .8, 1)) + 
	  scale_shape_manual(values=c(21, 22, 24)) + 
	  labs(shape="k", colour="Delta") +
	  ylab("% False positives / Statistical Power") +
	  xlab("Meta-analytic sample size (k)") +
	  ggtitle(title) + 
		theme_metashowdown
	return(PLOT)	
}


plotA <- buildFacet(RR.wide %>% filter(selProp==0), expression("(A) False positive error rates and statistical power at"~delta~" = 0.5, 0% publication bias"))
plotB <- buildFacet(RR.wide %>% filter(selProp==0.6), expression("(B) False positive error rates and statistical power at"~delta~" = 0.5, 60% publication bias"))
plotC <- buildFacet(RR.wide %>% filter(selProp==0.9), expression("(C) False positive error rates and statistical power at"~delta~" = 0.5, 90% publication bias"))


# ---------------------------------------------------------------------
# Build the legend

legOnlyPlot <- data.frame(qrp.label=factor(c("none", "med", "high"), ordered=TRUE), rate=c("False positive rate", "Power", "Power")) %>% 
  ggplot(aes(x=1, y=1, shape=qrp.label, color=rate, fill=rate)) + 
	geom_point()+
	theme(
  	panel.background = element_rect(fill="white"),
  	legend.position = c("bottom")
		) + 
  scale_shape_manual(values=c("none"=21,"med"=22,"high"=24),guide = guide_legend(title = "QRP Env.")) +
  scale_color_manual(values=c("False positive rate"="skyblue","Power"="black"), guide = guide_legend(title = "Rates")) +
  scale_fill_manual(values=c("False positive rate"="skyblue","Power"="black"), guide = guide_legend(title = "Rates"))

#Extract Legend 
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

legend <- g_legend(legOnlyPlot) 


# ---------------------------------------------------------------------
# Save PDF of plot

pdf("Plots/hyptest.pdf", width=15, height=22)
grid.arrange(plotA, plotB, plotC, legend, nrow=19, layout_matrix = cbind(c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4)))
dev.off()

