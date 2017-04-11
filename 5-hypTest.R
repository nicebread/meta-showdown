#' ---
#' title: "Hypothesis test"
#' author: "Felix Schönbrodt"
#' output: pdf_document
#' classoption: landscape
#' ---


# plotting of results
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)


load(file="res.hyp.RData")

# ---------------------------------------------------------------------
# Compute summary measures across replications
hyp.summ <- res.hyp %>% group_by(condition, k, k.label, delta, delta.label, qrpEnv, qrp.label, selProp, selProp.label, tau, tau.label, method) %>% dplyr::summarise(
	H0.reject = sum(H0.reject, na.rm=TRUE)/sum(!is.na(H0.reject)),
	n.simulations = n()
)

# ---------------------------------------------------------------------
# Compute rejection ratios

RR <- hyp.summ %>% ungroup() %>% 
	select(condition, k, delta, qrp.label, selProp, tau, method, H0.reject) %>% 
	filter(
	  !method %in% c("pcurve.hack", "topN.fixed", "PET.rma", "PEESE.rma", "PETPEESE.rma")
	)
	

RR$TypeI <- RR$H0.reject
RR$TypeI[RR$delta!=0] <- NA

RR$TypeII <- 1-RR$H0.reject
RR$TypeII[RR$delta==0] <- NA
RR$Power <- 1-RR$TypeII

# compare delta==0 against delta==0.5
H1 <- 0.5
RR.H0 <- RR %>% filter(delta == 0) %>% select(condition, k, qrp.label, selProp, tau, method, TypeI)
RR.H1 <- RR %>% filter(delta == H1) %>% select(k, qrp.label, selProp, tau, method, Power)

RR.wide <- inner_join(RR.H0, RR.H1)

RR.wide$rejectionRatio <- RR.wide$Power/RR.wide$TypeI
RR.wide$errorSum <- (1-RR.wide$Power) + RR.wide$TypeI

# res.wide$delta.label <- factor(res.wide$delta, levels=unique(res.wide$delta), labels=paste0("delta = ", unique(res.wide$delta)))
# res.wide$k.label <- factor(res.wide$k, levels=sort(unique(res.wide$k)), labels=paste0("k = ", sort(unique(res.wide$k))))
# res.wide$qrp.label <- factor(res.wide$qrpEnv, levels=unique(res.wide$qrpEnv), labels=paste0("QRP = ", unique(res.wide$qrpEnv)))
# res.wide$selProp.label <- factor(res.wide$selProp, levels=unique(res.wide$selProp), labels=paste0("selProp = ", unique(res.wide$selProp)))

RR.wide$tau.label <- factor(RR.wide$tau, levels=unique(RR.wide$tau), labels=paste0("tau = ", unique(RR.wide$tau)))


theme_metashowdown <- theme(
  panel.spacing =unit(.05, "lines"),
  panel.background = element_rect(fill="white"),
  panel.border = element_rect(color="grey90",fill=NA, size = 1), #element_blank(),
  panel.grid.minor= element_blank(),
  panel.grid.major= element_blank(),#element_line(color="grey90"),
  strip.background = element_rect(colour="white", fill="white"), #"grey93"
  axis.ticks = element_line(color="lightgrey"),
  legend.position = "none"#c("bottom")
)

RR.wide %>% filter(selProp==0) %>%
  ggplot(aes(x=factor(k), shape=qrp.label)) + 
  geom_hline(yintercept=c(.05, .80), linetype="dotted", color="grey60") +
  geom_point(aes(y=TypeI), position=position_dodge(width=.7), size = 0.4, color=1, fill=1) +	
	geom_point(aes(y=Power), position=position_dodge(width=.7), size = 0.4, color=2, fill=2) +	
  coord_flip() +
  facet_grid(tau.label~method) + 
  scale_y_continuous(limits=c(0, 1), breaks = c(.05, .5, .8)) + 
  scale_shape_manual(values=c(21, 22, 24)) + 
  scale_color_manual(values=c("0"="grey60","0.5"="black")) +
  scale_fill_manual(values=c("0"="grey60","0.5"="black")) +
  labs(shape="k", colour="Delta") +
  ylab("Error rates") +
  xlab("Meta-analytic sample size (k)") +
  ggtitle("(A) False positive and false negative error rates, 0% publication bias") + 
	theme_metashowdown

