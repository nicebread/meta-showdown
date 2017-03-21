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

# load("res.wide.red.RData")
# res.hyp <- res.wide.red %>% select(1:8, b0_estimate, b0_p.value, skewtest_p.value, 41:45) %>% filter(!method %in% c("pcurve", "pcurve.lack"))
#
# # define critical p-value for each method
# res.hyp$p.crit <- .05
#
# # merge two p-value columns into one
# res.hyp$p.value <- ifelse(!is.na(res.hyp$b0_p.value), res.hyp$b0_p.value, res.hyp$skewtest_p.value)
# res.hyp <- res.hyp %>% select(-b0_p.value, -skewtest_p.value)
#
# # compute rejection:
# # Reject H0 if test is significant AND estimate in correct direction.
# # In case of p-curve skewness tests, there is no estimate; estimate is set to NA there.
# res.hyp$H0.reject <- (res.hyp$p.value < res.hyp$p.crit) & (is.na(res.hyp$b0_estimate) | res.hyp$b0_estimate > 0)
#
# # Add combined hypothesis test: PETPEESE + 3PSM
# PP3 <- res.hyp %>% filter(method %in% c("PETPEESE.lm", "3PSM")) %>% group_by(id, condition, k, delta, qrpEnv, selProp, tau, delta.label, k.label, qrp.label, selProp.label, tau.label, p.crit) %>% summarise(
# 	H0.reject = H0.reject[1] & H0.reject[2] & !is.na(H0.reject[1]) & !is.na(H0.reject[2]),
# 	method="PP3",
# 	p.value = NA
# )
#
# res.hyp <- bind_rows(res.hyp, PP3)
#
# save(res.hyp, file="res.hyp.RData", compress="gzip")
load(file="res.hyp.RData")

# ---------------------------------------------------------------------
# Compute summary measures across replications
hyp.summ <- res.hyp %>% group_by(condition, k, k.label, delta, delta.label, qrpEnv, qrp.label, selProp, selProp.label, tau, tau.label, method) %>% dplyr::summarise(
	H0.reject = sum(H0.reject, na.rm=TRUE)/sum(!is.na(H0.reject)),
	n.simulations = n()
)


# ---------------------------------------------------------------------
#  loop plot for H0

#  show partly loop style
library(gtools)

# new labels for meaningful order
#hyp.summ$qrp.label <- factor(hyp.summ$qrpEnv, labels=c("QRP = 0 (none)", "QRP = 1 (med)", "QRP = 2 (high)"))

# order two variables into one loop
hyp.summ <- hyp.summ %>%  mutate(
	loop = paste0("τ = ", tau, "; k = ", k),
	loop2 = paste0(selProp.label, "; ", qrp.label)
)

# order loop factor alphabetically
hyp.summ$loop <- factor(hyp.summ$loop, levels = mixedsort(unique(hyp.summ$loop)))
hyp.summ$loop2 <- factor(hyp.summ$loop2, levels = mixedsort(unique(hyp.summ$loop2)))

# H0
#+ echo=FALSE, fig.width=11, fig.height=8
hyp.summ %>% 
  filter(selProp == 0.6, delta==0) %>% 
  ggplot(aes(x = loop, y = H0.reject, group = loop)) + 
  geom_point() + 
  facet_grid(qrp.label ~ method) + 
  geom_hline(aes(yintercept = 0.05)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size=6, hjust=1, vjust=.5)) +
  ggtitle("Under H0 - nominal Type I error? (for selProp = 60%)")
  
hyp.summ %>% 
	filter(selProp == 0.90, delta==0) %>% 
	ggplot(aes(x = loop, y = H0.reject, group = loop)) + 
	geom_point() + 
	facet_grid(qrp.label ~ method) + 
	geom_hline(aes(yintercept = 0.05)) + 
	theme_bw() +
	theme(axis.text.x = element_text(angle = 90, size=6, hjust=1, vjust=.5)) +
	ggtitle("Under H0 - nominal Type I error? (for selProp = 90%)")
	 

# H1: delta = 0.5
hyp.summ %>% 
filter(selProp == 0.6, delta==0.5) %>% 
ggplot(aes(x = loop, y = H0.reject, group = loop)) + 
geom_point() + 
facet_grid(qrp.label ~ method) + 
geom_hline(aes(yintercept = 0.80), linetype="dotted") + 
theme_bw() +
theme(axis.text.x = element_text(angle = 90, size=6, hjust=1, vjust=.5)) +
ggtitle("Under H1 - Power? (for selProp = 60%, delta=0.5)")

hyp.summ %>% 
filter(selProp == 0.90, delta==0.5) %>% 
ggplot(aes(x = loop, y = H0.reject, group = loop)) + 
geom_point() + 
facet_grid(qrp.label ~ method) + 
geom_hline(aes(yintercept = 0.80), linetype="dotted") + 
theme_bw() +
theme(axis.text.x = element_text(angle = 90, size=6, hjust=1, vjust=.5)) +
ggtitle("Under H1 - Power? (for selProp = 90%, delta=0.5)")

# ---------------------------------------------------------------------
# Compute rejection ratios

RR <- hyp.summ %>% ungroup() %>% 
	select(condition, k, delta, qrp.label, selProp, tau, loop, loop2, method, H0.reject)

RR$TypeI <- RR$H0.reject
RR$TypeI[RR$delta!=0] <- NA

RR$TypeII <- 1-RR$H0.reject
RR$TypeII[RR$delta==0] <- NA
RR$Power <- 1-RR$TypeII

# compare delta==0 against delta==0.2
H1 <- 0.2
RR.H0 <- RR %>% filter(delta == 0) %>% select(condition, k, qrp.label, selProp, tau, loop, loop2, method, TypeI)
RR.H1 <- RR %>% filter(delta == H1) %>% select(k, qrp.label, selProp, tau, loop, loop2, method, Power)

glimpse(RR.H0)
glimpse(RR.H1)

RR.wide <- inner_join(RR.H0, RR.H1)
glimpse(RR.wide)

RR.wide$rejectionRatio <- RR.wide$Power/RR.wide$TypeI
RR.wide$errorSum <- (1-RR.wide$Power) + RR.wide$TypeI

#+ echo=FALSE, fig.width=11, fig.height=8, warning=FALSE
ggplot(RR.wide %>% filter(selProp == 0.6), aes(x=loop, y=log(rejectionRatio), group=loop)) + 
	geom_point() + 
	facet_grid(qrp.label ~ method) + theme_bw() + geom_hline(yintercept=log(16), linetype="dotted") + 
	theme(axis.text.x = element_text(angle = 90, size=6, hjust=1, vjust=.5)) + coord_cartesian(ylim=c(0, 10)) +
	ggtitle("Rejection Ratio (for selProp = 60%, H0 against delta=0.5)")

ggplot(RR.wide %>% filter(selProp == 0.90), aes(x=loop, y=log(rejectionRatio), group=loop)) + 
	geom_point() + 
	facet_grid(qrp.label ~ method) + theme_bw() + geom_hline(yintercept=log(16), linetype="dotted") + 
	theme(axis.text.x = element_text(angle = 90, size=6, hjust=1, vjust=.5)) + coord_cartesian(ylim=c(0, 10)) +
	ggtitle("Rejection Ratio (for selProp = 90%, H0 against delta=0.5)")
	
	
# # combined plot: RR and sum or errors (RR can be on several levels)
# ggplot(RR.wide %>% filter(selProp == 0.6), aes(y=Power, x=log(rejectionRatio), color=loop)) +
# 	geom_point() + geom_vline(xintercept=log(16), linetype="dotted") + facet_grid(qrp.label ~ method)
#
# RR.wide %>% filter(method=="3PSM", qrp.label == "QRP = high", selProp==.6)
#
# 		facet_grid(qrp.label ~ method) + theme_bw() + geom_hline(yintercept=log(16), linetype="dotted") +
# 	theme(axis.text.x = element_text(angle = 90, size=6, hjust=1, vjust=.5)) + coord_cartesian(ylim=c(0, 10)) +
# 	ggtitle("Rejection Ratio (for selProp = 60%, H0 against delta=0.5)")
	
# ---------------------------------------------------------------------
# Rate of significant results in the wrong direction

# wrongSig <- res.wide.red %>% select(1:8, b0_estimate, b0_p.value) %>%
# 	filter(!method %in% c("pcurve.evidence", "pcurve.lack"), delta < 0.5, !is.na(b0_estimate), !is.na(b0_p.value)) %>%
# 	group_by(condition, k, delta, qrpEnv, selProp, tau, method) %>%
# 	summarise(
# 		wrongSig = sum((b0_estimate < 0) & (b0_p.value < .05)) / n()
# 	) %>%
# 	mutate(loop = paste0(k, "_", tau))
#
# # order loop factor alphabetically
# wrongSig$loop <- factor(wrongSig$loop, levels = mixedsort(unique(wrongSig$loop)))
#
# #' #Percentage of significant estimates in the *wrong* direction
# #+ echo=FALSE, fig.width=11, fig.height=8, warning=FALSE
# ggplot(wrongSig, aes(x=loop, y=wrongSig, color=factor(delta))) + geom_point() + facet_grid(selProp ~ qrpEnv ~ method) + theme(axis.text.x = element_text(angle = 90, size=6, hjust=1, vjust=.5)) + ylab("Percentage of significant estimates in wrong direction")
#


# ---------------------------------------------------------------------
# Another try: AUC

# library(ROCR)
#
# sel <- res.hyp %>% filter(method=="pcurve.evidence", delta %in% c(0, 0.5), tau==0.2, qrpEnv=="med", selProp==0.9, k==10)
# pred <- prediction(predictions=sel$p.value, labels=factor(sel$delta, levels=c(0.5, 0), ordered=TRUE))
#
# roc.perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# plot(roc.perf)
# abline(a=0, b= 1)
#
# auc <- performance(pred, measure = "auc")
# auc@y.values
#
# library(pROC)
# roc(factor(sel$delta, levels=c(0.5, 0), ordered=TRUE), sel$p.value)
#

# ---------------------------------------------------------------------
# Another try: PPV of a significant result

#head(RR.wide)

# assume p(H1) = 0.3

pH1 <- 0.3
R <- pH1/(1-pH1)

# PPV and NPV
RR.wide$PPV <- (RR.wide$Power*R) / (R - (1-RR.wide$Power)*R + RR.wide$TypeI)
RR.wide$NPV <- (1-RR.wide$TypeI)*R / (1 - RR.wide$TypeI + (1-RR.wide$Power)*R) 

#+ echo=FALSE, fig.width=11, fig.height=8, warning=FALSE
ggplot(RR.wide %>% filter(selProp == 0), aes(x=loop, y=PPV, group=loop), shape=1) + 
	geom_point() + geom_point(aes(y=NPV), shape=1) + facet_grid(qrp.label ~ method) + theme_bw() + coord_cartesian(ylim=c(0, 100)) +
	theme(axis.text.x = element_text(angle = 90, size=6, hjust=1, vjust=.5)) +
	ggtitle("Positive Predictive Value (for selProp = 0%, H0 against delta=0.5, a priori p(H1) = 0.3)")

#+ echo=FALSE, fig.width=11, fig.height=8, warning=FALSE
ggplot(RR.wide %>% filter(selProp == 0.6), aes(x=loop, y=PPV, group=loop), shape=1) + 
	geom_point() + geom_point(aes(y=NPV), shape=1) + facet_grid(qrp.label ~ method) + theme_bw() + coord_cartesian(ylim=c(0, 100)) +
	theme(axis.text.x = element_text(angle = 90, size=6, hjust=1, vjust=.5)) +
	ggtitle("Positive Predictive Value (for selProp = 60%, H0 against delta=0.5, a priori p(H1) = 0.3)")
	
#+ echo=FALSE, fig.width=11, fig.height=8, warning=FALSE
ggplot(RR.wide %>% filter(selProp == 0.9), aes(x=loop, y=PPV, group=loop), shape=1) + 
	geom_point() + geom_point(aes(y=NPV), shape=1) + facet_grid(qrp.label ~ method) + theme_bw() + coord_cartesian(ylim=c(0, 100)) +
	theme(axis.text.x = element_text(angle = 90, size=6, hjust=1, vjust=.5)) +
	ggtitle("Positive Predictive Value (for selProp = 90%, H0 against delta=0.5, a priori p(H1) = 0.3)")

# Try: all in one plot
#+ echo=FALSE, fig.width=11, fig.height=8, warning=FALSE
ggplot(RR.wide %>% filter(!method %in% c("PP3", "topN.fixed")), aes(x=loop, y=PPV, group=loop), shape=1) + 
	geom_point() + geom_point(aes(y=NPV), shape=1) + facet_grid(selProp+qrp.label ~ method) + theme_bw() + coord_cartesian(ylim=c(0, 1)) +
	theme(
		axis.text.x = element_text(angle = 90, size=8, hjust=1, vjust=.5, color="black"),
		axis.text.y = element_text(size=6, hjust=1, vjust=.5, color="black")
	) + coord_flip() + scale_y_continuous(labels = scales::percent) +
	ggtitle("Positive Predictive Value (H0 against delta=0.2, a priori p(H1) = 0.3)") + xlab("Heterogeneity (tau) and # of studies") + ylab("PPV (filled circles) and NPV (empty circles)")

# ---------------------------------------------------------------------
# Finally, straightforward: Type I and Power

#head(RR.wide)
#+ echo=FALSE, fig.width=11, fig.height=8, warning=FALSE
ggplot(RR.wide %>% filter(selProp == 0), aes(x=loop, y=Power, group=loop), shape=1) + 
	geom_point() + geom_point(aes(y=TypeI), shape=1) + facet_grid(qrp.label ~ method) + theme_bw() + coord_cartesian(ylim=c(0, 1)) +
	theme(axis.text.x = element_text(angle = 90, size=6, hjust=1, vjust=.5)) +  geom_hline(yintercept=c(.05, .8), linetype="dotted") +
	ggtitle("Error rates (for selProp = 0%, H0 against delta=0.5")

#+ echo=FALSE, fig.width=11, fig.height=8, warning=FALSE
ggplot(RR.wide %>% filter(selProp == 0.6), aes(x=loop, y=Power, group=loop), shape=1) + 
	geom_point() + geom_point(aes(y=TypeI), shape=1) + facet_grid(qrp.label ~ method) + theme_bw() + coord_cartesian(ylim=c(0, 1)) +
	theme(axis.text.x = element_text(angle = 90, size=6, hjust=1, vjust=.5)) +  geom_hline(yintercept=c(.05, .8), linetype="dotted") +
	ggtitle("Error rates (for selProp = 60%, H0 against delta=0.5")

#+ echo=FALSE, fig.width=11, fig.height=8, warning=FALSE
ggplot(RR.wide %>% filter(selProp == 0.9), aes(x=loop, y=Power, group=loop), shape=1) + 
	geom_point() + geom_point(aes(y=TypeI), shape=1) + facet_grid(qrp.label ~ method) + theme_bw() + coord_cartesian(ylim=c(0, 1)) +
	theme(axis.text.x = element_text(angle = 90, size=6, hjust=1, vjust=.5)) +  geom_hline(yintercept=c(.05, .8), linetype="dotted") +
	ggtitle("Error rates (for selProp = 90%, H0 against delta=0.5")


	# Try: all in one plot
	#+ echo=FALSE, fig.width=11, fig.height=8, warning=FALSE
	ggplot(RR.wide %>% filter(!method %in% c("PP3", "topN.fixed")), aes(x=loop, y=Power, group=loop), shape=1) + 
		geom_point() + geom_point(aes(y=TypeI), shape=1) + facet_grid(selProp+qrp.label ~ method) + theme_bw() + coord_cartesian(ylim=c(0, 1)) +
		theme(
			axis.text.x = element_text(angle = 90, size=8, hjust=1, vjust=.5, color="black"),
			axis.text.y = element_text(size=6, hjust=1, vjust=.5, color="black")
		) + coord_flip() + scale_y_continuous(labels = scales::percent) +  geom_hline(yintercept=c(.05, .8), linetype="dotted") + 
		ggtitle("Type I error rates and Power (H0 against delta=0.2)") + xlab("Heterogeneity (tau) and # of studies") + ylab("Power (filled circles) and Type I error (empty circles)")


		# LOOP2
		# Try: all in one plot
		#+ echo=FALSE, fig.width=11, fig.height=8, warning=FALSE
		ggplot(RR.wide %>% filter(!method %in% c("PP3", "topN.fixed")), aes(x=loop2, y=Power, group=loop2), shape=1) + 
			geom_point() + geom_point(aes(y=TypeI), shape=1) + facet_grid(k+tau ~ method) + theme_bw() + coord_cartesian(ylim=c(0, 1)) +
			theme(
				axis.text.x = element_text(angle = 90, size=8, hjust=1, vjust=.5, color="black"),
				axis.text.y = element_text(size=6, hjust=1, vjust=.5, color="black")
			) + coord_flip() + scale_y_continuous(labels = scales::percent) +  geom_hline(yintercept=c(.05, .8), linetype="dotted") + 
			ggtitle("Type I error rates and Power (H0 against delta=0.2)") + xlab("Heterogeneity (tau) and # of studies") + ylab("Power (filled circles) and Type I error (empty circles)")
