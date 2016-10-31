# plotting of results
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

#load("res.final.RData")
#res.hyp <- res.final %>% filter(term %in% c("b0", "evidence", "lack"), variable=="p.value", !method %in% c("PET.rma", "PEESE.rma", "PETPEESE.rma"))
#save(res.hyp, file="res.hyp.RData")
load(file="res.hyp.RData")

# ---------------------------------------------------------------------
# Summarize

# define critical p-value for each method
res.hyp$p.crit <- .05
res.hyp$p.crit[res.hyp$method=="pcurve"] <- .05

# compute rejection
res.hyp$H0.reject <- res.hyp$value < res.hyp$p.crit
	

# Compute summary measures across replications
hyp.summ <- res.hyp %>% group_by(condition, k, delta, qrpEnv, selProp, tau, method) %>% dplyr::summarise(
	H0.reject = sum(H0.reject, na.rm=TRUE)/sum(!is.na(H0.reject)),
	n.simulations = n()
)


# ---------------------------------------------------------------------
#  loop plot for H0

#  show partly loop style
library(gtools)

# new labels for meaningful order
hyp.summ$qrp.label <- factor(hyp.summ$qrpEnv, labels=c("QRP = 0 (none)", "QRP = 1 (med)", "QRP = 2 (high)"))

# order two variables into one loop
hyp.summ <- hyp.summ %>% 
  mutate(loop = paste0(tau, "_", qrp.label))

# order loop factor alphabetically
hyp.summ$loop <- factor(hyp.summ$loop, levels = mixedsort(unique(hyp.summ$loop)))

# H0
hyp.summ %>% 
  filter(selProp == 0.6, delta==0) %>% 
  ggplot(aes(x = loop, y = H0.reject, group = loop)) + 
  geom_point() + 
  facet_grid(k ~ method) + 
  geom_hline(aes(yintercept = 0.05)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size=6, hjust=1, vjust=.5)) +
  ggtitle("Under H0 - nominal Type I error? (for selProp = 60%)")

# H1: delta = 0.5
hyp.summ %>% 
filter(selProp == 0.6, delta==0.5) %>% 
ggplot(aes(x = loop, y = H0.reject, group = loop)) + 
geom_point() + 
facet_grid(k ~ method) + 
geom_hline(aes(yintercept = 0.80), linetype="dotted") + 
theme_bw() +
theme(axis.text.x = element_text(angle = 90, size=6, hjust=1, vjust=.5)) +
ggtitle("Under H1 - Power? (for selProp = 60%, delta=0.5)")


# ---------------------------------------------------------------------
# Compute rejection ratios

RR <- hyp.summ %>% ungroup() %>% 
	select(condition, k, delta, qrp.label, selProp, tau, loop, method, H0.reject)

RR$TypeI <- RR$H0.reject
RR$TypeI[RR$delta!=0] <- NA

RR$TypeII <- 1-RR$H0.reject
RR$TypeII[RR$delta==0] <- NA
RR$Power <- 1-RR$TypeII

# compare delta==0 against delta==0.5
H1 <- 0.5
RR.H0 <- RR %>% filter(delta == 0) %>% select(condition, k, qrp.label, selProp, tau, loop, method, TypeI)
RR.H1 <- RR %>% filter(delta == H1) %>% select(k, qrp.label, selProp, tau, loop, method, Power)

glimpse(RR.H0)
glimpse(RR.H1)

RR.wide <- inner_join(RR.H0, RR.H1)
glimpse(RR.wide)

RR.wide$rejectionRatio <- RR.wide$Power/RR.wide$TypeI

ggplot(RR.wide %>% filter(selProp == 0.6), aes(x=loop, y=log(rejectionRatio), group=loop)) + geom_point() + facet_grid(k ~ method) + theme_bw() + geom_hline(yintercept=log(16), linetype="dotted") + theme(axis.text.x = element_text(angle = 90, size=6, hjust=1, vjust=.5)) + coord_cartesian(ylim=c(0, 30))

RR.wide %>% filter(qrp.label=="QRP = med", selProp.label=="60% file-drawer", method=="puniform", tau==0)