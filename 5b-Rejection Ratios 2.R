# plotting of results
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

load("summ.RData")


# add selProp label column
summ$selProp.label = factor(summ$selProp, 
                            levels = c(0, .6, 1), 
                            labels = c("No file-drawer", "60% file-drawer", "All file-drawer"))
# Check accuracy
table(summ$selProp, summ$selProp.label)
  
head(summ, 20)


# ---------------------------------------------------------------------
# Compute rejection ratios

RR <- summ %>% ungroup() %>% 
	select(condition, k.label, delta, delta.label, qrp.label, selProp.label, tau, method, H0.reject)
	
RR$TypeI <- RR$H0.reject
RR$TypeI[RR$delta!=0] <- NA

RR$TypeII <- 1-RR$H0.reject
RR$TypeII[RR$delta==0] <- NA
RR$Power <- 1-RR$TypeII

# compare delta==0 against delta==0.5
H1 <- 0.2
RR.H0 <- RR %>% filter(delta == 0) %>% select(condition, k.label, qrp.label, selProp.label, tau, method, TypeI)
RR.H1 <- RR %>% filter(delta == H1) %>% select(k.label, qrp.label, selProp.label, tau, method, Power)

glimpse(RR.H0)
glimpse(RR.H1)

RR.wide <- inner_join(RR.H0, RR.H1)
glimpse(RR.wide)

RR.wide$rejectionRatio <- RR.wide$Power/RR.wide$TypeI

ggplot(RR.wide %>% filter(qrp.label=="QRP = med"), aes(x=k.label, y=log(rejectionRatio), color=method, group=method)) + geom_line(size=1) + facet_grid(tau~selProp.label) + theme_bw() + geom_hline(yintercept=log(16), linetype="dotted") + theme(axis.text.x = element_text(angle = 90)) + coord_cartesian(ylim=c(0, 5))

RR.wide %>% filter(qrp.label=="QRP = med", selProp.label=="60% file-drawer", method=="puniform", tau==0)