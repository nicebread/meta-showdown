# plotting of results
library(ggplot2)
library(dplyr)
library(tidyr)

load("summ.RData")

# drop method == "fill"
summ = summ %>% 
  filter(method != "fill")

# fix k.label
summ$k.label = factor(summ$k, 
                      levels = c(10, 30, 60, 100), 
                      labels = c("k = 10", "k = 30", "k = 60", "k = 100"))
# check accuracy
table(summ$k, summ$k.label)

# add selProp label column
summ$selProp.label = factor(summ$selProp, 
                            levels = c(0, .6, 1), 
                            labels = c("No file-drawer", "60% file-drawer", "All file-drawer"))
# Check accuracy
table(summ$selProp, summ$selProp.label)
  
glimpse(summ)

# ---------------------------------------------------------------------
# Compute rejeciton ratios

RR <- summ %>% ungroup() %>% 
	select(condition, k.label, delta, delta.label, qrp.label, selProp.label, tau, method, consisZero) %>% 
	filter(!method %in% c("Tau", "fill", "FAT"))
	
RR$TypeI <- 1-RR$consisZero
RR$TypeI[RR$delta!=0] <- NA

RR$TypeII <- RR$consisZero
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

ggplot(RR.wide %>% filter(qrp.label=="QRP = med"), aes(x=k.label, y=rejectionRatio, color=method, group=method)) + geom_line(size=2) + facet_grid(tau~selProp.label) + theme_bw() + geom_hline(yintercept=16) + theme(axis.text.x = element_text(angle = 90))

RR.wide %>% filter(qrp.label=="QRP = med", selProp.label=="No file-drawer", method=="RE", tau==0)