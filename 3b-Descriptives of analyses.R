library(tidyr)
library(dplyr)
load(file="dataFiles/res.wide.red.RData")

# ---------------------------------------------------------------------
# Get correlation between estimators within and across all conditions

getCor <- function(x) {
	x2 <- x %>% select(id, method, b0_estimate) %>% spread(method, b0_estimate) %>% select(-id)
	x3 <- cor(x2, use="p", method="spearman") %>% as.data.frame()
	x3$method1 <- rownames(x3)
	x4 <- gather(x3, method2, COR, -method1)
	return(x4)
}

# get correlation of estimators within all conditions
res <- data.frame()
for (cond in 1:432) {
	print(cond)
	x <- res.wide.red %>% filter(condition==cond)
	C <- getCor(x)
	res <- rbind(res, data.frame(
		x[1, 1:8],
		C
	))
}

res2 <- res %>% filter(method1=="pcurve", method2=="puniform")
summary(res2$COR)

x2 <- res.wide.red %>% filter(condition==84) %>% select(id, method, b0_estimate) %>% spread(method, b0_estimate) %>% select(-id)
plot(x2$pcurve, x2$puniform)

# get correlation of estimators across all conditions
C2 <- getCor(res.wide.red)
C2

C2 %>% filter(method1=="pcurve", method2=="puniform")


## ======================================================================
## Percentage of WAAP vs. WLS in the conditional estimator
## ======================================================================

estimator_types_desc <- res.wide.red %>% 
	filter(method %in% c("WAAP-WLS", "PETPEESE.lm")) %>% 
	select(id, condition, k, k.label, delta, delta.label, qrpEnv, qrp.label, censor, censor.label, tau, tau.label, estimator_type) %>% 
	group_by(condition, k, k.label, delta, delta.label, qrpEnv, qrp.label, censor, censor.label, tau, tau.label) %>% 
	summarise(
		WAAP_perc = sum(estimator_type == "WAAP") / sum(estimator_type %in% c("WAAP", "WLS")),
		WLS_perc = sum(estimator_type == "WLS") / sum(estimator_type %in% c("WAAP", "WLS")),
		PET_perc = sum(estimator_type == "PET") / sum(estimator_type %in% c("PET", "PEESE")),
		PEESE_perc = sum(estimator_type == "PEESE") / sum(estimator_type %in% c("PET", "PEESE"))
	)

print(estimator_types_desc, n=432)

print(estimator_types_desc %>% arrange(WAAP_perc), n=432)

mean(estimator_types_desc$WAAP_perc)
mean(estimator_types_desc$WLS_perc)

# plot WAAP-WLS percentages as small multiples

# reshape to long format
WAAP_desc.long <- melt(estimator_types_desc %>% select(-PET_perc, -PEESE_perc), 
	id.vars=c("condition", "k", "k.label", "delta", "delta.label", "qrpEnv", "qrp.label", "censor", "censor.label", "tau", "tau.label"),
	variable.name = "estimator_type", value.name = "percentage")
	
library(ggplot2)	
ggplot(WAAP_desc.long, aes(x=k.label, y=percentage, fill=estimator_type)) + geom_bar(stat="identity") + facet_grid(qrp.label~censor.label~tau.label~delta.label)


# zoom into the mixed delta=0.2 conditions:

ggplot(WAAP_desc.long %>% filter(delta == 0.2), aes(x=k.label, y=percentage, fill=estimator_type)) + geom_bar(stat="identity") + facet_grid(tau.label~qrp.label~censor.label)




## ======================================================================
## Percentage of p-uniform return codes
# comment codes:
# 0 = regular converged estimate
# 1 = "No significant studies on the specified side"
# 2 = "set to zero if avg. p-value > .025"
## ======================================================================

puniform_comments <- res.wide.red %>% 
	filter(method %in% c("puniform")) %>% 
	select(id, condition, k, k.label, delta, delta.label, qrpEnv, qrp.label, censor, censor.label, tau, tau.label, b0_estimate, b0_comment) %>% 
	group_by(condition, k, k.label, delta, delta.label, qrpEnv, qrp.label, censor, censor.label, tau, tau.label) %>% 
	summarise(
		regular_perc = sum(b0_comment == 0) / n(),
		noSig_perc = sum(b0_comment == 1) / n(),
		barelySig_perc = sum(b0_comment == 2) / n()
	)

print(puniform_comments, n=432)

# table collapsed across all conditions:
res.wide.red %>% filter(method %in% c("puniform")) %>% count(b0_comment) %>% select("n")/432000*100

# table per delta (i.e., at which deltas did the special case occur?):
# --> mostly at delta==0
res.wide.red %>% filter(method %in% c("puniform")) %>% group_by(delta) %>% count(b0_comment) %>% mutate(perc=n/108000)

# same for p-curve: How often does it return exactly 0? (Well, the optimizer converges on somethin like 0.0001, not exactly 0)
pcurve <- res.wide.red %>% filter(method %in% c("pcurve"))
sum(pcurve$b0_estimate < 0.0001, na.rm=TRUE)/432000

## ======================================================================
## How many estimates converge?
## ======================================================================

load("dataFiles/summ.RData")
# reduced set for revision
	summ2 <- summ %>% filter(method %in% c("reMA", "TF", "PETPEESE", "pcurve", "puniform", "3PSM", "WAAP-WLS")) %>% 
	  mutate(method = factor(method, levels=c("reMA", "TF", "WAAP-WLS", "pcurve", "puniform", "PETPEESE", "3PSM"), labels=c("RE", "TF", "WAAP-WLS", "p-curve", "p-uniform", "PET-PEESE", "3PSM"), ordered=TRUE))

# which method has less than 100% convergence? --> RE, WAAP-WLS and PET-PEESE always converge
convRate0 <- summ2 %>% 
	ungroup() %>% 
	select(k, delta, qrpEnv, censor, tau, method, n.validEstimates) %>% 
	spread(method, n.validEstimates)

# overall convergence rate (across all conditions)	
convRate0 %>% 
	select(RE, TF, 'WAAP-WLS', 'p-curve', 'p-uniform', 'PET-PEESE', '3PSM') %>% 
	summarise_all(mean, na.rm=TRUE)

# When does a method fail?	

## TF
convRate0 %>% filter(TF < 800) %>% print(n=100)

## p-curve
convRate0 %>% filter(`p-curve` < 900) %>% print(n=100)

## p-uniform
convRate0 %>% filter(`p-uniform` < 500) %>% print(n=100)
	
## 3PSM
convRate0 %>% filter(`3PSM` < 500) %>% print(n=100)	
	
# prepare the full table
convRate <- summ2 %>% ungroup() %>% 
	filter(!method %in% c("RE", "WAAP-WLS", "PET-PEESE")) %>% 							# remove methods with 100% convergence
	select(k, delta, qrpEnv, censor, tau, method, n.validEstimates) %>% 
	mutate(convRate = paste0(round(n.validEstimates/10), "%")) %>% 
	select(-n.validEstimates) %>% 
	arrange(k, delta, qrpEnv, censor, tau, method)

convRate.wide <- spread(convRate, method, convRate)
colnames(convRate.wide)[1:5] <- c("{k}", "{$\\delta$}", "{QRP}", "{PB}", "{$\\tau$}")

print(convRate.wide, n=432)

# print full table
library(xtable)
x1 <- xtable(convRate.wide, auto=TRUE, label="tab:convRate", caption="QRP = QRP Environment, PB = Publication bias")
print(x1, file="tex-exports/tab-convRate.tex", include.rownames=FALSE, sanitize.colnames.function=identity, tabular.environment="longtable", floating = FALSE)


## ======================================================================
## Average I^2 estimates in conditions without publication bias and without QRPs:
## i.e., what are the I^2 values that correspond to our chosen tau values?
## ======================================================================

I2 <- res.wide.red %>% 
	filter(method == "reMA", censor == "none", qrpEnv=="none", !is.na(I2_estimate)) %>% 
	select(k, k.label, delta, delta.label, tau, tau.label, I2_estimate) %>% 
	group_by(k, k.label, delta, delta.label, tau, tau.label) %>% 
	summarise(
		I2.mean = mean(I2_estimate, na.rm=TRUE),
		I2.SD = sd(I2_estimate, na.rm=TRUE)
	) %>% arrange(tau, delta, k)

print(I2, n=100)

# stronger aggregation: aggregate over k
I2b <- res.wide.red %>% 
	filter(method == "reMA", censor == "none", qrpEnv=="none", !is.na(I2_estimate)) %>% 
	select(delta, delta.label, tau, tau.label, I2_estimate) %>% 
	group_by(delta, delta.label, tau, tau.label) %>% 
	summarise(
		I2.mean = mean(I2_estimate, na.rm=TRUE),
		I2.SD = sd(I2_estimate, na.rm=TRUE)
	) %>% arrange(tau, delta)
	
I2b
	
# even stronger aggregation: aggregate over k and delta
I2c <- res.wide.red %>% 
	filter(method == "reMA", censor == "none", qrpEnv=="none", !is.na(I2_estimate)) %>% 
	select(tau, tau.label, I2_estimate) %>% 
	group_by(tau, tau.label) %>% 
	summarise(
		I2.mean = mean(I2_estimate, na.rm=TRUE),
		I2.SD = sd(I2_estimate, na.rm=TRUE)
	) %>% arrange(tau)	

I2c	


## ======================================================================
## How many puniform and pcurve results are negative?
## ======================================================================

estNeg <- res.wide.red %>% 
	filter(method %in% c("puniform", "pcurve"), !is.na(b0_estimate)) %>% 
	#select(method, k.label, delta.label, tau.label, censor.label, b0_estimate) %>% 
	group_by(method, k.label, delta.label, tau.label, censor.label) %>% 
	summarise(
		estNegPerc = sum(b0_estimate < 0)/n()
	) %>% arrange(tau.label, delta.label, k.label)

print(estNeg, n=300)

# --> virtually all pcurve and puniform estimates are truncated at zero.


## ======================================================================
## ANOVA-style analysis: Which experimental factor explains variation in performance?
## (Not very illuminating)
## ======================================================================

load("dataFiles/summ.RData")

library(lsr)
a1 <- aov(RMSE ~ k.label*delta.label*qrp.label*censor.label*tau.label, data=summ %>% filter(method=="pcurve"))
e1 <- data.frame(etaSquared(a1))
e1[order(e1$eta.sq, decreasing=TRUE), ]


## ======================================================================
## Do TF and PET always/mostly disagree in the case of H0 + publication bias?
## ======================================================================

H0.PB <- res.wide.red %>% 
	filter(method %in% c("TF", "PET"), !is.na(b0_estimate), delta==0, qrpEnv=="none", censor=="high")
	
	 %>% 
	#select(method, k.label, delta.label, tau.label, censor.label, b0_estimate) %>% 
	group_by(method, k.label, delta.label, tau.label, censor.label) %>% 
	summarise(
		estNegPerc = sum(b0_estimate < 0)/n()
	) %>% arrange(tau.label, delta.label, k.label)

ggplot(H0.PB, aes(x=b0_estimate, color=method)) + geom_density()