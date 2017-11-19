library(tidyr)
load(file="dataFiles/res.wide.red.RData")

# ---------------------------------------------------------------------
# Get correlation between estimators within and across all conditions

getCor <- function(x) {
	x2 <- x %>% select(id, method, b0_estimate) %>% spread(method, b0_estimate) %>% select(-id)
	x3 <- cor(x2, use="p") %>% as.data.frame()
	x3$method1 <- rownames(x3)
	x4 <- gather(x3, method1)
	colnames(x4) <- c("method1", "method2", "COR")
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

# get correlation of estimators across all conditions
C2 <- getCor(res.wide.red)
C2



# ---------------------------------------------------------------------
# Percentage of WAAP vs. WLS in the conditional estimator

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

table(estNeg$estNegPerc)

# --> all pcurve and puniform estimates are truncated at zero.


## ======================================================================
## ANOVA-style analysis: Which experimental factor explains variation in performance?
## (Not very illuminating)
## ======================================================================

load("dataFiles/summ.RData")

a1 <- aov(RMSE ~ k.label*delta.label*qrp.label*censor.label*tau.label, data=summ %>% filter(method=="pcurve"))
e1 <- data.frame(etaSquared(a1))
e1[order(e1$eta.sq, decreasing=TRUE), ]