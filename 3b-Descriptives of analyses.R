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
