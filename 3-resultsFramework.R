## ======================================================================
## This file loads the results of the meta-analyses (which were generated in 2-analysisFramework.R)
## and computes summaries of them (such as mean error, MSE, coverage, etc.)
## ======================================================================

source("start.R")

# load the results files which were generated in 2-analysisFramework.R,
# combine them into one large data frame
analysisFiles <- list.files("analysisParts", pattern=".*\\.RData", full.names=TRUE)

print(paste0("Collecting results from ", length(analysisFiles), " analysis files."))

# loop through all files
res_list <- list()
for (f in analysisFiles) {
	print(f)
	load(f)	# the simulation data frame always is called "res"
	#res$id <- paste0(f, "_", res$id)
	res_list[[f]] <- res
}
res.final <- bind_rows(res_list)
str(res.final)

# final data set in long format:
save(res.final, file="res.final.RData")



# Show conditions
tab <- res.final %>% group_by(k, delta, qrpEnv, selProp, tau) %>% summarise(n.MA=length(unique(id)))
print(tab, n=50)

## reduce to relevant variables, drop unused factor levels
#res2 <- res.final %>% filter(variable != "tauEst", method!="FAT") %>% droplevels()
res2 <- res.final

# reshape long format to wide format
res.wide <- dcast(res2, id + condition + k + delta + qrpEnv + selProp + tau + method ~ term + variable, value.var="value")
head(res.wide)

# define some meaningful labels for the plots
res.wide$delta.label <- factor(res.wide$delta, levels=unique(res.wide$delta), labels=paste0("delta = ", unique(res.wide$delta)))
res.wide$k.label <- factor(res.wide$k, levels=sort(unique(res.wide$k)), labels=paste0("k = ", sort(unique(res.wide$k))))
res.wide$qrp.label <- factor(res.wide$qrpEnv, levels=unique(res.wide$qrpEnv), labels=paste0("QRP = ", unique(res.wide$qrpEnv)))
res.wide$selProp.label <- factor(res.wide$selProp, levels=unique(res.wide$selProp), labels=paste0("selProp = ", unique(res.wide$selProp)))
res.wide$tau.label <- factor(res.wide$tau, levels=unique(res.wide$tau), labels=paste0("tau = ", unique(res.wide$tau)))

# sanity check
tab <- res.wide %>% group_by(k, delta, qrpEnv, selProp, tau) %>% select(id) %>% dplyr::summarise(n.MA=length(unique(id)))
print(tab, n=50)

save(res.wide, file="res.wide.RData", compress="gzip")
#load(file="res.wide.RData")



# Compute summary measures across replications
summ <- res.wide %>% group_by(condition, k, k.label, delta, delta.label, qrpEnv, qrp.label, selProp, selProp.label, tau, tau.label, method) %>% dplyr::summarise(
	meanEst		= mean(b0_estimate, na.rm=TRUE),
	#meanD		= mean(D.mean, na.rm=TRUE),
	#meanD.sig	= mean(D.mean.sig, na.rm=TRUE),
	
	# relative to true population mean ("delta")
	ME 			= mean(b0_estimate - delta, na.rm=TRUE),
	RMSE		= sqrt(mean((b0_estimate - delta)^2, na.rm=TRUE)),
	MAD			= mean(abs(b0_estimate - delta), na.rm=TRUE), # mean absolute deviation
	perc2.5		= quantile(b0_estimate, probs=.025, na.rm=TRUE),
	perc97.5	= quantile(b0_estimate, probs=.975, na.rm=TRUE),
	coverage 	= sum(delta > b0_conf.low & delta < b0_conf.high)/sum(!is.na(b0_conf.high)),
	consisZero  = sum(0 > b0_conf.low & 0 < b0_conf.high)/n()
	
	# # relative to mean of entered ES ("D")
	# ME.D		= mean(d - meanD, na.rm=TRUE),
	# RMSE.D		= sqrt(mean((d - meanD)^2, na.rm=TRUE)),
	#
	# # relative to mean of significant entered ES ("D.sig")
	# ME.D.sig	= mean(d - meanD.sig, na.rm=TRUE),
	# RMSE.D.sig	= sqrt(mean((d - meanD.sig)^2, na.rm=TRUE)),
	
	# # coverage etc.
	# coverage.D	= sum(meanD > lb & meanD < ub)/sum(!is.na(lb)),
	# consisZero.D = sum(0 > lb & 0 < ub)/n(),
	# n.simulations = n()
)

print(summ, n=50)
#print(summ, n=nrow(summ))


# summ contains the full summary of the simulations. This object can then be used to build tables, plots, etc.
library(rio)
export(summ, file="summ.csv")
save(summ, file="summ.RData")
#load("summ.RData")