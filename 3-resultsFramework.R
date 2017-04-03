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

res.final <- res.final %>% droplevels()

# reshape long format to wide format
res.wide <- dcast(res.final, id + condition + k + delta + qrpEnv + selProp + tau + method ~ term + variable, value.var="value")
head(res.wide, 16)

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


# save a filtered version
## remove p-curve and p-uniform with < 4 studies, drop unused factor levels
res.wide.red <- res.wide %>% 
  filter(!method %in% c("PET.rma", "PEESE.rma", "PETPEESE.rma", "pcurve.hack")) %>% 
  filter(!method %in% c("pcurve.evidence", "pcurve.hack", "pcurve.lack", "pcurve", "puniform") | 
         (method %in% c("pcurve.evidence", "pcurve.hack", "pcurve.lack", "pcurve", "puniform") & kSig_estimate >= 4))
          
save(res.wide.red, file="res.wide.red.RData", compress="gzip")
#load(file="res.wide.red.RData")


# ---------------------------------------------------------------------
#  Compute summary measures across replications

summ <- res.wide.red %>% group_by(condition, k, k.label, delta, delta.label, qrpEnv, qrp.label, selProp, selProp.label, tau, tau.label, method) %>% 
	dplyr::summarise(
		meanEst		= mean(b0_estimate, na.rm=TRUE),
		ME 			= mean(b0_estimate - delta, na.rm=TRUE),
		RMSE		= sqrt(mean((b0_estimate - delta)^2, na.rm=TRUE)),
		MAD			= mean(abs(b0_estimate - delta), na.rm=TRUE), # mean absolute deviation
		perc2.5		= quantile(b0_estimate, probs=.025, na.rm=TRUE),
		perc97.5	= quantile(b0_estimate, probs=.975, na.rm=TRUE),
		coverage 	= sum(delta > b0_conf.low & delta < b0_conf.high)/sum(!is.na(b0_conf.high)),
		consisZero  = sum(0 > b0_conf.low & 0 < b0_conf.high)/n()
	)

print(summ, n=50)



# summ contains the full summary of the simulations. This object can then be used to build tables, plots, etc.
library(rio)
export(summ, file="summ.csv")
save(summ, file="summ.RData")
#load("summ.RData")