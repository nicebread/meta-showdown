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
	load(f)	# the simulation data frame always is called "res"
	res_list[[f]] <- res
}
res.final <- bind_rows(res_list)


# Show conditions
tab <- res.final %>% group_by(k, delta, qrpEnv, selProp, tau) %>% summarise(n.MA=length(unique(id)))
print(tab, n=50)

## reduce to relevant variables, drop unused factor levels
res2 <- res.final %>% filter(variable != "tauEst", method!="FAT") %>% droplevels()

# reshape long format to wide format
res.wide <- dcast(res2, id + k + delta + qrpEnv + selProp + tau + method ~ variable, value.var="value")
head(res.wide)

# Compute summary measures across replications
summ <- res.wide %>% filter(method!="FAT") %>% group_by(k, delta, qrpEnv, selProp, tau, method) %>% summarise(
	meanEst		= round(mean(d, na.rm=TRUE), 3),
	ME 			= round(mean(d - delta, na.rm=TRUE), 3),
	MSE			= round(mean(d - delta, na.rm=TRUE)^2, 3),
	coverage 	= round(sum(delta > lb & delta < ub)/sum(!is.na(lb)), 3),
	consisZero      = round(sum(0 > lb & 0 < ub)/n(), 3),
	n.simulations = n()
)

# define some meaningful labels for the plots
summ$delta.label <- factor(summ$delta, labels=paste0("delta = ", unique(summ$delta)))
summ$k.label <- factor(summ$k, labels=paste0("k = ", unique(summ$k)))
summ$qrp.label <- factor(summ$qrpEnv, labels=paste0("QRP-Environment = ", unique(summ$qrpEnv)))

print(summ, n=nrow(summ))

# summ contains the full summary of the simulations. This object can then be used to build tables, plots, etc.



# ---------------------------------------------------------------------
#  visualize
library(ggplot2)
ggplot(summ %>% filter(tau==0.25, selProp==0.6), aes(x=k, y=meanEst, color=method)) + geom_point() + facet_grid(qrp.label~delta.label) + geom_hline(aes(yintercept=delta)) + theme_bw()

# control condition with tau=0 and selProp=0
ggplot(summ %>% filter(tau==0, selProp==0), aes(x=k, y=meanEst, color=method)) + geom_point() + facet_grid(qrp.label~delta.label) + geom_hline(aes(yintercept=delta)) + theme_bw()


# ---------------------------------------------------------------------
# pcurve follow up

res.wide %>% filter(method=="pcurve") %>% group_by(k, delta, qrpEnv, selProp, tau) %>% summarise(
	nStudies=round(mean(sig.studies, na.rm=TRUE))
	)