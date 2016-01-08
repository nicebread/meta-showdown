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
	res$id <- paste0(f, "_", res$id)
	res_list[[f]] <- res
}
res.final <- bind_rows(res_list)
str(res.final)

# Show conditions
tab <- res.final %>% group_by(k, delta, qrpEnv, selProp, tau) %>% summarise(n.MA=length(unique(id)))
print(tab, n=50)

## reduce to relevant variables, drop unused factor levels
res2 <- res.final %>% filter(variable != "tauEst", method!="FAT") %>% droplevels()

# reshape long format to wide format
res.wide <- dcast(res2, id + condition + k + delta + qrpEnv + selProp + tau + method ~ variable, value.var="value")
head(res.wide)

# define some meaningful labels for the plots
res.wide$delta.label <- factor(res.wide$delta, labels=paste0("delta = ", unique(res.wide$delta)))
res.wide$k.label <- factor(res.wide$k, labels=paste0("k = ", unique(res.wide$k)))
res.wide$qrp.label <- factor(res.wide$qrpEnv, labels=paste0("QRP = ", unique(res.wide$qrpEnv)))


# Compute summary measures across replications
summ <- res.wide %>% filter(method!="FAT") %>% group_by(condition, k, k.label, delta, delta.label, qrpEnv, qrp.label, selProp, tau, method) %>% summarise(
	meanEst		= round(mean(d, na.rm=TRUE), 3),
	meanD		= round(mean(D.mean, na.rm=TRUE), 3),
	meanD.sig	= round(mean(D.mean.sig, na.rm=TRUE), 3),
	
	# relative to true population mean ("delta")
	ME 			= round(mean(d - delta, na.rm=TRUE), 3),
	MSE			= round(mean(d - delta, na.rm=TRUE)^2, 3),
	coverage 	= round(sum(delta > lb & delta < ub)/sum(!is.na(lb)), 3),
	consisZero      = round(sum(0 > lb & 0 < ub)/n(), 3),
	
	# relative to mean of entered ES ("D")
	ME.D		= round(mean(d - meanD, na.rm=TRUE), 3),
	MSE.D		= round(mean(d - meanD, na.rm=TRUE)^2, 3),
	
	# relative to mean of significant entered ES ("D.sig")
	ME.D.sig	= round(mean(d - meanD.sig, na.rm=TRUE), 3),
	MSE.D.sig	= round(mean(d - meanD.sig, na.rm=TRUE)^2, 3),
	
	# coverage etc.
	coverage.D	= round(sum(meanD > lb & meanD < ub)/sum(!is.na(lb)), 3),
	consisZero.D     = round(sum(0 > lb & 0 < ub)/n(), 3),
	n.simulations = n()
)


print(summ, n=nrow(summ))


# summ contains the full summary of the simulations. This object can then be used to build tables, plots, etc.
library(rio)
export(summ, file="summ5000.csv")
save(summ, file="summ5000.RData")
load("summ5000.RData")
# ---------------------------------------------------------------------
#  visualize
library(ggplot2)
ggplot(summ %>% filter(tau==0, selProp==0.6), aes(x=k.label, y=meanEst, color=method)) + geom_point() + facet_grid(qrp.label~delta.label) + geom_hline(aes(yintercept=delta)) + theme_bw()

# ---------------------------------------------------------------------
#  show violin plots in partly loop style
library(gtools)

# order two variables into one loop
res.wide <- res.wide %>% mutate(k_method=paste0(k, "_", method))

# order loop factor alphabetically
res.wide$k_method <- factor(res.wide$k_method, levels=mixedsort(unique(res.wide$k_method)))

# select relevant data
sel <- res.wide %>% filter(method!="FAT", tau==0, selProp==1)

ggplot(sel, aes(x=k_method, y=d, color=method, group=k_method)) + geom_violin() + facet_grid(qrp.label~delta.label) + geom_hline(aes(yintercept=delta)) + theme_bw()+ theme(axis.text.x = element_text(angle = 90, size=3))

# without loop
ggplot(sel, aes(x=k.label, y=d, color=method, group=k_method)) + geom_violin(position=position_dodge()) + facet_grid(qrp.label~delta.label) + geom_hline(aes(yintercept=delta)) + theme_bw()


# control condition with tau=0 and selProp=0
ggplot(summ %>% filter(tau==0, selProp==0), aes(x=k, y=meanEst, color=method)) + geom_point() + facet_grid(qrp.label~delta.label) + geom_hline(aes(yintercept=delta)) + theme_bw()



# ---------------------------------------------------------------------
# pcurve follow up

res.wide %>% filter(method=="pcurve") %>% group_by(k, delta, qrpEnv, selProp, tau) %>% summarise(
	nStudies=round(mean(sig.studies, na.rm=TRUE), 2)
	)
	
head(res.wide %>% filter(method=="pcurve"))	