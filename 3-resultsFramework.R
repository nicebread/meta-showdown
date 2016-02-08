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

# final data set in long format:
save(res.final, file="res.final.RData")



# Show conditions
tab <- res.final %>% group_by(k, delta, qrpEnv, selProp, tau) %>% summarise(n.MA=length(unique(id)))
print(tab, n=50)

## reduce to relevant variables, drop unused factor levels
res2 <- res.final %>% filter(variable != "tauEst", method!="FAT") %>% droplevels()

# reshape long format to wide format
res.wide <- dcast(res2, id + condition + k + delta + qrpEnv + selProp + tau + method ~ variable, value.var="value")
head(res.wide)

# define some meaningful labels for the plots
res.wide$delta.label <- factor(res.wide$delta, levels=unique(res.wide$delta), labels=paste0("delta = ", unique(res.wide$delta)))
res.wide$k.label <- factor(res.wide$k, levels=sort(unique(res.wide$k)), labels=paste0("k = ", sort(unique(res.wide$k))))
res.wide$qrp.label <- factor(res.wide$qrpEnv, levels=unique(res.wide$qrpEnv), labels=paste0("QRP = ", unique(res.wide$qrpEnv)))

# save compressed version
tab <- res.wide %>% group_by(k, delta, qrpEnv, selProp, tau) %>% summarise(n.MA=length(unique(id)))
print(tab, n=50)

save(res.wide, file="res.wide.RData", compress="gzip")
#load(file="res.wide.RData")



# Compute summary measures across replications
summ <- res.wide %>% filter(method!="FAT") %>% group_by(condition, k, k.label, delta, delta.label, qrpEnv, qrp.label, selProp, tau, method) %>% summarise(
	meanEst		= mean(d, na.rm=TRUE),
	meanD		= mean(D.mean, na.rm=TRUE),
	meanD.sig	= mean(D.mean.sig, na.rm=TRUE),
	
	# relative to true population mean ("delta")
	ME 			= mean(d - delta, na.rm=TRUE),
	MSE			= mean((d - delta)^2, na.rm=TRUE),
	coverage 	= sum(delta > lb & delta < ub)/sum(!is.na(lb)),
	consisZero  = sum(0 > lb & 0 < ub)/n(),
	
	# relative to mean of entered ES ("D")
	ME.D		= mean(d - meanD, na.rm=TRUE),
	MSE.D		= mean((d - meanD)^2, na.rm=TRUE),
	
	# relative to mean of significant entered ES ("D.sig")
	ME.D.sig	= mean(d - meanD.sig, na.rm=TRUE),
	MSE.D.sig	= mean(d - meanD.sig, na.rm=TRUE)^2,
	
	# coverage etc.
	coverage.D	= sum(meanD > lb & meanD < ub)/sum(!is.na(lb)),
	consisZero.D = sum(0 > lb & 0 < ub)/n(),
	n.simulations = n()
)


print(summ, n=nrow(summ))


# summ contains the full summary of the simulations. This object can then be used to build tables, plots, etc.
library(rio)
export(summ, file="summ.csv")
save(summ, file="summ.RData")
#load("summ.RData")