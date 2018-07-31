## ======================================================================
## This file takes the simulated data sets, which are stored in separate
## files in folder /simParts, and extracts descriptives
## ======================================================================

# run this file:
# source("2b-Descriptives of simulations.R", echo=TRUE)

# load all functions and packages
source("0-start.R")

library(doParallel)
# detectCores()
registerDoParallel(cores=1)

(ncores <- getDoParWorkers())	# number of parallel processes

# simDatFiles stores the names of all simulated data files in the folder "simParts"
simDatFiles <- list.files("simPartsRev2", pattern=".*\\.RData", full.names=TRUE)

library(gtools)
simDatFiles <- mixedsort(simDatFiles)

## ======================================================================
## Percentage of significant studies in literature in each condition
## ======================================================================

res <- data.frame()

# loop through all simParts files
for (f in simDatFiles) {

	load(f)	# the simulation data frame always is called "sim"
	
	n.MA <- length(unique(sim$id))		# overall number of MAs
	print(paste0(Sys.time(), ": Analyzing ", n.MA, " unique MAs from file ", f))
	
	desc <- sim %>% group_by(condition, k, delta, qrpEnv, censor, tau) %>% summarise(
		perc.sig = sum(p <= .05) / n(),
		n1.q25 = quantile(n1, prob=.25),
		n1.q50 = quantile(n1, prob=.50),
		n1.q75 = quantile(n1, prob=.75),
	) %>% ungroup() %>% as.data.frame()
	
	res <- rbind(res, desc)

} # of "f in simDatFiles"

print(res)


## ======================================================================
## Which QRPs "survive" publication bias?
## We simulate research environments with certain proportions of pure/moderate/aggressive QRP strategies.
## But this is done *before* publication bias takes place. What are the proportions *after* publication bias?

## Encoded in variable qrp of the simDat files: 0 = none, 1 = moderate, 2 = aggressive
## ======================================================================

res <- data.frame()

# loop through all simParts files
for (f in simDatFiles) {
	load(f)	# the simulation data frame always is called "sim"
	
	n.MA <- length(unique(sim$id))		# overall number of MAs
	print(paste0(Sys.time(), ": Analyzing ", n.MA, " unique MAs from file ", f))
	
	desc <- sim %>% group_by(condition, k, delta, qrpEnv, censor, tau) %>% summarise(
		qrp.none = sum(qrp==0)/n(),
		qrp.moderate = sum(qrp==1)/n(),
		qrp.aggressive = sum(qrp==2)/n()
	) %>% ungroup() %>% as.data.frame()
	
	res <- rbind(res, desc)
} # of "f in simDatFiles"

res[, 7:9] <- round(res[, 7:9], 2)

# proportions are independent of k and delta; influence of tau is also minor
# --> aggregate across these factors; show mean of each proportion and the range
res.aggregate <- res %>% group_by(qrpEnv, censor) %>% summarise(
	qrp.none.range = paste0(f2(mean(qrp.none)), " (", f2(min(qrp.none)), " - ", f2(max(qrp.none)), ")"),
	qrp.moderate.range = paste0(f2(mean(qrp.moderate)), " (", f2(min(qrp.moderate)), " - ", f2(max(qrp.moderate)), ")"),
	qrp.aggressive.range = paste0(f2(mean(qrp.aggressive)), " (", f2(min(qrp.aggressive)), " - ", f2(max(qrp.aggressive)), ")")
)

print(res.aggregate, n=9)



## ======================================================================
## What is the *directionally consistent* false positive rate in each QRP style?
## (only evaluated in conditions without publication bias)
## I.e., here we only count successful p-hacking
## ======================================================================

res <- data.frame()

# loop through all simParts files
for (f in simDatFiles) {
	load(f)	# the simulation data frame always is called "sim"

	## (only evaluate in conditions without publication and without heterogeneity)
	if (sim$delta[1] != 0 | sim$censor[1] != "none" | sim$tau[1] != 0) next;
	
	n.MA <- length(unique(sim$id))		# overall number of MAs
	print(paste0(Sys.time(), ": Analyzing ", n.MA, " unique MAs from file ", f))

	desc <- sim %>% group_by(condition, k, delta, qrpEnv, censor, tau, qrp) %>% 
	summarise(
		FPR = sum(p<.05 & d > 0)/n(),
		n = n()
	) %>% ungroup() %>% as.data.frame()
	
	res <- rbind(res, desc)
} # of "f in simDatFiles"


ggplot(res, aes(y=FPR, x=factor(qrp), color=factor(k), shape=qrpEnv)) + geom_point()

# FPR is independent of k and qrpEnv
# --> aggregate across these factors; show mean of each proportion and the range
res.aggregate.qrp <- res %>% group_by(qrp) %>% summarise(
	FPR = paste0(round(mean(FPR, weight=n)*100), "%")
)

print(res.aggregate.qrp)

# # A tibble: 3 x 2
#     qrp FPR
#   <dbl> <chr>
# 1     0 2%
# 2     1 9%
# 3     2 27%


## ======================================================================
## What is the false positive rate in each QRP Environment?
## (only evaluated in conditions without publication bias)
## ======================================================================

res <- data.frame()

# loop through all simParts files
for (f in simDatFiles) {
	load(f)	# the simulation data frame always is called "sim"

	## (only evaluate in conditions without publication and without heterogeneity)
	if (sim$delta[1] != 0 | sim$censor[1] != "none" | sim$tau[1] != 0) next;
	
	n.MA <- length(unique(sim$id))		# overall number of MAs
	print(paste0(Sys.time(), ": Analyzing ", n.MA, " unique MAs from file ", f))

	desc <- sim %>% group_by(condition, k, delta, qrpEnv, censor, tau) %>% 
	summarise(
		FPR = sum(p<.05 & d > 0)/n(),
		n = n()
	) %>% ungroup() %>% as.data.frame()
	
	res <- rbind(res, desc)
} # of "f in simDatFiles"


ggplot(res, aes(y=FPR, x=factor(qrpEnv), color=factor(k))) + geom_point()

# FPR is independent of k
# --> aggregate across these factors; show mean of each proportion and the range
res.aggregate.qrpEnv <- res %>% group_by(qrpEnv) %>% summarise(
	FPR = paste0(round(mean(FPR, weight=n)*100), "%")
)

print(res.aggregate.qrpEnv)

# # A tibble: 3 x 2
#   qrpEnv FPR
#   <fct>  <chr>
# 1 none   5%
# 2 med    13%
# 3 high   20%
