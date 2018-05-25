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
