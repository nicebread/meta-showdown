# run this file:
# source("analysisFramework.R", echo=TRUE)

# load all functions and packages
source("start.R")
library(progress)

# # register CPU cores for parallel processing
# doParallel does not work so well on my machine; it prefers doMC ...
# library(doParallel)
# detectCores()
#
# # Create cluster with desired number of cores
# cl <- makeCluster(20)
# registerDoParallel(cl) 	# Register cluster

registerDoMC(20)
getDoParWorkers()

(ncores <- getDoParWorkers())	# number of parallel processes

# load simulated data
# ---------------------------------------------------------------------
# Our own simulated data set
#load("simData/simDataTest.RData")

# ---------------------------------------------------------------------
# Alternatively: Stanley's data set
load("simData/mixDataGen.RData")

# ---------------------------------------------------------------------
# This applies to both data sets ...
sim <- data.frame(sim)
# for testing purposes: reduce data set
#sim <- sim[sim$replication <= 1, ]

# show summary table of conditions
condition.tab <- sim %>% group_by(HET, kPer, EFF, BIAS) %>% summarise(n=n())
print(condition.tab, n=50)


n.MA <- length(unique(sim$unique))		# overall number of MAs
print(paste0(Sys.time(), ": Analyzing ", n.MA, " unique MAs..."))

## slice up the file into ncores pieces
if (length(unique(sim$unique)) %% ncores != 0) {
	warning(paste0("Number of MAs (", length(unique(sim$unique)), ") not dividable by number of cores (", ncores, ")"))
}

# build translation table: which unique sim ID goes into which core?
translation <- rep(1:ncores, each=length(unique(sim$unique))/ncores)
names(translation) <- unique(sim$unique)
sim$core <- translation[as.character(sim$unique)]

# Now, loop through all meta-analyses
res <- foreach(batch=1:ncores, .combine=rbind) %dopar% {    

	#source("start.R")
	counter <- 1
	reslist <- list()	# each MA is stored as 1 list element, which is later combined to a single data frame
	
	sim.piece <- sim[sim$core==batch, ]
	n.MA.piece <- length(unique(sim.piece$unique))
	for (i in 1:n.MA.piece) {
		print(paste0(Sys.time(), ", batch=", batch, ": Computing ", i, "/", n.MA.piece))

		# select rows from one single MA
		MAdat <- sim.piece[sim.piece$unique == unique(sim.piece$unique)[i], ]
		rownames(MAdat) <- NULL
	
		# analyze with all MA techniques
		re.est <- reEst(MAdat$d, MAdat$v, long=TRUE)
	    lm.est <- lmVarEst(MAdat$d, MAdat$v, long=TRUE)
		pcurve.est <- pcurveEst(t=MAdat$t, df=MAdat$N-2, B=10, progress=FALSE, long=TRUE, CI=FALSE)	# TODO: increase B to 1000
	
		# combine analysis results
		res0 <- rbind(re.est, lm.est, pcurve.est)
		#res0 <- rbind(re.est, lm.est)
	
		# collect results
		res1 <- cbind(
		
			# save settings of condition to results:
			MAdat[rep(1, nrow(res0)), 1:8],
		
			# save analysis results:
			res0
			## TODO: add all other MA techniques
		)
		reslist[[counter]] <- res1
		counter <- counter+1
	}

	res2 <- bind_rows(reslist)
	return(res2)
}

save(res, file="analysisData/analysismixDataGen.RData")
print(paste0(Sys.time(), ": Finished analyzing ", n.MA, " unique MAs."))

# sanity check 1:
if (!all.equal(unique(res$unique), unique(sim$unique))) warning("ERROR in analysisFramework.R")
	
# sanity check 2:
(tab1 <- sim %>% group_by(HET, kPer, EFF, BIAS) %>% summarise(n=n()/20))
(tab2 <- res %>% group_by(HET, kPer, EFF, BIAS) %>% summarise(n=n()/17))

if (!all.equal(tab1, tab2)) warning("ERROR 1 in analysisFramework.R")
if (!all.equal(unique(res$unique), unique(sim$unique))) warning("ERROR 2 in analysisFramework.R")