## ======================================================================
## This file takes the simulated data sets, which are stored in separate
## files in folder /simParts, and runs all meta-analytic techniques on them.
## Then it saves the analyses in one file in the /analysisData folder.
## ======================================================================

# run this file:
# source("analysisFramework.R", echo=TRUE)

# load all functions and packages
source("start.R")

library(doParallel)
# detectCores()
registerDoMC(2)

(ncores <- getDoParWorkers())	# number of parallel processes

# simDatFiles stores the names of all simulated data files in the folder "simParts"
simDatFiles <- list.files("simParts", pattern=".*\\.RData", full.names=TRUE)

res.final <- data.frame()

# loop through all simParts files
for (f in simDatFiles) {

	load(f)	# the simulation data frame always is called "sim"
	n.MA <- length(unique(sim$id))		# overall number of MAs
	print(paste0(Sys.time(), ": Analyzing ", n.MA, " unique MAs from file ", f))

	## slice up the file into ncores pieces
	if (length(unique(sim$id)) %% ncores != 0) {
		warning(paste0("Number of MAs (", length(unique(sim$id)), ") not dividable by number of cores (", ncores, ")"))
	}

	# build translation table: which unique sim ID goes into which core?
	translation <- rep(1:ncores, each=length(unique(sim$id))/ncores)
	names(translation) <- unique(sim$id)
	sim$core <- translation[as.character(sim$id)]

	# Now, loop through all meta-analyses, each core gets its share of studies
	res <- foreach(batch=1:ncores, .combine=rbind) %dopar% {    

		#source("start.R")
		counter <- 1
		reslist <- list()	# each MA is stored as 1 list element, which is later combined to a single data frame
	
		sim.piece <- sim[sim$core==batch, ]
		n.MA.piece <- length(unique(sim.piece$id))
		for (i in 1:n.MA.piece) {
			print(paste0(Sys.time(), ", batch=", batch, ": Computing ", i, "/", n.MA.piece))

			# select rows from one single MA
			MAdat <- sim.piece[sim.piece$id == unique(sim.piece$id)[i], ]
			rownames(MAdat) <- NULL
	
			# analyze with all MA techniques
			re.est <- reEst(MAdat$d, MAdat$v, long=TRUE)
		    lm.est <- lmVarEst(MAdat$d, MAdat$v, long=TRUE)
			#pcurve.est <- pcurveEst(t=MAdat$t, df=MAdat$N-2, B=10, progress=FALSE, long=TRUE, CI=FALSE)	# TODO: increase B to 1000
	
			# combine analysis results
			#res0 <- rbind(re.est, lm.est, pcurve.est)
			res0 <- rbind(re.est, lm.est)
	
			# collect results
			res1 <- cbind(
		
				# save settings of condition to results:
				MAdat[rep(1, nrow(res0)), c("id", "condition", "k", "delta", "qrpEnv", "selProp", "tau", "D", "kFD", "sel", "qrp")],
		
				# save analysis results:
				res0
				## TODO: add all other MA techniques
			)
			reslist[[counter]] <- res1
			counter <- counter+1
		}

		res2 <- bind_rows(reslist)
		return(res2)
	} # of dopar
	
	res.final <- bind_rows(res.final, res)
} # of "f in simDatFiles"


save(res.final, file="analysisData/analysis504.RData")
print(paste0(Sys.time(), ": Finished analyzing ", length(unique(res.final$id)), " unique MAs."))
